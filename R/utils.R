library(haven)
# age
age5 <- c("35-39", "40-44", "45-49", "50-54",
          "55-59", "60-64", "65-69", "70-74")

recode_age5 <- function(x){
  age_map <- split(rep(age5, each=5), paste0(35:74))
  unname(sapply(paste0(x), function(g) {age_map[[g]]}))
}


# education


educ3 <- function(x){
  idx89 <- startsWith(x, "educ89")
  educ_num <- sapply(strsplit(x, '_'), function(x){ x[2] })
  educ_89 <- split(rep(c("low", "middle", "high"), c(12, 4, 2)), 0:17)
  recode_89 <- sapply(educ_num[idx89], function(g) { educ_89[[g]] })
  
  educ_2003 <- split(rep(c("low", "middle", "high"), c(2,3,3)), 1:8)
  recode_2003 <- sapply(educ_num[!idx89], function(g) { educ_2003[[g]] })
  
  x[idx89] <- recode_89
  x[!idx89] <- recode_2003
  
  x
}

leecarter <- function(mort) {
  T = dim(mort)[1]
  ax = apply(mort,2,mean, na.rm=TRUE)
  matrix_sd = sweep(mort,2,ax)
  U = svd(matrix_sd)$u
  V = svd(matrix_sd)$v
  d = svd(matrix_sd)$d
  kapa = d[1]*U[,1]*sum(V[,1])
  bx <- t(V[,1]/sum(V[,1]))
  matrix_sd_fitted = as.matrix(kapa)%*%t(V[,1]/sum(V[,1]))
  matrix_fitted = sweep(matrix_sd_fitted,2,-ax)
  
  return(list(ax=ax, kapa=kapa, bx=bx, fitted=matrix_fitted))
}

rep_row <- function(x, n) {
  rep(x, n) %>% matrix(nrow = n, byrow = TRUE)
}

rep_col <- function(x, n) {
  rep(x, n) %>% matrix(nrow = n, byrow = TRUE) %>% t()
}


leecarter_f <- function(x){
  
  xx <- select(x, -year) %>% as.matrix() %>% unname()
  
  lc <- leecarter(log(xx))
  kapat <- lc$kapa
  
  
  # random walk with drift
  rw <- rwf(kapat, h=10, drift = TRUE)$mean
  rw_lower <- rwf(kapat, h=10, drift = TRUE)$lower[,"95%"]
  rw_upper <- rwf(kapat, h=10, drift = TRUE)$upper[,"95%"]
  
  rw <- (rep_row(lc$ax, 10) + rep_row(lc$bx, 10) * rep_col(rw, NCOL(xx))) %>%
    exp() %>%
    data.frame(P=.) %>%
    mutate(year = (2020:2029))
  
  rw_lower <- (rep_row(lc$ax, 10) + rep_row(lc$bx, 10) * rep_col(rw_lower, NCOL(xx))) %>%
    exp() %>%
    data.frame(P=.) %>%
    mutate(year = (2020:2029))
  
  rw_upper <- (rep_row(lc$ax, 10) + rep_row(lc$bx, 10) * rep_col(rw_upper, NCOL(xx))) %>%
    exp() %>%
    data.frame(P=.) %>%
    mutate(year = (2020:2029))
  

  list(point = rw, lower = rw_lower, upper = rw_upper)
}

read_population <- function(age_type = 5) {
  population <- read.delim("Data Collection/Bridged-Race Population Estimates 1990-2020.txt")
  
  # Population data
  population <- population %>%
    mutate(age = as.integer(Age.Code), gender = Gender.Code,
           population = as.integer(Population), 
           year = as.integer(Yearly.July.1st.Estimates.Code)) %>%
    select(year, age, gender, population) %>%
    tidyr::drop_na() %>%
    filter(age >=35, age <= 74)
  
  if (age_type == 5) {
    population <- population %>% mutate(age=recode_age5(age)) %>%
      group_by(year, gender, age) %>%
      summarise(population = sum(population), .groups = "drop")
  }
  return(population)
}

read_dtC <- function(age_type = 5, education_type = 3){
  year_dirs <- c("2003-2004", "2005-2006", "2007-2008", "2009-2010", "2011-2012",
                 "2013-2014", "2015-2016", "2017-2020")
  BMX_files <- c("BMX_C", "BMX_D", "BMX_E", "BMX_F", "BMX_G", "BMX_H", "BMX_I", "P_BMX")
  DEMO_files <- c("DEMO_C", "DEMO_D", "DEMO_E", "DEMO_F", "DEMO_G", "DEMO_H", "DEMO_I", "P_DEMO")
  
  output <- NULL
  for (year in seq_along(year_dirs)) {
    d <- read_xpt(sprintf("NHANES/%s/%s.XPT", year_dirs[year], BMX_files[year]))
    d2 <- read_xpt(sprintf("NHANES/%s/%s.XPT", year_dirs[year], DEMO_files[year]))
    if (year == 8) {
      d2 <- d2 %>% rename(WTMEC2YR = WTMECPRP)
    }
    output <- d2 %>% left_join(d, by="SEQN") %>%
      select(id=SEQN, gender=RIAGENDR, age=RIDAGEYR, BMI=BMXBMI, education=DMDEDUC2, sampleweight = WTMEC2YR) %>%
      filter(age >= 35, age <= 74) %>%
      # tidyr::drop_na() %>%
      filter(sampleweight > 0) %>%
      mutate(year = year_dirs[year]) %>%
      rbind(output)
  }
  
  
  
  # calculate education and overweight status
  output <- output %>% 
    mutate(overweight=if_else(!((BMI < 30) | is.na(BMI)), 1, 0))
  
  # recode education
  recode_edu2_NHANES <- function(x) {
    low_idx <- (x %in% 1:2) | (x > 5)
    high_idx <- x %in% 3:5
    x[low_idx] <- "low"
    x[high_idx] <- "high"
    return(x)
  }
  
  
  recode_edu3_NHANES <- function(x) {
    low_idx <- (x %in% 1:2) 
    middle_idx <- x %in% 3:4 | (x > 5)
    high_idx <- x == 5
    
    x[low_idx] <- "low"
    x[middle_idx] <- "low"
    x[high_idx] <- "high"
    return(x)
  }
  
  # recode education
  output <-
    output %>%
    mutate(education = recode_edu2_NHANES(education), education_type=2) %>%
    rbind(output %>% 
            mutate(education = recode_edu3_NHANES(education), education_type = 3)) %>%
    mutate(gender = if_else(gender==1, "M", "F"))
  
  # recode age
  output <- output %>%
    mutate(age=recode_age5(age), age_type=5) %>%
    rbind(output %>% mutate(age_type=1))
  
  
  
  prob_output <- NULL
  PO_ouptut <- NULL
  
  # combine years use weighting strategy in 
  # https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/OverviewBrief.aspx?Cycle=2017-2020
  recode_year <- function(x, group_idx) {
    groups <- split(year_dirs, group_idx)
    group_name <- sapply(groups, function(x) {
      paste0(stringi::stri_split_fixed(x[1], "-")[[1]][1], "-",
             stringi::stri_split_fixed(x[length(x)], "-")[[1]][2])
    })
    for (i in seq_along(groups)) {
      x[x %in% groups[[i]]] = group_name[i]
    }
    x
  }
  
  
  
  # four
  output$sampleweight <- ifelse(output$year <= "2009-2010", output$sampleweight/4,
                                ifelse(output$year == "2017-2020", output$sampleweight * 3.2 / 9.2,
                                       output$sampleweight * 2/9.2))
  output$year <- recode_year(output$year, c(rep(1:2, each = 4)))
  output %>%
    filter(education_type == .env$education_type, age_type == .env$age_type) %>%
    select(-education_type, -age_type)
}

read_dtD <- function(age_type = 5, education_type = 3) {
  f <- NULL
  for (file in list.files("Data Collection")) {
    file_split <- strsplit(file, '[_.]')[[1]]
    gender <- toupper(file_split[1])
    age <- paste0(file_split[2], "-", file_split[3])
    d <- read.csv(paste0("Data Collection/", file)) %>%
      mutate(low = None + X1st.4th_grade + X5th.6th_grade + X7th.8th_grade + X9th_grade+ X10th_grade + X11th_grade,
             middle = High.school.graduate + Some.college.no.degree + Associate.degree..occupational + Associate.degree..academic,
             high =  Bachelor.s.degree + Master.s.degree + Professional.degree + Doctorate.degree) %>%
      select(year=Year, low, middle, high)
    d1 <- d %>% tidyr::pivot_longer(cols = c(low, middle, high), names_to = "education",
                                    values_to = "P_D") %>%
      mutate(age = age, gender = gender, education_type = 3)
    d2 <- d %>% mutate(high = middle+high) %>% select(year, low, high) %>%
      tidyr::pivot_longer(cols = c(low, high), names_to = "education", values_to = "P_D") %>%
      mutate(age = age, gender = gender, education_type = 2)
    
    f <- rbind(f, d1, d2)
  }
  
  f <- f %>% mutate(age_type = 5)
  # transform age5 to age
  f <- 
    lapply(unique(f$age), function(age5_str){
      ages <- strsplit(age5_str, "-")[[1]]
      do.call(rbind, lapply(as.integer(ages[1]): as.integer(ages[2]), function(x){
        f %>% filter(age == age5_str) %>% mutate(age = as.character(x), age_type=1)
      }))
    }) %>% do.call(rbind, .) %>%
    rbind(f)
  f %>% filter(education_type == .env$education_type,
               age_type == .env$age_type) %>%
    select(-education_type, -age_type)
}

educ2 <- function(x){
  idx89 <- startsWith(x, "educ89")
  educ_num <- sapply(strsplit(x, '_'), function(x){ x[2] })
  educ_89 <- split(rep(c("low", "high"), c(12, 6)), 0:17)
  recode_89 <- sapply(educ_num[idx89], function(g) { educ_89[[g]] })
  
  educ_2003 <- split(rep(c("low", "high"), c(2,6)), 1:8)
  recode_2003 <- sapply(educ_num[!idx89], function(g) { educ_2003[[g]] })
  
  x[idx89] <- recode_89
  x[!idx89] <- recode_2003
  
  x
}