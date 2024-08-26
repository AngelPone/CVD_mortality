rm(list = ls())
library(dplyr)
library(stringi)
library(ggplot2)
library(ggrepel)
library(scales)

educ_summary <- NULL

for (year in 2003:2019) {
  print(sprintf("Year: %d", year))
  mort <- read.csv(sprintf('/Volumes/SSD/Downloads/mort%d.csv', year))
  colnames(mort) <- replace(colnames(mort), colnames(mort) == "educ", "educ2003")
  colnames(mort) <- replace(colnames(mort), colnames(mort) == "educ1989", "educ89")
  
  # filter age
  mort <- mort %>% 
    rename(age5=ager27) %>%
    mutate(age = as.integer(stringi::stri_sub(age, 3, 4))) %>%
    filter(age5 <= 20, age5 >=13, age>=35, age<=74)
  
  # remove records whose education is unknown for both 2003 code and 89code
  if (year == 2021) {
    mort <- mort %>%
      mutate(educ2003 = if_else(educ2003 == 9, NA, educ2003))
    
    educ_summary <- rbind(educ_summary, c(year, 0, 0, sum(is.na(mort$educ2003)),
                                          !sum(is.na(mort$educ2003))))
    
    mort <- mort %>%  
      filter(!is.na(educ2003)) %>%
      select(educ2003, gender = sex, age5, age, ucod, starts_with("enicon")) %>%
      filter(!is.na(age), !is.na(age5), !is.na(gender), !is.na(ucod))
  } else {
    mort <- mort %>%
      mutate(educ89 = if_else(educ89 == 99, NA, educ89),
             educ2003 = if_else(educ2003 == 9, NA, educ2003))
    educ_summary <- rbind(educ_summary, c(year, sum(is.na(mort$educ89)),
                                          sum(!is.na(mort$educ89)),
                                          sum(is.na(mort$educ2003)),
                                          sum(!is.na(mort$educ2003))))
    mort <- mort %>%
      filter(!is.na(educ89) | !is.na(educ2003)) %>%
      select(educ2003, educ89, gender = sex, age5, age, ucod, starts_with("enicon")) %>%
      filter(!is.na(age), !is.na(age5), !is.na(gender), !is.na(ucod))
  }
  write.csv(mort, sprintf("NVSS/NVSS_%s.csv", year), row.names = FALSE)
}

educ_summary <- data.frame(educ_summary)
colnames(educ_summary) <- c("year", "89_na", "89_notna", "2003_na", "2003_notna")

# educ_summary %>% 
#   select(year, "89_notna", "2003_notna") %>%
#   tidyr::pivot_longer(cols = c("89_notna", "2003_notna"), names_to = "revision") %>%
#   ggplot(aes(x = year, y=value, fill=revision)) +
#     geom_bar(aes(fill=revision), stat = "identity", position = "fill") +
#     scale_y_continuous(labels = scales::percent) +
#     ylab("") + ggtitle("Percentage of 1989 revision and 2003 revision of education level") +
#     theme(legend.position = "bottom", text = element_text(size=25)) + labs(fill="") +
#     scale_fill_discrete(labels=c("2003 Revision", "1989 Revision")) +
#     xlab("Year")
# ggsave("NVSS/education_ratio.png", width = 15, height = 10)




tbl_all <- NULL
for (year in 2003:2019) {
  mort <- read.csv(sprintf('NVSS/NVSS_%d.csv', year))
  
  mort <- mort %>%
    mutate(diabetes = if_any(c(ucod, starts_with("enicon")), ~ (. >= "E10") & (. <= "E149")),
           hypertension = if_any(c(ucod, starts_with("enicon")), ~ (. >= "I10") & (. <= "I139")),
           lipidemias = if_any(c(ucod, starts_with("enicon")), ~ (. >= "E78") & (. <= "E789")),
           chronickd = if_any(c(ucod, starts_with("enicon")), ~ (. >= "N18") & (. <= "N189")),
           obesity = if_any(c(ucod, starts_with("enicon")), ~ (. >= "E65") & (. <= "E669")),
           uCVD = if_any(ucod, ~ (. >= "I00") & (. <= "I999")),
           uCVD46 = if_any(ucod, ~ startsWith(., "I46")),
           CVD = if_any(c(ucod, starts_with("enicon")), ~ ((. >= "I00") & (. <= "I459")) | ((. >= "I47") & (. <= "I999")))) %>%
    mutate(CVD = uCVD46 | CVD) %>%
    select(-uCVD46)
  
  mort <- mort %>% select(-c(ucod, starts_with("enicon")))
  
  # replace NA with FALSE
  na_fill_lst <- as.list(rep(FALSE, 7))
  names(na_fill_lst) <- colnames(mort)[6:12]
  mort <- mort %>% tidyr::replace_na(na_fill_lst)
  mort <- mort %>% 
    mutate(overweight = diabetes | hypertension | lipidemias | chronickd | obesity) %>%
    select(-c(diabetes, hypertension, lipidemias, chronickd, obesity))
  mort <- mort %>% mutate(education = if_else(!is.na(educ89), paste0("educ89_", educ89),
                                              paste0("educ2003_", educ2003))) %>%
    select(-educ2003, -educ89, -age5)
  
  # all death
  tbl_all <- mort %>% group_by(age, gender, education) %>%
    summarise(overweight = sum(overweight), n = n()) %>%
    mutate(year = .env$year) %>%
    rbind(tbl_all)
  
  tbl1 <- mort %>% filter(CVD) %>%
    group_by(age, gender, education) %>%
    summarise(overweight = sum(overweight), n=n()) %>%
    mutate(panel = 2) %>%
    mutate(year = year)
  
  tbl2 <- mort %>% filter(uCVD) %>%
    group_by(age, gender, education) %>%
    summarise(overweight = sum(overweight), n=n()) %>%
    mutate(panel = 1) %>%
    mutate(year = year) %>%
    rbind(tbl1)
  
  write.csv(tbl2, sprintf("NVSS/dt_%s.csv", year), row.names = FALSE)
}
write.csv(tbl_all, "NVSS/dt_all.csv", row.names = FALSE)





# Calculate P(O, E | CVD)

tbl1 <- NULL
for (year in 2003:2021) {
  tbl1 <- rbind(tbl1, read.csv(sprintf("NVSS/dt_%s.csv", year)))
}
tbl_all <- read.csv("NVSS/dt_all.csv")

source("utils.R")

# Calculate P(CVD) / P(death)
# ratio_ <- tbl1 %>% filter(panel == 1) %>% 
#   select(age, gender, n, year) %>%
#   mutate(age = recode_age5(age)) %>%
#   group_by(age, gender, year) %>%
#   summarise(n = sum(n), .groups = "drop") %>%
#   left_join(tbl_all %>% select(age, gender, n, year) %>%
#               mutate(age = recode_age5(age)) %>%
#               group_by(age, gender, year) %>%
#               summarise(n_t = sum(n), .groups = "drop"), by = c("age", "gender", "year")) %>%
#   mutate(ratio = n/n_t) %>%
#   filter(year <= 2019) %>%
#   mutate(gender = ifelse(gender == "F", "Female", "Male"))

# ggplot(ratio_) +
#   geom_line(mapping = aes(x = year, y=ratio, color = age)) +
#   facet_wrap(~ gender) +
#   ggtitle("P(CVD)/P(death)") +
#   xlab("Year") +
#   labs(color="Age") +
#   ylab("")
# 
# ggsave("report_plots/ratio.png", width = 7, height = 5)




## recode education
tbl1 <- tbl1 %>% mutate(education2 = educ2(education),
                        education3 = educ3(education)) %>%
  select(-education) %>%
  tidyr::pivot_longer(c(education2, education3), names_prefix = "education",
                      names_to = "education_type",
                      values_to = "education")

tbl1 <- tbl1 %>%
  mutate(age=recode_age5(age), age_type=5) %>%
  rbind(tbl1 %>% mutate(age_type=1))



# P(O,E|CVD)

library(tidyr)
for (gender in c("F", "M")) {
  dt1 <- tbl1 %>%
    filter(panel == 2, education_type == 3,
           gender == .env$gender, age_type==5) %>%
    select(-c(panel, education_type, gender, age_type)) %>%
    group_by(age, year, education) %>%
    summarise(overweight = sum(overweight), n = sum(n), .groups = "drop")
  dt1 <- dt1 %>% group_by(age, year) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    left_join(dt1 %>% mutate(normalweight = n-overweight)  %>% select(-n), by = c("age", "year"))

  dt1 <- dt1 %>% pivot_longer(cols = c("overweight", "normalweight"), names_to = "O")
  dt1$education <- paste0(stri_trans_totitle(dt1$education), " ", "education")
  dt1$O <- ifelse(dt1$O == "overweight", "Obesity", "Non-obesity")
  dt1$education <- factor(dt1$education, levels = paste0(c("Low", "Middle", "High"), " education"))
  dt1$O <- factor(dt1$O, levels = c("Non-obesity", "Obesity"))
  
  dt1 <- dt1 %>% rowwise() %>%
    mutate(P = value / n, 
           lower = binom.test(value, n, P)$conf.int[1],
           upper = binom.test(value, n, P)$conf.int[2]) %>%
    ungroup()
  dt1 %>% filter(year <= 2019) %>%
    ggplot(aes(x=year, y=P, color=age)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray", alpha = 0.3, linetype="dashed") +
    facet_wrap(~ O + education) +
    scale_y_log10() +
    xlab("Year") +
    ylab("") +
    labs(color="Age") +
    scale_x_continuous(breaks = c(2003, 2010, 2019)) +
    guides(color = guide_legend(reverse = TRUE))

  ggsave(sprintf("report_plots/PA_%s.png", gender), width = 9, height = 6)
}


# Bar plot for age groups
 # for (gender in c("F", "M")) {
 #  
 #   dt <- filter(tbl1, panel==.env$panel, education_type==.env$education_type,
 #                gender == .env$gender, age_type == 1) %>%
 #     select(-c(panel, education_type, gender, age_type)) %>%
 #     group_by(year, education) %>%
 #     summarise(n=sum(n), overweight = sum(overweight), .groups = "drop") %>%
 #     mutate(Obesity = overweight / n) %>%
 #     select(year, education, Obesity) %>%
 #     tidyr::pivot_longer(cols = "Obesity", values_to = "P", names_to = "O")
 #   dt <- dt %>% mutate(P = 1-P, O = "Non-obesity") %>%
 #     rbind(dt)
  
  # dt <- dt %>% filter(year %in% c(2003, 2010, 2019)) %>%
  #   mutate(year = as.character(year)) %>%
  #   mutate(education = paste0(stringi::stri_trans_totitle(education), " education")) %>%
  #   mutate(educaiton = factor(education, levels = c("Low education", "Middle education", "High education")))
  # 
  # ggplot(dt, mapping = aes(x = year, y = P, group=O, fill=O)) +
  #   geom_bar(stat="identity", position="stack", width = 0.5) +
  #   facet_wrap( ~ factor(education, levels = c("Low education", "Middle education", "High education"))) +
  #   xlab("Year") +
  #   labs(fill = "") +
  #   theme_gray(base_size = 16) +
  #   theme(legend.position = "bottom") +
  #   ggtitle(sprintf("P(O|E, CVD) (%s)", ifelse(gender == "F", "Female", "Male")))
  # 
  # ggsave(sprintf("report_plots/PA_bar_%s.png", gender), width = 12, height = 8)
  
  

#   ggsave(sprintf("report_plots/PA_bar_%s.png", gender), width = 6, height = 4)
# }


# 20140817 REVISION change bar plot to line plot
dt <- filter(tbl1, panel==2, education_type==3, age_type == 1) %>%
  select(-c(panel, education_type, age_type)) %>%
  group_by(year, education, gender) %>%
  summarise(n=sum(n), overweight = sum(overweight), .groups = "drop") %>%
  mutate(Obesity = overweight / n) %>%
  mutate(gender = case_match(gender, "M" ~ "Male", "F" ~ "Female")) %>%
  mutate(education = factor(paste(stringi::stri_trans_totitle(education), "education"), levels = c("Low education", "Middle education", "High education"))) %>%
  rename(Education = education)

dt <- dt %>% 
  rowwise() %>%
  mutate(lower = binom.test(overweight, n, Obesity)$conf.int[1],
                    upper = binom.test(overweight, n, Obesity)$conf.int[2]) %>%
  ungroup()

for (gender in c("Female", "Male")) {
  dt %>% filter(gender == .env$gender, year <= 2019) %>%
    ggplot(aes(x = year, group=Education, color=Education)) +
    geom_line(aes(y=Obesity)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), linetype="dashed", fill = "gray", alpha=0.3) +
    scale_x_continuous(breaks = c(2003, 2010, 2019)) +
    labs(linetype="", color="") +
    ylab("") +
    xlab("Year")
  ggsave(sprintf("report_plots/PA_line_%s.png", gender), width = 8/1.5, height = 6 / 1.5)
}


