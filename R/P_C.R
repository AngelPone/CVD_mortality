rm(list = ls())
library(haven)
library(dplyr)
library(ggplot2)
source("utils.R")

output <- read_dtC()

library(survey)
mydesign <- svydesign(ids = ~1, weights = ~sampleweight, data = output)
mymean <- svyby(~overweight, ~year+gender+age+education, mydesign, svymean)

output %>% group_by(year, gender, age) %>% 
  tally() %>%
  tidyr::pivot_wider(names_from = "age", values_from = "n") %>%
  write.csv("report_plots/samples_sizes.csv", row.names = FALSE)


prob_output <- select(mymean, year, gender, age, education, overweight) %>%
  cbind(confint(mymean)) %>%
  `row.names<-`(NULL) %>%
  tidyr::nest(dt = -c(gender, year, education)) %>%
  mutate_at("dt", purrr::map, function(x){
    x <- x %>% arrange(age)
    age <- 1:length(unique(x$age))
    x$P_loess <- fitted(loess(x$overweight ~ age, span = 1))
    x$lower_loess <- fitted(loess(x$`2.5 %` ~ age, span = 1))
    x$upper_loess <- fitted(loess(x$`97.5 %` ~ age, span = 1))
    x
  }) %>%
  tidyr::unnest(cols = dt)

PO_output <- svyby(~overweight, ~year+gender+age, mydesign, svymean) %>%
  select(year, gender, age, overweight) %>%
  `row.names<-`(NULL)

prob_output <- prob_output %>% filter(education == "low") %>%
  mutate(education = "middle") %>%
  rbind(prob_output)

transform_year <- function(x){
  g <- NULL
  for (year_str in unique(x$year)) {
    years <- strsplit(year_str, "-")[[1]] %>% as.integer()
    for (y in years[1]:years[2]) {
      g <- x %>% filter(year == year_str) %>%
        mutate(year = y) %>%
        rbind(g)
    }
  }
  g
}


prob_output <- transform_year(prob_output) %>% arrange(year)

f <- NULL
for (gender in c("F", "M")) {
  df <- prob_output %>% filter(gender==.env$gender) %>%
    select(-gender) %>%
    filter(year <= 2019)
  
  path <- sprintf("report_plots/csv/%s/", ifelse(gender=="F", "Female", "Male"))
  
  for (education in unique(df$education)) {
    file1 <- sprintf("PC_%s_%s.csv", education, "overweight")
    file2 <- sprintf("PC_%s_%s.csv", education, "normalweight")
    df1 <- df %>% filter(education == .env$education) %>% select(-education) %>%
      tidyr::pivot_wider(id_cols = year, names_from = age, values_from = P_loess)
    df1[is.na(df1)] <- mean(as.matrix(df1 %>% select(-year)), na.rm = TRUE)
    
    write.csv(df1, paste0(path, file1), row.names = FALSE)
    write.csv(df1 %>% mutate_at(vars(-c("year")), function(x){1-x}), paste0(path, file2), row.names = FALSE)
  }
  f <- rbind(f, df %>% mutate(gender = .env$gender))
}

f %>% 
  filter(year %in% c(2003, 2015)) %>%
  filter(education != "low") %>%
  mutate(year = ifelse(year == 2003, "2003-2010", "2011-2019")) %>%
  mutate(education = ifelse(education %in% c("middle", "low"), "Low and middle education", "High education")) %>%
  mutate(gender = ifelse(gender == "F", "Female", "Male")) %>%
  ggplot(aes(x = age, y=P_loess, group = interaction(year, gender), color = gender)) + 
  geom_line(mapping = aes(linetype = year), linewidth = 1) +
  geom_ribbon(aes(ymin = lower_loess, ymax = upper_loess), alpha = 0.1, linetype="dotted") +
  facet_wrap(~ factor(education, levels = c("Low and middle education", "High education")), nrow = 1) +
  scale_y_log10() + 
  ylab("") +
  labs(linetype="Year", color="Gender") +
  theme_gray(base_size = 12) +
  xlab("Age")

ggsave(sprintf("report_plots/PC_interval.png"), width = 9, height = 7)

# PO_ouptut <- PO_ouptut %>% group_by(year, gender) %>% nest() %>%
#   mutate_at("data", purrr::map, function(x){
#     age <- 1:8
#     x <- arrange(x, age)
#     x$P <- fitted(loess(x$P ~ age, span = 1))
#     x
#   }) %>%
#   unnest(cols = "data")
# 
# PO_ouptut %>%
#   mutate(gender = ifelse(gender == "F", "Female", "Male")) %>%
#   mutate(year = ifelse(year=="2003-2010", "2003-2010", "2011-2019")) %>%
#   ggplot(aes(x=age, y=P, group = interaction(gender, year), color=year)) +
#   geom_line(aes(linetype=gender)) +
#   ggtitle("P(O)")
# 
# PO_ouptut %>% tidyr::pivot_wider(names_from = "age", values_from = "P") %>%
#   mutate(gender = ifelse(gender == "F", "Female", "Male")) %>%
#   mutate(year = ifelse(year=="2003-2010", "2003-2010", "2011-2019")) %>%
#   arrange(year) %>%
#   write.csv("report_plots/PO.csv", row.names = FALSE)
# 
# ggsave("report_plots/PO.png", width = 7, height = 7)




# all age
dt <- output %>% filter(education_type == 3, age_type == 5) %>%
  select(-education_type, -age_type, -id)
dt %>% group_by(year, gender, education) %>%
  summarise(P = sum(overweight * sampleweight) / sum(sampleweight)) %>%
  mutate(gender = ifelse(gender == "F", "Female", "Male")) %>%
  tidyr::pivot_wider(names_from = "gender", values_from = "P") %>%
  mutate(education = ifelse(education == "low", "low and middle", "high")) %>%
  write.csv("report_plots/allage/PC.csv")






