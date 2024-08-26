rm(list = ls())
library(ggplot2)
library(dplyr)

f <- read_dtD()



for (education_type in c(3)) {
  for (age_type in c(5)) {
    for (gender in c("F", "M")) {
      path1 <- sprintf("report_plots/csv/%s", ifelse(gender=="F", "Female", "Male"))
      
      df <- f %>% filter(age_type==.env$age_type, gender==.env$gender,
                         education_type==.env$education_type, year <= 2019) %>%
        select(-c(age_type, gender, education_type))
      
      for (education in unique(df$education)) {
        dfc <- filter(df, education == .env$education) %>%
          tidyr::pivot_wider(id_cols = "year", names_from = "age", values_from = "P_D")
        write.csv(dfc, file = sprintf("%s/PD_%s.csv", path1, education), row.names = FALSE)
      }
    }
  }
}




# # Population
# population <- read.delim("Bridged-Race Population Estimates 1990-2020.txt")
# 
# # Population data
# population <- population %>%
#   mutate(age = as.integer(Age.Code), gender = Gender.Code,
#          population = as.integer(Population), 
#          year = as.integer(Yearly.July.1st.Estimates.Code)) %>%
#   select(year, age, gender, population) %>%
#   tidyr::drop_na() %>%
#   filter(age >=35, age <= 74)
# source("utils.R")
# population <- population %>%
#   mutate(age=recode_age5(age))
# 
# population <- population %>%
#   group_by(year, gender, age) %>% 
#   summarise(population=sum(population), .groups = "drop")
# 
# dt <- f %>% filter(education_type == 3, age_type == 5) %>%
#   select(-c(education_type, age_type)) 
# 
# dt <- dt %>% left_join(population, by = c("year", "gender", "age")) %>%
#   mutate(n = population * P_D) %>%
#   group_by(year, gender, education) %>%
#   summarise(n = sum(n)) %>%
#   left_join(population %>% group_by(year, gender) %>% summarise(population = sum(population)), by=c("year", "gender")) %>%
#   mutate(P = n/population, gender = ifelse(gender == "F", "Female", "Male"))
# 


# dt %>% 
#   mutate(education = factor(paste(stringi::stri_trans_totitle(education), "education"), levels = c("Low education", "Middle education", "High education"))) %>%
#   ggplot(aes(x=year, y=P, group = interaction(gender, education), color = education)) +
#   geom_line(aes(linetype = gender)) +
#   ggtitle("P(E)")
# 
# ggsave("report_plots/allage/PD.png", width = 8, height = 6)

# dt <- dt %>%
#   tidyr::pivot_wider(id_cols = c("year", "gender"), names_from = "education", values_from = "P")
# 
# for (gender in c("Female", "Male")) {
#   tmpdt <- dt %>% filter(gender == .env$gender) %>% select(-gender) %>%
#     data.frame()
#   tmpdt <- tmpdt[, c("year", "low", "middle", "high")]
#   write.csv(tmpdt, sprintf("report_plots/allage/PD_%s.csv", gender), row.names = FALSE)
# }







