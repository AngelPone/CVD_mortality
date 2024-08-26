rm(list = ls())
library(dplyr)
library(ggplot2)
source("utils.R")

population <- read_population(age_type = 1)

population <- population %>%
  mutate(age=recode_age5(age), age_type=5) %>%
  rbind(population %>% mutate(age_type=1))

population <- population %>%
  group_by(year, gender, age, age_type) %>% 
  summarise(population=sum(population), .groups = "drop")


tbl1 <- NULL
for (year in 2003:2019) {
  path <- sprintf("NVSS/dt_%d.csv", year)
  tbl1 <- rbind(read.csv(path), tbl1)
}

tbl1 <- tbl1 %>%
  mutate(age=recode_age5(age), age_type=5) %>%
  rbind(tbl1 %>% mutate(age_type=1)) %>%
  group_by(panel, gender, age_type, year, age) %>%
  summarise(n=sum(n), .groups = "drop")


tbl1 <- tbl1 %>% left_join(population, by=c("age", "age_type", "year", "gender"))
tbl1 <- tbl1 %>% mutate(P = n/population) %>%
  rowwise() %>%
  mutate(lower = binom.test(n, population, P)$conf.int[1],
         upper = binom.test(n, population, P)$conf.int[2]) %>%
  ungroup()



tbl1 %>% filter(panel == 2, age_type==5) %>%
  mutate(gender=case_match(gender, "F" ~ "Female", "M" ~ "Male")) %>%
  select(-panel, -age_type) %>%
  ggplot(aes(x = year, y = P, color=age)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill = "gray", alpha=0.3, linetype="dashed") +
  facet_wrap(~gender) +
  scale_y_log10() + 
  xlab("Year") +
  ylab("") +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(color = "Age") +
  scale_x_continuous(breaks = c(2003, 2010, 2019)) +
  theme(panel.spacing = unit(1, "lines"))
  
ggsave("report_plots/PB.png", width = 8/1.3, height = 5 / 1.2)


tbl1 %>% filter(panel == 2, age_type==5) %>%
  mutate(gender=case_match(gender, "F" ~ "Female", "M" ~ "Male")) %>%
  select(-panel, -age_type) %>%
  ggplot(aes(x = year, y = P, color=age)) +
  geom_line() +
  facet_wrap(~gender) +
  xlab("Year") +
  ylab("") +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(color = "Age") +
  scale_x_continuous(breaks = c(2003, 2010, 2019)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill = "gray", alpha=0.3, linetype="dashed") +
  theme(panel.spacing = unit(1, "lines"))
ggsave("report_plots/PB_nonlogscale.png", width = 8/1.3, height = 5 / 1.2)



table1 <- NULL
for (gender in c("Female", "Male")) {
  dt <- read.csv(sprintf("report_plots/csv/%s/PB.csv", gender))
  ann_rate <- apply(dt[,2:ncol(dt)], 2, function(x) {
    c((x[9] / x[1])^(1/8) - 1,
      (x[17] / x[9])^(1/8) - 1,
      (x[17] / x[1])^(1/16) - 1)
  })
  ann_rate <- as.data.frame(ann_rate)
  table1 <- rbind(table1, ann_rate %>% mutate(year = c("2003 - 2011", "2011 - 2019", "2003 - 2019"),
                                              gender = .env$gender))
}

table1 <- table1 %>% relocate(gender, year)
colnames(table1)[3:10] <- age5
write.csv(table1, "report_plots/table1.csv", row.names = FALSE)

tbl <- tbl1 %>% filter(age_type == 5, panel == 2) %>%
  select(-age_type, -panel) %>%
  filter(year %in% c(2003, 2011, 2019))

tbl <- tbl %>% rowwise() %>%
  mutate(lower = binom.test(n, population, P)$conf.int[1],
         upper = binom.test(n, population, P)$conf.int[2]) %>%
  ungroup()

interval_ratio <- function(p2, p1, lower2, lower1, upper2, upper1, year) {
  lower <- exp(log(p2/p1) - sqrt((log(upper1) - log(p1))^2 + (log(p2) - log(lower2))^2))
  upper <- exp(log(p2/p1) + sqrt((log(p1) - log(lower1))^2 + (log(upper2) - log(p2))^2))
  
  list(c(((p1 * lower)/p1)^(1/year) - 1, ((p1 * upper)/p1)^(1/year) - 1))
}

tbl <- tbl %>% tidyr::pivot_wider(names_from = "year", values_from = c("n", "population", "lower", "upper", "P"))
  
conf.intervals <- tbl %>% rowwise() %>%
  mutate(interval1 = interval_ratio(P_2011, P_2003, lower_2011, lower_2003, upper_2011, upper_2003, 8),
         interval2 = interval_ratio(P_2019, P_2011, lower_2019, lower_2011, upper_2019, upper_2011, 8),
         interval3 = interval_ratio(P_2019, P_2003, lower_2019, lower_2003, upper_2019, upper_2003, 16))

conf.intervals <- conf.intervals %>% 
  select(gender, age, starts_with("interval")) %>%
  tidyr::pivot_longer(cols = starts_with("interval"),
                      names_to = "interval_year", 
                      values_to = "interval") %>%
  mutate(interval_year = case_match(interval_year, "interval1" ~ "2003 - 2011", "interval2" ~ "2011 - 2019", "interval3" ~ "2003 - 2019"))

conf.intervals %>%
  mutate(interval = purrr::map_dbl(interval, ~.x[1])) %>%
  tidyr::pivot_wider(names_from = "age", values_from = "interval") %>%
  mutate(gender = ifelse(gender == "F", "Female", "Male")) %>%
  write.csv("report_plots/tabl1_lower.csv")

conf.intervals %>%
  mutate(interval = purrr::map_dbl(interval, ~.x[2])) %>%
  tidyr::pivot_wider(names_from = "age", values_from = "interval") %>%
  mutate(gender = ifelse(gender == "F", "Female", "Male")) %>%
  write.csv("report_plots/tabl1_upper.csv")

# all age
# dt <- tbl1 %>% filter(age_type == 5, panel == 2) %>%
#   select(-age_type, -panel) %>%
#   group_by(year, gender) %>%
#   summarise(n = sum(n), population = sum(population)) %>%
#   mutate(P = n/population) %>%
#   select(-n, -population) %>%
#   mutate(gender = ifelse(gender == "F", "Female", "Male"))
# dt %>% tidyr::pivot_wider(names_from = "gender", values_from = "P") %>%
#   filter(year <= 2019) %>%
#   write.csv("report_plots/allage/PB.csv")
# 
# dt %>% 
#   filter(year <= 2019) %>%
#   ggplot(aes(x=year, y=P, group=gender, color=gender)) +
#   geom_line() +
#   ggtitle("P(CVD)") +
#   scale_y_log10()
# ggsave("report_plots/allage/PB.png", width=7, height = 7)




# 
# for (gender in c("F", "M")) {
#     dt <- filter(tbl1, panel==2, gender==.env$gender, age_type==5) %>%
#       select(-c(panel, gender, age_type))
#     path <- sprintf("report_plots/csv/%s", ifelse(gender == "F", "Female", "Male"))
#     dt  <- select(dt, year, age, P) %>%
#       tidyr::pivot_wider(id_cols = year, names_from = age, values_from = P) %>%
#       filter(year <= 2019)
#     
#     write.csv(dt, paste0(path, "/PB.csv"), row.names = FALSE)
# }
