rm(list=ls())
library(dplyr)
library(ggplot2)
source("utils.R")

education_levels <- list(
  NULL,
  c("low", "high"),
  c("low", "middle", "high")
)

for (gender in c("F", "M")) {
  path <- sprintf("report_plots/csv/%s/", ifelse(gender=="F", "Female", "Male"))
  
  P_B <- read.csv(paste0(path, "PB.csv"))
  for (o in c("normalweight", "overweight")) {
    for (education in education_levels[[3]]) {
      P_A <- read.csv(paste0(path, sprintf("PA_%s_%s.csv", education, o)))
      
      P_D <- read.csv(paste0(path, sprintf("PD_%s.csv", education)))
      
      P_C <- read.csv(paste0(path, sprintf("PC_%s_%s.csv", education, o)))
      P <- P_A*P_B/(P_C*P_D) %>% mutate(year = 2003:2019)
      if (age_type == 5) {
        colnames(P) <- c("year", age5)
      } else {
        colnames(P) <- c("year", as.character(35:74))
      }
      write.csv(P, sprintf("%sP_%s_%s.csv", path, education, o), row.names = FALSE)
    }
  }
}


# P(death|O, E)
for (gender in c("F", "M")) {
  path <- sprintf("report_plots/csv/alldeath/%s/", ifelse(gender=="F", "Female", "Male"))
  path2 <- sprintf("report_plots/csv/%s/", ifelse(gender=="F", "Female", "Male"))
  P_B <- read.csv(paste0(path, "PB.csv"))
  for (o in c("normalweight", "overweight")) {
    for (education in c("low", "middle", "high")) {
      P_A <- read.csv(paste0(path, sprintf("PA_%s_%s.csv", education, o)))
      
      P_D <- read.csv(paste0(path2, sprintf("PD_%s.csv", education)))
      
      P_C <- read.csv(paste0(path2, sprintf("PC_%s_%s.csv", education, o)))
      P <- P_A*P_B/(P_C*P_D) %>% mutate(year = 2003:2019)
      colnames(P) <- c("year", age5)

      write.csv(P, sprintf("%sP_%s_%s.csv", path, education, o), row.names = FALSE)
    }
  }
}

# ratio: CVD/death
for (gender in c("F", "M")) {
  path <- sprintf("report_plots/csv/alldeath/%s/", ifelse(gender=="F", "Female", "Male"))
  path2 <- sprintf("report_plots/csv/%s/", ifelse(gender=="F", "Female", "Male"))
  
  for (o in c("normalweight", "overweight")) {
    for (education in c("low", "middle", "high")) {
      P_CVD <- read.csv(paste0(path2, sprintf("P_%s_%s.csv", education, o)))
      P <- read.csv(paste0(path, sprintf("P_%s_%s.csv", education, o)))
      P[,2:NCOL(P)] <- P_CVD[,2:NCOL(P)]/P[,2:NCOL(P)]
      colnames(P) <- stringi::stri_replace_all_fixed(colnames(P), ".", "-")
      colnames(P) <- stringi::stri_replace_all_fixed(colnames(P), "X", "")
      write.csv(P, sprintf("%sratio_%s_%s.csv", path, education, o), row.names = FALSE)
    }
  }
}



# all age
PA <- NULL
for (O in c("overweight", "normalweight")) {
  for (e in c("low", "middle", "high")) {
    PA <- read.csv(sprintf("report_plots/allage/PA_%s_%s.csv", e, O)) %>%
      rename(Female = F, Male = M) %>%
      tidyr::pivot_longer(cols = c("Female", "Male"), names_to = "gender", values_to = "PA") %>%
      mutate(O = O, education = e) %>%
      select(-X) %>%
      rbind(PA)
  }
}


PB <- read.csv("report_plots/allage/PB.csv") %>%
  select(-X) %>%
  tidyr::pivot_longer(cols = c("Female", "Male"), names_to = "gender", values_to = "PB")

PC <- read.csv("report_plots/allage/PC.csv") %>%
  select(-X) %>%
  tidyr::pivot_longer(cols = c("Female", "Male"), names_to = "gender", values_to = "PC")

PD <- rbind(read.csv("report_plots/allage/PD_Female.csv") %>% mutate(gender = "Female"),
            read.csv("report_plots/allage/PD_Male.csv") %>% mutate(gender = "Male")) %>%
  tidyr::pivot_longer(cols = c("low", "middle", "high"), names_to = "education", values_to = "PD")

PC_ <- NULL
for (gender in c("Female", "Male")) {
  for (education in c("high", "low and middle")) {
    tmp1 <- rbind(PC %>% filter(gender == .env$gender, year == "2003-2010", education == .env$education)) %>% pull(PC) 
    tmp2 <- rbind(PC %>% filter(gender == .env$gender, year == "2011-2020", education == .env$education)) %>% pull(PC) 
    PC_ <- rbind(PC_, data.frame(year=2003:2010, gender=gender, PC=tmp1, education = education),
                 data.frame(year=2011:2019, gender=gender, PC=tmp2, education=education))
  }
}
 
PC_ <- PC_ %>% filter(education == "low and middle") %>%
  mutate(education = "low") %>%
  rbind(PC_) %>%
  mutate(education = ifelse(education == "low and middle", "middle", education))
  
PC_ <- PC_ %>% mutate(PC = 1-PC) %>%
  mutate(O = "normalweight") %>%
  rbind(PC_ %>% mutate(O = "overweight"))

dt <- PA %>% left_join(PB, by = c("year", "gender")) %>%
  filter(year <= 2019) %>%
  left_join(PC_, by = c("year", "gender", "education", "O")) %>%
  left_join(PD, by = c("year", "gender", "education")) %>%
  mutate(P = (PA * PB) / (PC * PD)) %>%
  select(-c(PA, PB, PC, PD)) %>%
  mutate(education = paste(stringi::stri_trans_totitle(education), "education"))

dt$education <- factor(dt$education, levels = c("Low education", "Middle education", "High education"))
dt$O <- ifelse(dt$O == "normalweight", "Non-obesity", "Obesity")

dt %>% 
  filter(year <= 2019) %>%
  ggplot(aes(x=year, y=P, group = interaction(O, education), color = education)) +
  geom_line(aes(linetype=O)) +
  facet_wrap(~ gender) + 
  scale_y_log10() +
  labs(linetype = "", color="") + ylab("")

ggsave("report_plots/allage/P.png", width = 7, height = 4)

fcast <- function(x) {
  x <- arrange(x, year) %>% pull()
  
  mdl <- forecast::rwf(x, h=10, drift = TRUE)
  tibble(year = 2003:2029, P = c(x, as.numeric(mdl$mean)), 
         lower = c(x, as.numeric(mdl$lower[,"95%"])), 
         upper = c(x, as.numeric(mdl$upper[,"95%"])))
}


dt <- dt %>% group_by(gender, O, education) %>%
  group_modify( ~ fcast(.x))



dt %>% ggplot(aes(x=year, y=P, group = interaction(O, education), color = education, linetype=O)) +
  geom_line() +
  facet_wrap(~ gender) + 
  scale_y_log10() +
  geom_ribbon(aes(x=year, ymin=lower, ymax=upper), dt %>% filter(year >= 2020),alpha = 0.2) +
  labs(linetype = "", color = "") + ylab("") + 
  geom_vline(xintercept = 2020, linetype = 2) +
  scale_x_continuous(breaks = c(2003, 2010, 2019, 2029)) +
  scale_linetype_manual(values = c("Obesity" = "solid", "Non-obesity" = "dashed")) +
  xlab("Year") +
  theme(panel.spacing = unit(1, "lines"))

ggsave("report_plots/allage/P_forecast_interval.png", width = 7, height = 4)


dt %>% ggplot(aes(x=year, y=P, group = interaction(O, education), color = education, linetype=O)) +
  geom_line() +
  facet_wrap(~ gender) + 
  scale_y_log10() +
  labs(linetype = "", color = "") + ylab("") + 
  geom_vline(xintercept = 2020, linetype = 2) +
  scale_x_continuous(breaks = c(2003, 2010, 2019, 2029)) +
  scale_linetype_manual(values = c("Obesity" = "solid", "Non-obesity" = "dashed")) +
  xlab("Year") +
  theme(panel.spacing = unit(1, "lines"))

ggsave("report_plots/allage/P_forecast.png", width = 7, height = 4)



for (o in c("normalweight", "overweight")) {
  for (e in c("low", "middle", "high")) {
    tmpdt <- dt %>% filter(year <= 2019) %>% tidyr::pivot_wider(names_from = "gender", values_from = "P") %>%
      ungroup()
    tmpdt <- tmpdt %>% filter(O == ifelse(o=="normalweight", "Non-Obesity", "Obesity"), education == paste(stringi::stri_trans_totitle(e), "education")) %>% 
      dplyr::select(-O, -education)

    write.csv(tmpdt, sprintf("report_plots/allage/P_%s_%s.csv", e, o), row.names = FALSE)
  }
}


for (o in c("normalweight", "overweight")) {
  for (e in c("low", "middle", "high")) {
    tmpdt <- dt %>% tidyr::pivot_wider(names_from = "gender", values_from = "P") %>%
      ungroup()
    tmpdt <- tmpdt %>% filter(O == o, education == paste(stringi::stri_trans_totitle(e), "education")) %>% 
      dplyr::select(-O, -education)
    
    write.csv(tmpdt, sprintf("report_plots/allage/P_forecast_%s_%s.csv", e, o), row.names = FALSE)
  }
}



