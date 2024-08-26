library(dplyr)
library(tidyr)
library(forecast)
source("utils.R")

for (gender in c("Female", "Male")) {
  for (o in c("normalweight", "overweight")) {
    for (education in c("low", "middle", "high")) {
      file <- sprintf("report_plots/csv/%s/P_%s_%s.csv", gender, education, o)
      f <- read.csv(file)
      fcasts <- leecarter_f(f)
      
      
      colnames(f)[2:9] <- age5
      for (type in c("lower", "upper")) {
        colnames(fcasts[[type]])[1:8] <- age5
        output <- rbind(f, fcasts[[type]])
        output <- relocate(output, year)
        fpath <- sprintf("report_plots/csv/interval/%s/%s/P_%s_%s_forecast.csv", gender, type, education, o)
        write.csv(output, fpath, row.names = FALSE)
      }
      
      colnames(fcasts$point)[1:8] <- age5
      output <- rbind(f, fcasts$point)
      output <- relocate(output, year)
      fpath <- sprintf("report_plots/csv/%s/P_%s_%s_forecast.csv", gender, education, o)
      write.csv(output, fpath, row.names = FALSE)
    }
  }
  
  P_B <- sprintf("report_plots/csv/%s/PB.csv", gender)

  f <- read.csv(P_B)
  fcasts <- leecarter_f(f)
  colnames(f)[2:9] <- age5
  for (type in c("lower", "upper")) {
    colnames(fcasts[[type]])[1:8] <- age5
    output <- rbind(f, fcasts[[type]])
    output <- relocate(output, year)
    fpath <- sprintf("report_plots/csv/interval/%s/%s/PB_forecast.csv", gender, type)
    write.csv(output, fpath, row.names = FALSE)
  }
  colnames(fcasts$point)[1:8] <- age5
  output <- rbind(f, fcasts$point)
  output <- relocate(output, year)
  fpath <- sprintf("report_plots/csv/%s/PB_forecast.csv", gender)
  write.csv(output, fpath, row.names = FALSE)
}



