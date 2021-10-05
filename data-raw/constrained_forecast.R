## code to prepare `Data` dataset goes here

library(readxl)
StatesAffected <- read_excel("data/data1.xlsx")
StatesAffected$Date <- as.Date(StatesAffected$Date)

usethis::use_data(StatesAffected, overwrite = TRUE)
