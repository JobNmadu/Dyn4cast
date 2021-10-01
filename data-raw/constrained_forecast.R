## code to prepare `Data` dataset goes here

library(readxl)
Data1 <- read_excel("data/data1.xlsx")
Data1$Date <- as.Date(Data1$Date)

usethis::use_data(Data1, overwrite = TRUE)
