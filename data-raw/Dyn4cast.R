## code to prepare `Data` dataset goes here

library(readxl)
Data <- read_excel("data/Data.xlsx")
Data$Date <- as.Date(Data$Date)

usethis::use_data(Data, overwrite = TRUE)
