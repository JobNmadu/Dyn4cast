## code to prepare `Data` dataset goes here

library(readxl)
COVID19Nig <- read_excel("data/Data.xlsx")
COVID19Nig$Date <- as.Date(COVID19Nig$Date)

usethis::use_data(COVID19Nig, overwrite = TRUE)
