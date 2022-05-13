## code to prepare `Data` dataset goes here

COVID19 <- readxl::read_excel("data-raw/Data.xlsx")
COVID19$Date <- as.Date(COVID19$Date)
#saveRDS(COVID19Nig, "inst/COVID19Nig.RDS")
saveRDS(COVID19, "R/COVID19.RDS")

usethis::use_data(COVID19, overwrite = TRUE)

Data <- readxl::read_excel("data-raw/data1.xlsx")
Data$Date <- as.Date(Data$Date)
#saveRDS(StatesAffected, "inst/StatesAffected.RDS")
saveRDS(Data, "R/Data.RDS")

usethis::use_data(Data, overwrite = TRUE)
