## code to prepare `Data` dataset goes here

COVID19Nig <- readxl::read_excel("data/Data.xlsx")
COVID19Nig$Date <- as.Date(COVID19Nig$Date)
#saveRDS(COVID19Nig, "inst/COVID19Nig.RDS")
saveRDS(COVID19Nig, "R/COVID19Nig.RDS")

usethis::use_data(COVID19Nig, overwrite = TRUE)

StatesAffected <- readxl::read_excel("data/data1.xlsx")
StatesAffected$Date <- as.Date(StatesAffected$Date)
#saveRDS(StatesAffected, "inst/StatesAffected.RDS")
saveRDS(StatesAffected, "R/StatesAffected.RDS")

usethis::use_data(StatesAffected, overwrite = TRUE)
