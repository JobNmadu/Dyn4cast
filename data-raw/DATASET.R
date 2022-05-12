## code to prepare `Data` dataset goes here

COVID19Nig <- readxl::read_excel("data/Data.xlsx")
COVID19Nig$Date <- as.Date(COVID19Nig$Date)
#saveRDS(COVID19Nig, "inst/COVID19Nig.RDS")
saveRDS(COVID19Nig, "Dyn4cast/data/COVID19.RDS")

usethis::use_data(COVID19, overwrite = TRUE)

StatesAffected <- readxl::read_excel("data/data1.xlsx")
StatesAffected$Date <- as.Date(StatesAffected$Date)
#saveRDS(StatesAffected, "inst/StatesAffected.RDS")
saveRDS(StatesAffected, "Dyn4cast/data/Data.RDS")

usethis::use_data(Data, overwrite = TRUE)
