## code to prepare `Data` dataset goes here

Data <- readxl::read_excel("data-raw/data/Data.xlsx")
Data$Date <- as.Date(Data$Date)
saveRDS(Data, "inst/Data.RDS")

usethis::use_data(Data, overwrite = TRUE)

Data1 <- readxl::read_excel("data-raw/data/data1.xlsx")
Data1$Date <- as.Date(Data$Date)
saveRDS(Data1, "inst/StatesAffected.RDS")

usethis::use_data(Data1, overwrite = TRUE)
