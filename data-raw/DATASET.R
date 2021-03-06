## code to prepare `Data` dataset goes here

Data <- readxl::read_excel("data-raw/data/Data.xlsx")
Data$Date <- as.Date(Data$Date)
saveRDS(Data, "inst/Data.RDS")

usethis::use_data(Data, overwrite = TRUE)
