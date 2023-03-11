## code to prepare `Data` dataset goes here

COVID19 <- readxl::read_excel("Data.xlsx")
COVID19$Date <- as.Date(COVID19$Date)
saveRDS(COVID19, "inst/COVID19.RDS")

usethis::use_data(COVID19, overwrite = TRUE)

Data <- readxl::read_excel("data1.xlsx")
Data$Date <- as.Date(Data$Date)
saveRDS(Data, "inst/Data.RDS")

usethis::use_data(Data, overwrite = TRUE)

linearsystems <- readxl::read_excel("linearsystems.xlsx")
saveRDS(linearsystems, "inst/linearsystems.RDS")

usethis::use_data(linearsystems, overwrite = TRUE)

Quicksummary <- readxl::read_excel("quicksummary.xlsx")
saveRDS(Quicksummary, "inst/Quicksummary.RDS")

usethis::use_data(Quicksummary, overwrite = TRUE)

sample <- readxl::read_excel("sample.xlsx")
saveRDS(sample, "inst/sample.RDS")

usethis::use_data(sample, overwrite = TRUE)

Transform <- readxl::read_excel("transform.xlsx")
saveRDS(Transform, "inst/Transform.RDS")

usethis::use_data(Transform, overwrite = TRUE)
