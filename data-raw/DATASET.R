## code to prepare `Data` dataset goes here

COVID19 <- readr::read_csv("Data.csv")
COVID19$Date <- as.Date(COVID19$Date)
saveRDS(COVID19, "inst/COVID19.RDS")

usethis::use_data(COVID19, overwrite = TRUE)

Data <- readr::read_csv("data1.csv")
Data$Date <- as.Date(Data$Date)
saveRDS(Data, "inst/Data.RDS")

usethis::use_data(Data, overwrite = TRUE)

linearsystems <- readr::read_csv("linearsystems.csv")
saveRDS(linearsystems, "inst/linearsystems.RDS")

usethis::use_data(linearsystems, overwrite = TRUE)

sampling <- readr::read_csv("sampling.csv")
saveRDS(sampling, "inst/sampling.RDS")

usethis::use_data(sampling, overwrite = TRUE)

Quicksummary <- readr::read_csv("quicksummary.csv")
saveRDS(Quicksummary, "inst/Quicksummary.RDS")

usethis::use_data(Quicksummary, overwrite = TRUE)

Transform <- readr::read_csv("transform.csv")
saveRDS(Transform, "inst/Transform.RDS")

usethis::use_data(Transform, overwrite = TRUE)

garrett_table <- readr::read_csv("garrett table.csv")
saveRDS(garrett_table, "inst/garrett_table.RDS")

usethis::use_data(garrett_table, overwrite = TRUE)

garrett_data <- readr::read_csv("garrett.csv")
saveRDS(garrett_data, "inst/garrett_data.RDS")

usethis::use_data(garrett_data, overwrite = TRUE)

treatments <- readr::read_csv("treatments.csv")
saveRDS(treatments, "inst/treatments.RDS")

usethis::use_data(treatments, overwrite = TRUE)

mdpi1 <- readr::read_csv("mdpi1.csv")
saveRDS(mdpi1, "inst/mdpi1.RDS")

usethis::use_data(mdpi1, overwrite = TRUE)

mdpi2 <- readr::read_csv("mdpi2.csv")
saveRDS(mdpi2, "inst/mdpi2.RDS")

usethis::use_data(mdpi2, overwrite = TRUE)
