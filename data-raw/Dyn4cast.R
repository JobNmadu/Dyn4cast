## code to prepare `Dyn4cast` dataset goes here
DDD <- as.data.frame(readr::read_csv("data/Data.csv"))
usethis::use_data(Dyn4cast, overwrite = TRUE)
