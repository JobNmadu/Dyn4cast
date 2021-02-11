## code to prepare `Dyn4cast` dataset goes here
Data <- readxl::read_excel("data/Data.xlsx")
usethis::use_data(Dyn4cast, overwrite = TRUE)
