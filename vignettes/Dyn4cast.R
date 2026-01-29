## ----setup, include=FALSE-----------------------------------------------------

knitr::opts_chunk$set(tidy      = "styler",
                      fig.align = "center",
                      comment   = NA,
                      warning   = FALSE,
                      error     = FALSE,
                      message   = FALSE,
                      collapse  = FALSE,
                      out.width = "100%",
                      dev = "ragg_png", #<- prevent default Windows device to
                      # render plots
                      dpi       = 132,
                      echo      = FALSE)

options(scipen = 999, digits = 2)

library(Dyn4cast)
library(tidyverse, quietly = T)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----github, echo = TRUE, eval = FALSE----------------------------------------
# 
# # install.packages("devtools")
# devtools::install_github("JobNmadu/Dyn4cast")

## ----universe, echo = TRUE, eval = FALSE--------------------------------------
# 
# install.packages("Dyn4cast", repos = c("https://jobnmadu.r-universe.dev", "https://cloud.r-project.org"))

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(splines)
# library(forecast)
# library(readr)
# lower <- 1
# upper <- 37
# Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
# FitModel <- scaledlogit(x2 = fitted.values(Model), lower = lower,
#  upper = upper)
# ForecastModel <- forecast(FitModel, h = length(200))
# constrainedforecast(model10 = ForecastModel, lower, upper)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# data0 <- Transform %>%
# pivot_longer(!X, names_to = "Factors", values_to = "Data")
# 
# ggplot(data = data0, aes(x = X, y = Data, fill = Factors, color = Factors)) +
#   geom_line() +
#   scale_fill_brewer(palette = "Set1") +
#   scale_color_brewer(palette = "Set1") +
#   labs(y = "Data", x = "Series", color = "Factors") +
#   theme_bw(base_size = 12)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# data1 <- data_transform(Transform[, -1], 1)
# data1 <- cbind(Transform[, 1], data1)
# data1 <- data1 %>%
#   pivot_longer(!X, names_to = "Factors", values_to = "Data")
# 
# ggplot(data = data1, aes(x = X, y = Data, fill = Factors, color = Factors)) +
#   geom_line() +
#   scale_fill_brewer(palette = "Set1") +
#   scale_color_brewer(palette = "Set1") +
#   labs(y = "Data", x = "Series", color = "Factors") +
#   theme_bw(base_size = 12)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# data2 <- data_transform(Transform[, -1], 2)
# data2 <- cbind(Transform[, 1], data2)
# data2 <- data2 %>%
#   pivot_longer(!X, names_to = "Factors", values_to = "Data")
# 
# ggplot(data = data2, aes(x = X, y = Data, fill = Factors, color = Factors)) +
#   geom_line() +
#   scale_fill_brewer(palette = "Set1") +
#   scale_color_brewer(palette = "Set1") +
#   labs(y = "Data", x = "Series", color = "Factors") +
#   theme_bw(base_size = 12)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# data3 <- data_transform(Transform[, -1], 3)
# data3 <- cbind(Transform[, 1], data3)
# data3 <- data3 %>%
#   pivot_longer(!X, names_to = "Factors", values_to = "Data")
# 
# ggplot(data = data3, aes(x = X, y = Data, fill = Factors, color = Factors)) +
#   geom_line() +
#   scale_fill_brewer(palette = "Set1") +
#   scale_color_brewer(palette = "Set1") +
#   labs(y = "Data", x = "Series", color = "Factors") +
#   theme_bw(base_size = 12)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(forecast)
# library(readr)
# COVID19$Date <- zoo::as.Date(COVID19$Date, format = '%m/%d/%Y')
# 
# LEN <- length(COVID19$Case)
# Dss <- seq(COVID19$Date[1], by = "day", length.out = LEN)
# ORIGIN = "2020-02-29"
# lastdayfo21 <- Dss[length(Dss)]
# gggg <- COVID19[COVID19$Date <= lastdayfo21 - 28, ]
# BREAKS <- c(70, 131, 173, 228, 274)
# dyrima <- auto.arima(gggg$Case)
# 
# DynamicForecast(date = gggg$Date, series = gggg$Case, dyrima = dyrima,
#                 BREAKS = BREAKS, MaximumDate = "2021-02-10",
#                 Trend = "Day", Length = 0, Type = "Integer")

## ----echo = TRUE, eval = FALSE------------------------------------------------
# lastdayfo21 <- Dss[length(Dss)]
# ddy <- COVID19[COVID19$Date <= lastdayfo21 - 14, ]
# BREAKS = c(70, 131, 173, 228, 274)
# dyrima <- auto.arima(ddy$Case)
# 
# suppressWarnings(DynamicForecast(date = ddy$Date, series = ddy$Case,
#                                  dyrima = dyrima, BREAKS = BREAKS ,
#                 MaximumDate = "2021-02-10", Trend = "Day", Length = 0,
#                 Type = "Integer"))

## ----echo = TRUE, eval = FALSE------------------------------------------------
# DD <- rnorm(100000)
# formattedcut(DD, 12, FALSE)
# DD1 <- cut(DD, 12)
# formattedcut(DD1, 12, TRUE)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(readr)
# garrett_data <- data.frame(garrett_data)
# ranking <- c("Serious constraint", "Constraint",
# "Not certain it is a constraint", "Not a constraint",
# "Not a serious constraint")
# 
# garrett_ranking(garrett_data, 5, ranking)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# garrett_ranking(garrett_data, 5)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# garrett_ranking(garrett_data, 8)
# 
# garrett_ranking(garrett_data, 4)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# df <- data.frame(Age = c(49, 30, 44, 37, 29, 56, 28, 26, 33, 45, 45, 19,
#                          32, 22, 19, 28, 28, 36, 56, 34),
# Sex = c("male", "female", "female", "male", "male", "male", "female",
#         "female", "Prefer not to say", "male", "male", "female", "female",
#         "male", "Non-binary/third gender", "male", "female", "female", "male",
#         "male"))
# gender(df)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(tidyverse)
# library(ggtext)
# 
# y <- linearsystems$MKTcost
# x <- select(linearsystems, -MKTcost)
# Linearsystems(y, x, 6, 15)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(tidyverse)
# library(ggtext)
# 
# x <- sampling[, -1]
# y <- sampling$qOutput
# limit <- 20
# mod <-3
# Test <- NA
# Linearsystems(y, x, 3, 15)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(tidyverse)
# library(ggtext)
# 
# x <- sampling[, -1]
# y <- sampling$qOutput
# mod <- 1
# Linearsystems(y, x, 1, 15)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(tidyverse)
# library(ggtext)
# 
# x <- sampling[, -1]
# y <- sampling$qOutput
# mod <- 2
# Linearsystems(y, x, 2, 15)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(tidyverse)
# library(ggtext)
# 
# x <- sampling[, -1]
# y <- sampling$qOutput
# ddc <- cbind(y, x)
# sampling <- sample(1 : nrow(ddc), 0.8 * nrow(ddc))
# train <- ddc[sampling, ]
# Test  <- ddc[-sampling, ]
# y <- train$y
# x <- train[, -1]
# mod <- 4
# Linearsystems(y, x, 4, 15, Test)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(tidyverse)
# library(ggtext)
# 
# y <- linearsystems$MKTcost
# x <- select(linearsystems, -MKTcost)
# Linearsystems(y, x, 5, 15)

## ----echo = TRUE, eval = FALSE------------------------------------------------
#  data <- mdpi1 # data from `MPI` package
#  data1 <- mdpi2 #from `mpitbR` package
# 
# dm <- list(d1 = c("Child.Mortality", "Access.to.health.care"),
#              d2 = c("Years.of.education", "School.attendance", "School.lag"),
#              d3 = c("Cooking.Fuel", "Access.to.clean.source.of.water",
#                     "Access.to.an.improve.sanatation", "Electricity",
#                     "Housing.Materials", "Asset.ownership"))
#   mdpi(data, dm, plots = "t", Factor = "Region")

## ----echo = TRUE, warning = FALSE, eval = FALSE-------------------------------
# mdpi(data, dm, plots = "t")

## ----echo = TRUE, warning = FALSE, eval = FALSE-------------------------------
# dm <- list(d1 = c("Child.Mortality", "Access.to.health.care"),
#              d2 = c("Years.of.education", "School.attendance", "School.lag"),
#              d3 = c("Cooking.Fuel", "Electricity", "Housing.Materials",
#                     "Asset.ownership"),
#              d4 = c("Access.to.clean.source.of.water",
#                     "Access.to.an.improve.sanatation"))
# 
#   mdpi(data, dm, id_add = c("Water and Sanitation"),
#                 plots = "t", Factor = "Region")

## ----echo = TRUE, warning = FALSE, eval = FALSE-------------------------------
# dm <- list(d1 = c("d_cm"),
#              d2 = c("d_satt","d_educ"),
#              d3 = c("d_elct", "d_hsg","d_ckfl","d_asst"),
#              d4 = c("d_sani","d_wtr"),
#              d5 = c("d_nutr"))
# 
#   mdpi(data1, dm, id_add = "Water and Sanitation", id_add1 = "Nutrition",
#        plots = "t", Factor = "region")

## ----echo = TRUE, warning = FALSE, eval = FALSE-------------------------------
# mdpi(data1, dm, id_add = "Water and Sanitation", id_add1 = "Nutrition",
#        Factor = "region")

## ----echo = TRUE, warning = FALSE, eval = FALSE-------------------------------
# mdpi(data1, dm, id_add = "Water and Sanitation", id_add1 = "Nutrition")

## ----echo = TRUE, warning = FALSE, eval = FALSE-------------------------------
# dm <- list(d1 = c("d_cm"),
#              d2 = c("d_satt","d_educ"),
#              d3 = c("d_elct", "d_ckfl"),
#              d4 = c("d_sani","d_wtr"),
#              d5 = c("d_nutr"),
#              d6 = c("d_hsg","d_asst"))
# 
#   mdpi(data1, dm, id_add = "Water and Sanitation", id_add1 = "Nutrition",
#                 id_addn = "Housing and Assets", plots = "t", Factor = "region")

## ----echo = TRUE, warning = FALSE, eval = FALSE-------------------------------
# dm <- list(d1 = c("d_cm"),
#              d2 = c("d_satt","d_educ"),
#              d3 = c("d_elct", "d_ckfl"),
#              d4 = c("d_sani","d_wtr"),
#              d5 = c("d_nutr"),
#              d6 = c("d_hsg"),
#              d7 = c("d_asst"))
# 
#   mdpi(data1, dm, id_add = "Water and Sanitation",
#                 id_add1 = "Nutrition", id_addn = c("Housing", "Assets"),
#                 plots = "t", Factor = "region")

## ----echo = TRUE, warning = FALSE, eval = FALSE-------------------------------
# dm <- list(d1 = c("d_cm"),
#              d2 = c("d_satt","d_educ"),
#              d3 = c("d_elct", "d_ckfl"),
#              d4 = c("d_sani"),
#              d5 = c("d_nutr"),
#              d6 = c("d_hsg"),
#              d7 = c("d_asst"),
#              d8 = c("d_wtr"))
# 
#   mdpi(data1, dm, id_add = "Sanitation",
#                  id_add1 = "Nutrition",
#                  id_addn = c("Housing", "Assets", "Water"),
#                  plots = "t", Factor = "region")

## ----echo = TRUE, warning = FALSE, eval = FALSE-------------------------------
#   dm <- list(d1 = c("d_cm"),
#              d2 = c("d_satt","d_educ"),
#              d3 = c("d_elct", "d_ckfl"),
#              d4 = c("d_sani"),
#              d5 = c("d_nutr"),
#              d6 = c("d_hsg"),
#              d7 = c("d_asst"),
#              d8 = c("d_wtr"),
#              d9 = c("d_elct"))
# 
#   mdpi(data1, dm, id_add = "Sanitation",
#                  id_add1 = "Nutrition",
#                  id_addn = c("Housing", "Assets", "Water", "Electricity"),
#                  plots = "t", Factor = "region")

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(splines)
# library(readr)
# Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
# suppressWarnings(MLMetrics(Observed = Data, yvalue = Data$states, modeli = Model, K = 2,
#            Name = "Linear", Form = "LM", kutuf = 0, TTy = "Number"))

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(psych)
# library(readr)
# ddd <- Quicksummary
# GGn <- names(ddd)
# GG <- ncol(ddd)
# GGx <- c(paste0('x0', 1 : 9), paste("x", 10 : ncol(ddd), sep = ""))
# names(ddd) <- GGx
# lll <- fa.parallel(ddd, fm = "minres", fa = "fa")
# dat <- fa(ddd, nfactors = lll[["nfact"]], rotate = "varimax",fm = "minres")
# 
# model_factors(data = dat, DATA = ddd)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# Data <- c(1.2, 0.5, 0.103, 7, 0.1501)
# Percent(Data = Data, Type = "Frame")
# Data <- 1.2
# Percent(Data = Data, Type = "Value")

## ----echo = TRUE, eval = FALSE------------------------------------------------
# df <- data.frame(c(A = 2320, 5760, 4800, 2600, 5700, 7800, 3000, 6300, 2400,
# 10000, 2220, 3740),
# B = c(0, 0, 1620, 3600, 1200, 1200, 1200, 4250, 14000, 10000, 1850, 1850),
# C = c(3000, 3000, 7800, 5400, 3900, 7800, 1950, 2400, 2400, 7000, 1850, 1850),
# D = c(2900, 5760, 3750, 5400, 4095, 3150, 2080, 7800, 1920, 1200, 5000, 1950),
# E = c(2900, 2030, 0, 5400, 5760, 1800, 2000, 1950, 1850, 3600, 5200, 5760),
# F = c(2800, 5760, 1820, 4340, 7500, 2400, 2300, 1680, 1850, 0, 2800, 8000),
# G = c(5760, 4600, 13000, 7800, 6270, 1200, 1440, 8000, 1200, 2025, 4800, 2600),
# H = c(2100, 5760, 8250, 3900, 1800, 1200, 4800, 1800, 7800, 2035, 8000, 3000))
# Percent(Data = df, Type = "Frame")  # Value, Frame

## ----echo = TRUE, eval = FALSE------------------------------------------------
# quicksummary(x = Quicksummary, Type = 2)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# x <- select(linearsystems, 1:6)
# quicksummary(x = x, Type = 1)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# library(readr)
# Treatment = treatments$treatment
# data = treatments[, c(2:3)]
# treatment_model(Treatment, data)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# 
# library(readr)
# garrett_data <- data.frame(garrett_data)
# index_construction(garrett_data)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# 
# library(readr)
# garrett_data <- data.frame(garrett_data)
# relative_likert(garrett_data, Ranks = 3, Option = "sccore")
# relative_likert(garrett_data, Ranks = 5, Option = "sccore")
# relative_likert(garrett_data, Ranks = 7, Option = "sccore")
# relative_likert(garrett_data, Ranks = 9, Option = "sccore")
# 
# relative_likert(Quicksummary, Ranks = 5, Option = "sccore")
# 
# library(tidyverse)
# data_l <- garrett_data %>%
# pivot_longer(cols = everything()) %>%
#  mutate(value = case_when(value == 5 ~ "Serious constraint",
#                           value == 4 ~ "Constraint",
#                           value == 3 ~ "Not certain it is a constraint",
#                           value == 2 ~ "Not a constraint",
#                           value == 1 ~ "Not a serious constraint",
#                           .default = "None")) %>%
#  group_by(name) %>%
#  mutate(row = row_number()) %>%
#  pivot_wider(names_from = name, values_from = value) %>%
#  select(-row) %>%
#  unnest(cols = everything())
# 
#  ranking <- c("Serious constraint", "Constraint",
# "Not certain it is a constraint", "Not a constraint",
# "Not a serious constraint")
# 
#  relative_likert(data_l, Likert = ranking)

