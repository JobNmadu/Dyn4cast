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
options(rmarkdown.html_vignette.check_title = FALSE)

## ----github, echo = TRUE, eval = FALSE----------------------------------------
# 
# # install.packages("devtools")
# devtools::install_github("JobNmadu/Dyn4cast")

## ----universe, echo = TRUE, eval = FALSE--------------------------------------
# 
# install.packages("Dyn4cast", repos = c("https://jobnmadu.r-universe.dev", "https://cloud.r-project.org"))

## ----install-suggested, echo = TRUE, eval = FALSE-----------------------------
# install.packages(c("lubridate", "tidyverse", "xlsx", "readxl", "rmarkdown",
#                    "covr", "qpdf", "caret", "kableExtra", "knitr", "spelling",
#                    "psych", "lifecycle", "MetBrewer", "data.table", "ggtext"))

## ----citation, comment = "", warning = FALSE----------------------------------

citation("Dyn4cast")


