## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(tidy      = "styler",
                      fig.align = "center",
                      comment   = NA,
                      warning   = FALSE,
                      error     = FALSE,
                      message   = FALSE,
                      collapse  = TRUE,
                      out.width = "100%",
                      dev = "ragg_png", #<- prevent default Windows device to
                      # render plots
                      dpi       = 132,
                      echo      = FALSE)

## ----setup--------------------------------------------------------------------
old_options <- options(scipen = 999, digits = 2)
on.exit(options(old_options))

olds <- options(rmarkdown.html_vignette.check_title = FALSE)
on.exit(options(olds))

library(Dyn4cast)
library(tidyverse, quietly = T)

## ----github, echo = TRUE, eval = FALSE----------------------------------------
# 
# # install.packages("devtools")
# pak::pak("JobNmadu/Dyn4cast")

## ----universe, echo = TRUE, eval = FALSE--------------------------------------
# 
# install.packages("Dyn4cast", repos = c("https://jobnmadu.r-universe.dev", "https://cloud.r-project.org"))

## ----install-suggested, echo = TRUE, eval = FALSE-----------------------------
# install.packages(c("lubridate", "tidyverse", "xlsx", "readxl", "rmarkdown",
#                    "covr", "caret", "kableExtra", "knitr", "spelling",
#                    "psych", "lifecycle", "MetBrewer", "data.table", "ggtext",
#                    "lubridate", "forecast", "MASS", "mlogit", "nnet", "betareg",
#                    "mvProbit", "miscTools"))

## ----citation, warning = FALSE------------------------------------------------

citation("Dyn4cast")


