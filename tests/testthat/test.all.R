# getwd()
# setwd("../koboquest/")
setwd("./tests/testthat/")
library(testthat)
library(koboquest)
source("./test_utilities.R")
test_file("./test_load_questionnaire_m.R")
test_file("./test_questionnaire_skiplogic.R")

