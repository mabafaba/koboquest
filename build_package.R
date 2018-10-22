
newpackage<-function(packagename){
  rm(list=ls())

  # detach("package:reachR")
  this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd(this_script_path)
  create(packagename)
  devtools::use_testthat()
}

build<-function(){

rm(list=ls())

this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)

rfiles<-paste0("./R/", list.files("./R/"))
sapply(rfiles,source)

require("roxygen2")
require("devtools")
roxygenize(clean=T)
}



this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
build()
# setwd("../../")
# devtools::load_all("../koboquest")
devtools::test("../koboquest")
devtools::load_all("../koboquest")
require("devtools");install_github("mabafaba/koboquest",quiet = T);detach("package:devtools")
