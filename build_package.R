
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

# detach("package:reachR")
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

source("./dependencies_koboquest.R")
.install_koboquest(T)
rm(list=ls())
require(koboquest)

question_in_questionnaire(asdf)

devtools::test()








