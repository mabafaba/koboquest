#' koboquest: Reading \href{http://xlsform.org/en/}{XLSForm} questionnaires
#'
#'For data collected with \href{https://www.kobotoolbox.org/}{kobotoolbox}, \href{https://opendatakit.org/}{ODK} or similar.
#'
#' This package lets you load a questionnaire and match it with the data. It then provides three main functionalities:
#'
#' \itemize{
#'   \item identifying question types
#'   \item converting xml style data headers and values to labels
#'   \item parsing questionnaire skiplogic ('relevant' column in XLSForms.)
#' }
#'
#' A questionnaire (in .csv format) is loaded globally with \code{\link{load_questionnaire}}.
#' All other functions then refer to that questionnaire automatically. (See below on using multiple questionnaires in parallel.)
#'
#'
#' @section Identifying question types:
#' \itemize{
#' \item \code{\link{question_type}} is the standard function to determine data types.
#'}
#' There are functions for groups of types:
#'  \itemize{
#' \item \code{\link{question_is_categorical}},
#' \item \code{\link{question_is_numeric}},
#'}
#' And for testing individual types specifically:
#'\itemize{
#'\item \code{\link{question_is_select_multiple}},
#' \item \code{\link{question_is_select_one}},
#' \item \code{\link{question_is_sm_choice}},
#'}
#' @section Parsing skiplogic:
#' Use \code{\link{question_is_skipped}} To find out which records of a certain question were skipped
#'
#'
#' @section Using Multiple Quesitonnaires:
#'
#' If multiple questionnaires need to be used in parallel, you can store the output of \code{link{load_questionniare}} in an object. All other functions in this package are then available relating to that specific questionnaire as a list element of that object.
#' Example:
#' \preformatted{
#' # load the first questionnaire:
#' q1<-load_questionnaire(...)
#'
#' question_is_categorical(...) # global functions now refer to q1
#' q1$question_is_categorical(...) # always refers to q1
#'
#' # load the second questionnaire:
#' q2<-load_quesitonniare(...)
#' question_is_categorical(...) # global functions now refer to q2
#' q1$question_is_categorical(...) # always refers to q1
#' q2$question_is_categorical(...) # always refers to q2
#'}
#' @docType package
#' @name koboquest
#' @md
NULL

#'@import data.table dplyr stringi stringr magrittr utils
NULL


# require("knitr")
# ls(pattern = "question_is") %>% paste("#' \\code{\\link{",.,"}}") %>% paste(collapse=", \n") %>% cat
