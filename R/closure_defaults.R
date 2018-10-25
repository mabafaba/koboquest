#' Convert kobo xml choice names to labels
#'
#' Uses a loaded kobo questionnaire to get the labels of select_one and select_multiple type questions.
#'
#' @param responses A vector of responses in kobo xml name format
#' @param variable.name The xml name of a kobo question. (as it appears in the kobo questionnaire and subsequently in the data column headers)
#' Should be a question of type 'select_one' or 'select_multiple'.
#' @return A vector of strings with labels corresponding to the choices supplied in \code{responses}.
#' @details To use this you must first successfully run \code{\link{load_questionnaire}}.
#' If conversion of a value in \code{responses} fails, the original input value is returned. This happens in the following cases:
#'\itemize{
#'   \item No questionnaire has been loaded with \code{\link{load_questionnaire}}
#'   \item \code{variable.name} could not be found in the questionnaire, or in the data that was supplied to \code{\link{load_questionnaire}}
#'   \item A value in \code{responses} is not listed in the loaded questionnaire
#'   \item \code{variable.name} is not listed as a question of type 'select_one' or 'select_multiple' in the questionnaire.
#' }
#'
#' This does not work for concatenated responses of 'select_multiple' questions (e.g. strings of the form "choice_a choice_b"); The \code{responses} should have only a single response per element.
#'
#' @seealso  \code{\link{load_questionnaire}} must be run first. The equivalent of this but for question labels is \code{\link{question_get_question_label}},
#' @export
#' @examples
#' question_get_choice_labels(mydata$location, "location")
question_get_choice_labels<-function(responses,variable.name){
  question_get_choice_labels_internal(responses,variable.name)
}

#' Convert kobo xml question names to labels
#'
#' Uses a loaded kobo questionnaire to get the label corresponding to a question xml name / data column headers.
#'
#' @param question.name The xml name of a kobo question as a string. (as it appears in the kobo questionnaire and subsequently in the data column headers)
#' @return A string with the kobo label of the question / data column header
#' @details To use this you must first successfully run \code{\link{load_questionnaire}}.
#' If conversion to label fails, the original input value is returned. This happens in the following cases:
#'\itemize{
#'   \item No questionnaire has been loaded with \code{\link{load_questionnaire}}
#'   \item \code{question.name} could not be found in the questionnaire, or in the data that was supplied to \code{\link{load_questionnaire}}
#' }
#'
#' @seealso  \code{\link{load_questionnaire}} must be run first. The equivalent of this but for choice labels is \code{\link{question_get_choice_labels}},
#' @export
#' @examples
#' question_get_question_label("a_variable_name")
#'@export
question_get_question_label<-function(question.name){
  question_get_question_label_internal(question.name) }


#' Determine if a kobo question is of type 'numeric'
#'
#' Uses a loaded kobo questionnaire to look up the question type for a given question.
#'
#' @param question.name The xml name of a kobo question as a string. (as it appears in the kobo questionnaire and subsequently in the data column headers)
#' @return \code{TRUE} if the question is listed as a numeric type in the questionnaire. \code{FALSE} if the question is listed as a different type. \code{FALSE} if the question type could not be determined from the questionnaire.
#' @details To use this you must first successfully run \code{\link{load_questionnaire}}.
#' This does not derive the data type from any actual data; it only looks up the type defined in the questionnaire.
#' If type identification fails, the default return value is \code{FALSE}.This happens in the following cases:
#'\itemize{
#'   \item No questionnaire has been loaded with \code{\link{load_questionnaire}}
#'   \item \code{question.name} could not be found in the questionnaire, or in the data that was supplied to \code{\link{load_questionnaire}}
#' }
#'
#' @seealso  \code{\link{load_questionnaire}} must be run first.
#' Use \code{\link{question_type}} for the most generalised way to guess the data type.
#' Part of the \code{question_is_*} family of functions:
#' testing for specific types:
#' \code{\link{question_is_numeric}},
#' \code{\link{question_is_categorical}},
#' \code{\link{question_is_select_one}},
#' \code{\link{question_is_multiple}},
#' \code{\link{question_is_sm_choice}}
#'
#' parsing kobo skip-logic:
#' \code{\link{question_is_skipped}}
#' @export
#' @examples
#' question_is_numeric("some_numeric_kobo_xml_question_name") # TRUE
#' question_is_numeric("some_categorical_kobo_xml_question_name") # FALSE
#' question_is_numeric("some_unidentified_string") # FALSE
#'@export
question_is_numeric<-function(question.name){
  question_is_numeric_internal(question.name) }





#' Determine if a kobo question is of type 'select_one'
#'
#' Uses a loaded kobo questionnaire to look up the question type for a given question.
#'
#' @param question.name The xml name of a kobo question as a string. (as it appears in the kobo questionnaire and subsequently in the data column headers)
#' @return \code{TRUE} if the question is listed as a select_one type in the questionnaire. \code{FALSE} if the question is listed as a different type. \code{FALSE} if the question type could not be determined from the questionnaire.
#' @details To use this you must first successfully run \code{\link{load_questionnaire}}.
#' This does not derive the data type from any actual data; it only looks up the type defined in the questionnaire.
#' If type identification fails, the default return value is \code{FALSE}.This happens in the following cases:
#'\itemize{
#'   \item No questionnaire has been loaded with \code{\link{load_questionnaire}}
#'   \item \code{question.name} could not be found in the questionnaire, or in the data that was supplied to \code{\link{load_questionnaire}}
#' }
#'
#' @seealso  \code{\link{load_questionnaire}} must be run first.
#' Use \code{\link{question_type}} for the most generalised way to guess the data type.
#' Part of the \code{question_is_*} family of functions:
#' testing for specific types:
#' \code{\link{question_is_numeric}},
#' \code{\link{question_is_categorical}},
#' \code{\link{question_is_select_one}},
#' \code{\link{question_is_multiple}},
#' \code{\link{question_is_sm_choice}}
#'
#' parsing kobo skip-logic:
#' \code{\link{question_is_skipped}}
#' @export
#' @examples
#' question_is_select_one("some_numeric_kobo_xml_question_name") # FALSE
#' question_is_select_one("a_select_one_kobo_xml_question_name") # TRUE
#' question_is_select_one("a_select_multiple_kobo_xml_question_name") # FALSE
#' question_is_numeric("some_unidentified_string") # FALSE
#'@export
question_is_select_one<-function(question.name){
  question_is_select_one_internal(question.name)
}


#' Determine if a kobo question is of type 'select_multiple'
#'
#' Uses a loaded kobo questionnaire to look up the question type for a given question.
#'
#' @param question.name The xml name of a kobo question as a string. (as it appears in the kobo questionnaire and subsequently in the data column headers)
#' @return \code{TRUE} if the question is listed as a select_multiple type in the questionnaire. \code{FALSE} if the question is listed as a different type. \code{FALSE} if the question type could not be determined from the questionnaire.
#' @details To use this you must first successfully run \code{\link{load_questionnaire}}.
#' This does not derive the data type from any actual data; it only looks up the type defined in the questionnaire.
#' If type identification fails, the default return value is \code{FALSE}.This happens in the following cases:
#'\itemize{
#'   \item No questionnaire has been loaded with \code{\link{load_questionnaire}}
#'   \item \code{question.name} could not be found in the questionnaire, or in the data that was supplied to \code{\link{load_questionnaire}}
#' }
#'
#' @seealso  \code{\link{load_questionnaire}} must be run first.
#' Use \code{\link{question_type}} for the most generalised way to guess the data type.
#' Part of the \code{question_is_*} family of functions:
#' testing for specific types:
#' \code{\link{question_is_numeric}},
#' \code{\link{question_is_categorical}},
#' \code{\link{question_is_select_one}},
#' \code{\link{question_is_multiple}},
#' \code{\link{question_is_sm_choice}}
#'
#' parsing kobo skip-logic:
#' \code{\link{question_is_skipped}}
#' @export
#' @examples
#' question_is_select_multiple("some_numeric_kobo_xml_question_name") # FALSE
#' question_is_select_multiple("a_select_multiple_kobo_xml_question_name") # TRUE
#' question_is_select_multiple("a_select_one_kobo_xml_question_name") # FALSE
#' question_is_numeric("some_unidentified_string") # FALSE
#'@export
question_is_select_multiple<-function(question.name){
  question_is_select_multiple_internal(question.name)
}


#' Determine if a data column header is a logical choice column of a select_multiple question
#'
#' @param question.name The xml name of a kobo question as a string. (as it appears in the kobo questionnaire and subsequently in the data column headers)
#' @return \code{TRUE} if the question is listed as a select_multiple type in the questionnaire. \code{FALSE} if the question is listed as a different type. \code{FALSE} if the question type could not be determined from the questionnaire.
#' @details To use this you must first successfully run \code{\link{load_questionnaire}}.
#' This does not derive the data type from any actual data; it only looks up the type defined in the questionnaire.
#' If type identification fails, the default return value is \code{FALSE}.This happens in the following cases:
#'\itemize{
#'   \item No questionnaire has been loaded with \code{\link{load_questionnaire}}
#'   \item \code{question.name} could not be found in the questionnaire, or in the data that was supplied to \code{\link{load_questionnaire}}
#' }
#'
#' @seealso  \code{\link{load_questionnaire}} must be run first.
#' Use \code{\link{question_type}} for the most generalised way to guess the data type.
#' Part of the \code{question_is_*} family of functions:
#' testing for specific types:
#' \code{\link{question_is_numeric}},
#' \code{\link{question_is_categorical}},
#' \code{\link{question_is_select_one}},
#' \code{\link{question_is_multiple}},
#' \code{\link{question_is_sm_choice}}
#'
#' parsing kobo skip-logic:
#' \code{\link{question_is_skipped}}
#' @export
#' @examples
#' question_is_select_multiple("some_numeric_kobo_xml_question_name") # FALSE
#' question_is_select_multiple("a_select_multiple_kobo_xml_question_name") # TRUE
#' question_is_select_multiple("a_select_one_kobo_xml_question_name") # FALSE
#' question_is_numeric("some_unidentified_string") # FALSE
#'@export
question_is_sm_choice<-function(question.name){
  question_is_sm_choice_internal(question.name)
}
#'@export
question_is_categorical<-function(question.name){
  question_is_categorical_internal(question.name) }
#'@export
is_questionnaire_loaded<-function(){
  is_questionnaire_loaded_internal()
}
#'@export
question_in_questionnaire <- function(question.name){
  question_in_questionnaire_internal(question.name)
}

#'@export
question_is_skipped<-function(data, variable.name){
  question_is_skipped_internal(data, variable.name)
}


question_get_choice_labels_internal<-function(responses,variable.name){
  stop("you must successfully run load_questionnaire() first")

}


question_get_question_label_internal<-function(variable.name){
  stop("you must successfully run load_questionnaire() first")
}

question_is_numeric_internal<-function(question.name){
  stop("you must successfully run load_questionnaire() first")
}


question_is_select_one_internal<-function(question.name){
  stop("you must successfully run load_questionnaire() first")

}
question_is_select_multiple_internal<-function(question.name){
  stop("you must successfully run load_questionnaire() first")

}

question_is_sm_choice_internal<-function(question.name){
  stop("you must successfully run load_questionnaire() first")

}

question_is_categorical_internal<-function(question.name){
  stop("you must successfully run load_questionnaire() first")
}

is_questionnaire_loaded_internal<-function(){return(FALSE)
}

question_in_questionnaire_internal <- function(question.name){return(FALSE)}

question_is_skipped_internal<-function(data, variable.name){
  warning("questionnaire not loaded. Assuming not skipped for all.")
  return(rep(FALSE,nrow(data)))
}
