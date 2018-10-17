#'@export
question_get_choice_labels<-function(responses,variable.name){
  koboquest::question_get_choice_labels_internal(responses,variable.name)
}
#'@export
question_get_question_label<-function(variable.name){
  koboquest::question_get_question_label_internal(variable.name) }
#'@export
question_is_numeric<-function(question.name){
  koboquest::question_is_numeric_internal(question.name) }

#'@export
question_is_select_one<-function(question.name){
  koboquest::question_is_select_one_internal(question.name)
}
#'@export
question_is_select_multiple<-function(question.name){
  koboquest::question_is_select_multiple_internal(question.name)
}
#'@export
question_is_sm_choice<-function(question.name){
  koboquest::question_is_sm_choice_internal(question.name)
}
#'@export
question_is_categorical<-function(question.name){
  koboquest::question_is_categorical_internal(question.name) }
#'@export
is_questionnaire_loaded<-function(){
  koboquest::is_questionnaire_loaded_internal()
}
#'@export
question_in_questionnaire <- function(question.name){
  koboquest::question_in_questionnaire_internal(question.name)
}
#'@export
question_is_skipped<-function(data, variable.name){
  koboquest::question_is_skipped_internal(data, variable.name)
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
  stop("you must successfully run load_questionnaire() first")
}
