
question_get_choice_labels<-function(responses,variable.name){
  question_get_choice_labels_internal(responses,variable.name)  
}

question_get_question_label<-function(variable.name){
  question_get_question_label_internal(variable.name) }

question_is_numeric<-function(question.name){
  question_is_numeric_internal(question.name) }


question_is_select_one<-function(question.name){
  question_is_select_one_internal(question.name)   
}
question_is_select_multiple<-function(question.name){
  question_is_select_multiple_internal(question.name)   
}

question_is_sm_choice<-function(question.name){
  question_is_sm_choice_internal(question.name)   
}
question_is_categorical<-function(question.name){
  question_is_categorical_internal(question.name) }

is_questionnaire_loaded<-function(){
  is_questionnaire_loaded_internal() 
}

question_in_questionnaire <- function(question.name){
  question_in_questionnaire_internal(question.name) 
}

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

question_in_questionnaire <- function(question.name){return(FALSE)}

question_is_skipped_internal<-function(data, variable.name){
  stop("you must successfully run load_questionnaire() first")
}
