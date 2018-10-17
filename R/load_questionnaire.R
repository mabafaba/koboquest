
# questionnaire_is_loaded <- FALSE
#

#' load_questionnaire
#' @param data data frame containing the data matching the questionnaire to be loaded.
#' @param questions.file file name of a csv file containing the kobo form's question sheet
#' @param choices.file file name of a csv file containing the kobo form's choices sheet
#' @return list of questions and choices, sorted to match. to data columns.
#' @seealso \code{\link{load_data()}, \link{load_samplingframe}}
#' @export
#' @examples
#'
load_questionnaire<-function(data,
                             questions.file,
                             choices.file,
                             choices.label.column.to.use=NULL){
  # generic function to remove non-data from vectors
  hasdata<-function (x, return.index = F) {
    index <- which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))
    value <- x[which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))]
    if (return.index) {
      return(index)
    }
    return(value)
  }

  # generic function to replace values in a vector based on a lookup table
  replace_with_lookup_table<-function(x,y){
    x2 <- y[match(x, y[,1]),2]
    dim(x2) <- dim(x)
    x2
  }

  # load files
  questions <- read.csv.auto.sep(questions.file,stringsAsFactors = F, header = T)
  choices <- read.csv(choices.file, stringsAsFactors = F, header = T)

  # harmonise data column references
  names(questions) <- to_alphanumeric_lowercase(names(questions))
  names(choices) <- to_alphanumeric_lowercase(names(choices))
  names(data) <- to_alphanumeric_lowercase(names(data))


  # choices$name <- gsub("_", ".", choices$name) # UGANDA


  choices.label.column.to.use <- to_alphanumeric_lowercase(choices.label.column.to.use)

  # sanitise
  insure.string.is.column.header(questions, "type")
  insure.string.is.column.header(questions, "name")
  insure.string.is.column.header(choices, choices.label.column.to.use)
  insure.string.is.column.header(choices, "list_name")
  questions$name <- to_alphanumeric_lowercase(questions$name)


  # # ####UGA ONLY
  # questions$name <- gsub("_", ".", questions$name)
  # choices$name <- gsub("_", ".", choices$name)
  # names(data) <- gsub("_", ".", names(data))
  # questions$relevant <- gsub("_", ".", questions$relevant) %>% to_alphanumeric_lowercase

  # # UGANDA

  begin_gr <- grep(paste(c("begin_group","begin group"), collapse = "|"), questions$type, ignore.case = T)
  end_gr <- grep(paste(c("end_group","end group"), collapse = "|"), questions$type, ignore.case = T)
  number_of_questions <- (length(questions$name) - length(begin_gr) - length(end_gr))

  questions$relevant<-add_group_conditions_to_question_conditions(questions)
  # get data column names
  data_colnames<-names(data)

  # this changes the questionnaire questions and choices to fit the data columns,
  # with empty entries for data columns that don't appear in the questionnaire.
  if((sum(!is.na(match(data_colnames, questions$name)))/number_of_questions) < 0.1) {
    stop("The question names (questionnaire) and data column names (data) don't seem to match. please make sure the two columns are harmonized")
  }

      questions <- questions[match(data_colnames, questions$name),]

        choices_per_data_column<-questions$type %>% as.character %>% strsplit(" ") %>% lapply(unlist)%>% lapply(function(x){

        x %>% lapply(function(y){
        # grep(y,choices[["list_name"]],value=F)
        # match (full word only)
        grep(paste0(" ",y," "),paste0(" ",choices[["list.name"]]," "),value=F,fixed = T)

      }
      ) %>% unlist
    }) %>% lapply(hasdata) %>% lapply(function(x){
      choices[x,]
    })
    names(choices_per_data_column)<- data_colnames


    # make functions that need questionnaire

   question_get_choice_labels <- function(responses,variable.name){

     variable.name<-as.character(variable.name)
     responses<-as.character(responses)
      if(question_is_categorical(variable.name)){
      labels<-replace_with_lookup_table(
        as.character(responses),
        # MAKE LABEL COLUMN A PARAMETER!!!
        cbind(as.character(choices_per_data_column[[variable.name]]$name),as.character(choices_per_data_column[[variable.name]][,choices.label.column.to.use]))
      )
      # fix those that were not found to go back to original NA
      labels[is.na(labels)]<-responses[is.na(labels)]
      return(labels)

      }
     return(responses)
   }

   question_get_question_label<-function(variable.names){
     variable.names<-as.character(variable.names)

     labelcol<-grep("label",names(questions))[1]
     questionnaire_rows<-match(variable.names,questions[,"name"])
     questionnaire_rows[is.na(variable.names)]<-NA
     labels<-questionnaire_rows
     labels[!is.na(questionnaire_rows)]<-questions[questionnaire_rows[!is.na(questionnaire_rows)],labelcol]
     labels[is.na(labels)]<-variable.names[is.na(labels)]

     labels[gsub("[[:space:]]", "", labels)==""| is.na(variable.names)]<-variable.names[gsub("[[:space:]]", "", labels)==""| is.na(variable.names)]
     return(labels)
      }



    question_is_numeric <- function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      qid<-which(questions$name==question.name)
      if(length(qid)==0){return(FALSE)}
      if(length(c(grep("integer",questions$type[qid]),grep("decimal", questions$typep[qid]), grep("calculate", questions$type[qid])))>0){return(TRUE)}
      return(FALSE)
    }


    question_is_select_one <- function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      if(!(question.name %in% questions$name)){return(FALSE)}
      qid<-which(questions$name==question.name)
      if(length(grep("select_one",questions$type[qid]))>0){return(TRUE)}
      return(FALSE)
    }

    question_is_select_multiple <- function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      if(!(question.name %in% questions$name)){return(FALSE)}
      qid<-which(questions$name==question.name)
      if(length(grep("select_multiple",questions$type[qid]))>0){return(TRUE)}
      return(FALSE)
    }

    question_is_categorical <- function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      return(question_is_select_one(question.name) | question_is_select_multiple(question.name))
    }

    question_in_questionnaire <- function(question.name){
      if(sum(question.name %in% questions$name) > 0){
        return(TRUE)}
      return(FALSE)
    }

    question_is_skipped <- function(data, question.name){
      qid<-which(questions$name==question.name)
      condition<-questions$relevant[qid[1]]
      question_is_skipped_apply_condition_to_data(data,condition)
    }

    questionnaire_is_loaded <- TRUE

    is_questionnaire_loaded<-function(){return(TRUE)}


    question_is_sm_choice<-function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}

      var.name.split<-strsplit(question.name,"\\.")
      question.name.sans.choice<-paste0(var.name.split[[1]][-length(var.name.split[[1]])],collapse=".")
      choice.name<-var.name.split[[1]][length(var.name.split[[1]])]

      if(!question_is_select_multiple(question.name.sans.choice)){return(FALSE)}

      if(choice.name %in% choices_per_data_column[[question.name.sans.choice]]$name){
        return(TRUE)
      }
      return(FALSE)
    }



    update.internal.package.function<-function(new.fun.name,
                                               internal_fun_name_prefix="",
                                               internal_fun_name_suffix="_internal",
                                               package="koboquest"){
      assignInNamespace(x = paste0(internal_fun_name_prefix,new.fun.name,internal_fun_name_suffix), ns=package, new.fun.name)
    }


    sapply(c("question_get_choice_labels",
    "question_get_question_label",
    "question_is_numeric",
    "question_is_select_one",
    "question_is_select_multiple",
    "question_is_categorical",
    "question_in_questionnaire",
    "question_is_skipped",
    "is_questionnaire_loaded",
    "question_is_sm_choice"),update.internal.package.function)

    return(c(list(questions=questions,choices=choices,choices_per_variable=choices_per_data_column), list(data),
             question_get_choice_labels=question_get_choice_labels,
             question_get_question_label=question_get_question_label,
             question_is_numeric=question_is_numeric,
             question_is_select_one=question_is_select_one,
             question_is_select_multiple=question_is_select_multiple,
             question_is_categorical=question_is_categorical,
             question_in_questionnaire=question_in_questionnaire,
             question_is_skipped=question_is_skipped,
             is_questionnaire_loaded=is_questionnaire_loaded,
             question_is_sm_choic=question_is_sm_choice))

    }


#' variable_type
#'
#' @param variables the vector or value for which the type should be determined
#' @return a vector or value with variable types
#' @seealso
#' @examples
#'
question_type_from_questionnaire <- function(variables){
    variable_types <- as.vector(sapply(variables, function(x){
      # if(question_is_categorical(x)){return("categorical")}
      if(question_is_select_multiple(x)){return("select_multiple")}
      if(question_is_select_one(x)){return("select_one")}
      if(question_is_numeric(x)){return("numeric")}

      return(NA)
      })
    )
    return(variable_types)
    }


#' question_data_type
#'
#' @param variables the vector or value for which the type should be determined
#' @return a vector or value with variable types
#' @seealso
#' @export
#' @examples
#'
question_type<-function(variable.name,data=NULL,from.questionnaire=T,from.data=T){
  if(!from.questionnaire & !from.data){stop("at least one of 'from.questionnaire' or 'from.data' parameters must be 'TRUE'.")}
    if(!is.null(data) & from.data & !from.questionnaire){
      stop("to infer data types from data, provide 'data' parameter. alternatively, use 'from.questionnaire'.")
    }
  if(from.questionnaire & !from.data & !is_questionnaire_loaded()){
    stop("to infer data from questionnaire, successfully run load_questionnaire(...) first.")
  }
    ### try to determine question type from questionnaire:
  if(from.questionnaire & is_questionnaire_loaded()){
      if(question_in_questionnaire(variable.name)){
        type.from.q<-question_variable_type_from_questionnaire(variable.name)
        # if succesfull, we're done here:
        if(!is.na(type.from.q)){return(type.from.q)}
        }
  }
    # if from questionnaire failed but from.data=F, we shouldn't be here:
    if(from.questionnaire & !from.data){
      stop(paste(variable.name), "'s type could not be determined from questionnaire.
           provide 'data' parameter and set from.data to 'TRUE' to infer the type from the data.")
      }
    # Guess from data:

    if(!all(variable.name%in%names(data))){stop("Can not determine the data type: it's neither in the questionnaire nor in the data column headers")}
    if(is.numeric(data[[variable.name]])){return("numeric")}
    if(is.numeric.fuzzy(data[[variable.name]], 0.9)){return("numeric")}
    return("select_one")

  }

add_group_conditions_to_question_conditions<-function(questions){
  group_conditions<-NULL
  conditions<-c()

  begin_gr <- grep(paste(c("begin_group","begin group"), collapse = "|"), questions$type, ignore.case = T)
  end_gr <- grep(paste(c("end_group","end group"), collapse = "|"), questions$type, ignore.case = T)


  for(i in 1:nrow(questions)){
    is_group_start<-(i %in% as.numeric(begin_gr))
    is_group_end<-(i %in% as.numeric(end_gr))

    if(is_group_start){

      group_conditions<-c(group_conditions,questions$relevant[i])
      condition_that_only_applies_to_this_question<-NULL
    }
    if(is_group_end){

      group_conditions<-group_conditions[-length(group_conditions)]
      condition_that_only_applies_to_this_question<-NULL  }
    if(!is_group_end & !is_group_start){
      condition_that_only_applies_to_this_question<-questions$relevant[i]
    }

    all_condition_for_this_q<-c(group_conditions,condition_that_only_applies_to_this_question)

    if(all(all_condition_for_this_q=="")){
        all_condition_for_this_q<-""
      }else{
        all_condition_for_this_q<-paste("(",all_condition_for_this_q[all_condition_for_this_q!=""],")")
      }

    all_condition_for_this_q_combined<-paste(all_condition_for_this_q,collapse=" and ")
    conditions<-c(conditions,all_condition_for_this_q_combined)
  }
  conditions
}

read.csv.auto.sep<-function(file,stringsAsFactors=F,...){
  df<-fread(file,stringsAsFactors=stringsAsFactors,...) %>% as.data.frame
  df<-to_alphanumeric_lowercase_colnames_df(df)
  return(df)
}

