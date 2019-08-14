# 'questionnaire' object is stored in the parent environment of a closure. CMD check doesn't seem to get it

# generic function to remove non-data from vectors
hasdata<-function (x, return.index = F) {
  index <- which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))
  value <- x[which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))]
  if (return.index) {
    return(index)
  }
  return(value)
}



#' load_questionnaire
#' @param data data frame containing the data matching the questionnaire to be loaded.
#' @param questions kobo form question sheet; either as a data frame, or a single character string with the name of a csv file
#' @param choices questions kobo form choices sheet; either as a data frame, or a single character string with the name of a csv file
#' @param choices.label.column.to.use The choices table has (sometimes multiple) columns with labels. They are often called "Label::English" or similar. Here you need to provide the _name of the column_ that you want to use for labels (see example!)
#' @return A list containing the original questionnaire questions and choices, the choices matched 1:1 with the data columns, and all functions created by this function relating to the specific questionnaire
#' @export
#' @examples
#'
load_questionnaire<-function(data,
                             questions,
                             choices,
                             choices.label.column.to.use=NULL){


  # generic function to replace values in a vector based on a lookup table
  replace_with_lookup_table<-function(x,y){
    x2 <- y[match(x, y[,1]),2]
    dim(x2) <- dim(x)
    x2
  }

  # load files
  if(is.vector(questions)){
    if(length(questions)==1){
      questions <- read.csv.auto.sep(questions,stringsAsFactors = F, header = T)
    }
  }else{if(!is.data.frame(questions)){stop("questions must either be a data frame or a csv file name")}}
  if(is.vector(choices)){
    if(length(choices)==1){
      choices <- read.csv.auto.sep(choices, stringsAsFactors = F, header = T)
    }
  }else{if(!is.data.frame(questions)){stop("choices must either be a data frame or a csv file name")}}

  # harmonise data column references
  names(questions) <- to_alphanumeric_lowercase(names(questions))
  names(choices) <- to_alphanumeric_lowercase(names(choices))
  choices$name<-trimws(as.character(choices$name))
  questions$relevant<-as.character(questions$relevant)

  names(data) <- to_alphanumeric_lowercase(names(data))


  # choices$name <- gsub("_", ".", choices$name) # UGANDA

  if(is.null(choices.label.column.to.use)){
    choices.label.column.to.use<-grep("label",names(choices),value = T)
    if(length(choices.label.column.to.use)==0){stop("No column in the choices file contains the word 'label', so you have to provide the exact name of the column to use as labels in the `choices.label.column.to.use` parameters.")}
    choices.label.column.to.use<-choices.label.column.to.use[1]
  }
  choices.label.column.to.use <- to_alphanumeric_lowercase(choices.label.column.to.use)

  # sanitise
  insure.string.is.column.header(questions, "type")
  insure.string.is.column.header(questions, "name")
  insure.string.is.column.header(choices, choices.label.column.to.use)
  insure.string.is.column.header(choices, "list_name")
  questions$name <- to_alphanumeric_lowercase(questions$name)


  begin_gr <- grep(paste(c("begin_group","begin group"), collapse = "|"), questions$type, ignore.case = T)
  end_gr <- grep(paste(c("end_group","end group"), collapse = "|"), questions$type, ignore.case = T)
  number_of_questions <- (length(questions$name) - length(begin_gr) - length(end_gr))

  questions$relevant<-add_group_conditions_to_question_conditions(questions)


  # remove empty choices

  choices<-choices[!(choices$list_name %in% c("", " ", NA)),]
  if(nrow(choices)<1){stop("no choice rows (with valid list_name value")}
  # make choices unique
  choices <- choices %>%
    split.data.frame(choices$list_name) %>%
    change_non_unique_choices(choices.label.column.to.use = choices.label.column.to.use) %>%
    do.call(rbind,.)


  # preserve questions data frame
  questions_as_df<-questions
  choices_as_df<-choices
  # get data column names
  data_colnames<-names(data)

  # this changes the questionnaire questions and choices to fit the data columns,
  # with empty entries for data columns that don't appear in the questionnaire.

  if((sum(!is.na(match(data_colnames, questions$name)))/number_of_questions) < 0.1) {
    warning("The question names (questionnaire) and data column names (data) don't seem to match (<10% of data columns recognised as questions). please make sure the two columns are harmonized")
  }

      questions <- questions[match(data_colnames, questions$name),]
        if(length(grep("^list[\\._]name$","list_name",value=T))<1){stop("kobo 'choices' sheet must have a column named 'list.name' or 'list_name'")}
        choices_per_data_column<-questions$type %>% as.character %>% strsplit(" ") %>% lapply(unlist)%>% lapply(function(x){

        x %>% lapply(function(y){
        # grep(y,choices[["list_name"]],value=F)
        # match (full word only)
        grep(paste0(" ",y," "),
             paste0(" ",
                    choices[[grep("^list[\\._]name$",names(choices),value=T)[1]]]
                    ," "),value=F,fixed = T)

      }
      ) %>% unlist
    }) %>% lapply(hasdata) %>% lapply(function(x){
      choices[x,]
    })
    names(choices_per_data_column)<- data_colnames



    # make functions that need questionnaire
    question_get_choices<- function(variable.name){
      variable.name<-as.character(variable.name)
      variable.name<-to_alphanumeric_lowercase(variable.name)

      choices_per_data_column[[variable.name]][["name"]]

    }



   question_get_choice_labels <- function(responses,variable.name){

     variable.name<-as.character(variable.name)
     variable.name<-to_alphanumeric_lowercase(variable.name)

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
     variable.names<-to_alphanumeric_lowercase(variable.names)
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
      question.name<-to_alphanumeric_lowercase(question.name)
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      question.name<-to_alphanumeric_lowercase(question.name)

      qid<-which(questions$name==question.name)
      if(length(qid)==0){return(FALSE)}
      if(length(c(grep("integer",questions$type[qid]),grep("decimal", questions$typep[qid]), grep("calculate", questions$type[qid])))>0){return(TRUE)}
      return(FALSE)
    }


    question_is_select_one <- function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      question.name<-to_alphanumeric_lowercase(question.name)

      if(!(question.name %in% questions$name)){return(FALSE)}
      qid<-which(questions$name==question.name)
      if(length(grep("select_one",questions$type[qid]))>0){return(TRUE)}
      return(FALSE)
    }

    question_is_select_multiple <- function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      question.name<-to_alphanumeric_lowercase(question.name)

      if(!(question.name %in% questions$name)){return(FALSE)}
      qid<-which(questions$name==question.name)
      if(length(grep("select_multiple",questions$type[qid]))>0){return(TRUE)}
      return(FALSE)
    }

    question_is_categorical <- function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}
      question.name<-to_alphanumeric_lowercase(question.name)

      return(question_is_select_one(question.name) | question_is_select_multiple(question.name))
    }

    question_in_questionnaire <- function(question.name){
      if(is.null(question.name)){stop("question.name can not be NULL")}
      if(question.name %in% c(NA,"","N/A","NA")){stop("question.name can not be empty")}
      if(length(question.name)!=1){stop("question_in_questionnaire() takes only a single variable name as input")}
      question.name<-to_alphanumeric_lowercase(question.name)

      if(sum(question.name %in% questions$name) > 0){
        return(TRUE)}
      return(FALSE)
    }

    question_is_skipped <- function(data, question.name){
      question.name<-to_alphanumeric_lowercase(question.name)
      qid<-which(as.character(questions$name)==as.character(question.name))
      condition<-questions$relevant[qid[1]]
      question_is_skipped_apply_condition_to_data(data,condition)
    }

    questionnaire_is_loaded <- TRUE

    is_questionnaire_loaded<-function(){return(TRUE)}


    question_is_sm_choice<-function(question.name){
      if(is.null(question.name)){return(FALSE)}
      if(is.na(question.name)){return(FALSE)}
      if(question.name==""){return(FALSE)}

      sm_cols<-purrr::map2(names(choices_per_data_column),
                           choices_per_data_column,function(x,y){
                             if(length(y$name)>0){
                               paste(x,y$name,sep=".")}else{NULL}
                           }) %>% unlist %>% to_alphanumeric_lowercase

      return(to_alphanumeric_lowercase(question.name) %in% sm_cols)

    }

    as.data.frames<-function(){
      return(list(questions=questions_as_df,choices = choices_as_df))
    }
    # CRAN doesn't accept the closure...:

    # update.internal.package.function<-function(new.fun.name,
    #                                            internal_fun_name_prefix="",
    #                                            internal_fun_name_suffix="_internal",
    #                                            package="koboquest"){
    #   internal.function.name<-paste0(internal_fun_name_prefix,new.  un.name,internal_fun_name_suffix)
    #   assignInNamespace(x = internal.function.name, ns=package, value= get(new.fun.name))
    # }


    # sapply(c("question_get_choice_labels",
    # "question_get_question_label",
    # "question_is_numeric",
    # "question_is_select_one",
    # "question_is_select_multiple",
    # "question_is_categorical",
    # "question_in_questionnaire",
    # "question_is_skipped",
    # "is_questionnaire_loaded",
    # "question_is_sm_choice"),update.internal.package.function)



    question_type_from_questionnaire <- function(variables){
      variable_types <- as.vector(sapply(variables, function(x){
        # if(question_is_categorical(x)){return("categorical")}
        if(question_is_select_multiple(x)){return("select_multiple")}
        if(question_is_select_one(x)){return("select_one")}
        if(question_is_numeric(x)){return("numeric")}
        # none of the above? then we can't help you either
        return(NA)
      })
      )
      return(variable_types)
    }


    # documented in external default
    # moved inside here to avoid dependence on global closure with 'assignInNamespace'

    question_type<-function(variable.name,data=NULL,from.questionnaire=T,from.data=T){
      if(is.null(variable.name)){stop("variable name can not be NULL")}
      if(is.na(variable.name)){stop("variable name can not be NA")}
      variable.name<-to_alphanumeric_lowercase(as.character(variable.name))
      if(!from.questionnaire & !from.data){stop("at least one of 'from.questionnaire' or 'from.data' parameters must be 'TRUE'.")}
      if(is.null(data) & from.data & !from.questionnaire){
        stop("to infer data types from data, provide 'data' parameter. alternatively, use 'from.questionnaire=TRUE'.")

      }

      if(from.questionnaire & !from.data & !is_questionnaire_loaded()){
        stop("to infer data type from questionnaire, successfully run load_questionnaire(...) first.")
      }
      ### try to determine question type from questionnaire:
      if(from.questionnaire & is_questionnaire_loaded()){
        if(question_in_questionnaire(variable.name)){
          type.from.q<-question_type_from_questionnaire(variable.name)
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
      names(data)<-to_alphanumeric_lowercase(names(data))
      if(!all(variable.name%in%names(data))){stop("Can not determine the data type: it's neither in the questionnaire nor in the data column headers")}
      if(is.numeric(data[[variable.name]])){return("numeric")}
      return("select_one")

    }



    choices_for_select_multiple <- function(question_name, data){
      question_name<-as.character(question_name)
      choices<-choices_per_data_column[[question_name]]
      select_mult_colnames<-paste(question_name,choices$name,sep=".") %>% to_alphanumeric_lowercase
      indices<-match(as.character(select_mult_colnames),names(data))
      if(any(is.na(indices))){stop(paste("can not find TRUE/FALSE columns for variable",question_name,". Please double check that they all exist in the data and that their names are in the standard kobo format of \"[question name].[choice name]\""))}
      return(indices)
    }



    return(c(question_type=question_type,
             question_is_numeric=question_is_numeric,
             question_is_select_one=question_is_select_one,
             question_is_select_multiple=question_is_select_multiple,
             question_is_categorical=question_is_categorical,
             question_in_questionnaire=question_in_questionnaire,
             question_is_skipped=question_is_skipped,
             is_questionnaire_loaded=is_questionnaire_loaded,
             question_is_sm_choice=question_is_sm_choice,
             choices_for_select_multiple=choices_for_select_multiple,
             question_get_choices= question_get_choices,
             question_get_choice_labels=question_get_choice_labels,
             question_get_question_label=question_get_question_label,
             raw_questionnaire=as.data.frames))


    }







# skiplogic can apply to a whole group, so we need to (recursively) attach a group's condition to each individual questions condition when loading the questionnaire
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
      if(is.na(condition_that_only_applies_to_this_question)){
        condition_that_only_applies_to_this_question<-""
      }
    }

    all_condition_for_this_q<-c(group_conditions,condition_that_only_applies_to_this_question)
    all_condition_for_this_q[is.na(all_condition_for_this_q)]<-""
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



#' question_type
#'
#' Determines the kobo question type for a given variable name
#'
#' @param variable.name the kobo question name for which the type should be determined. (works on a vector with multiple names)
#' @param data the dataset matching the kobo questionnaire. This can be left empty if \code{from.data=F}.
#' A provided dataset can be used as a fallback with \code{from.data=T} in case the data type can not be determined from the questionnaire.
#' @param from.questionnaire if FALSE, prevent determinining data type from questionnaire. Can not be FALSE if from.data is also FALSE.
#' @param from.data if TRUE, allows to determine data type from provided \code{data}. Can not be FALSE if from.data is also FALSE. If both from.questionnaire and from.data are TRUE, data types determined from the questionnaire have precedence.
#' @return a string naming the question type. One of "select_one", "select_multiple" or "numeric".
#' @seealso Should be used after \code{\link{load_questionnaire}}, but can work without if data is provided as a fallback.
#' @export
#' @examples
#' question_type("question_name_in_loaded_questionnaire")
question_type<-function(variable.name,data=NULL,from.questionnaire=T,from.data=T){

  stop("run load_questionnaire, which will return a list of functions including 'question_type()' as an object - use that one! ")
}




