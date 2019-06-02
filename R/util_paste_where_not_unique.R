

#' takes a list of kobo choice data frames (one per question), and attach the name to the label if the label is not unique within a question
change_non_unique_choices<-function(choice_list_by_question,choices.label.column.to.use){
  assertthat::assert_that(is.list(choice_list_by_question))
  assertthat::assert_that(assertthat::is.string(choices.label.column.to.use))

  choices_labels_made_unique <-
    choice_list_by_question %>%
    lapply(function(x){
      if(is.null(x)){return(NULL)}
      if(!is.data.frame(x)){return(x)}
      if(nrow(x)<2){return(x)}
      if(!("list_name" %in% names(x))){return(x)}
      if(!(choices.label.column.to.use %in% names(x))){return(x)}


      x[[choices.label.column.to.use]]<-as.character(x[[choices.label.column.to.use]])
      x[["name"]]<-as.character(x[["name"]])

      # copy
      unique_choices <- x
      # add name to label where label not unique
      unique_choices[[choices.label.column.to.use]] <- paste0_where_not_unique(unique_choices[[choices.label.column.to.use]],
                                                                               after = paste0(" (", unique_choices[["name"]], ")"
                                                                               )
      )
      # throw warning if labels changed:
      changed_label_row_ids<-which(x[[choices.label.column.to.use]]!=unique_choices[[choices.label.column.to.use]])
      changed_labels_new <- unique_choices[[choices.label.column.to.use]][changed_label_row_ids]
      changed_labels_old <- x[[choices.label.column.to.use]][changed_label_row_ids]
      if(length(changed_label_row_ids)!=0){

        message(paste("changed non unique labels to:\n",paste0(paste0("'",changed_labels_old ,"==>", changed_labels_new, "'"),collapse = "\n")))
      }


      unique_choices
    })


}





paste0_where_not_unique<-function(x,before=NULL,after=NULL,warning = F){

  x<-as.character(x)
  before<-as.character(before)
  after<-as.character(after)

  assertthat::assert_that(is.vector(x))
  assertthat::assert_that(is.vector(before))
  assertthat::assert_that(is.vector(after))



  if(length(before)==0){
    before<-rep("",length(x))
  }

  if(length(after)==0){
    after<-rep("",length(x))
  }

  if(length(x)!=length(before) | length(x)!=length(after)){
    stop("'x', 'before' and 'after' values must all have the same length")
  }
  # count number of occurances for all values in x
  counts<-table(x) %>% as.data.frame
  # if they're all unique, return input unchanged
  if(all(counts$Freq<=1)){return(x)}

  # otherwise pick out values that appear > 1 time
  addtovalues <- counts$x[counts$Freq > 1]
  # what is their index in the input vector x?
  addtoindex <- which(x %in% addtovalues)

  # create new values

  new_values <- paste0(before[addtoindex],
         x[addtoindex],
         after[addtoindex])

  # those that didn't become unique now.. change them back:
  if(any(!is.unique(new_values))){
    new_values[!is.unique(new_values)]<-x[addtoindex][!is.unique(new_values)]
    warning("there are different choices with the same list_name, name and label; these are indistinguishable and should be removed")
  }

  # paste to those



  x[addtoindex]<-new_values
  x
}


is.unique<-function(x){
  x<-as.character(x)
  tb<-table(x) %>% as.data.frame(stringsAsFactors = F)
  unique_values<-tb[tb$Freq==1,"x"]
  is_unique<-x %in% unique_values
  is_unique
}
