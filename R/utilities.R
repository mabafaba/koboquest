to_alphanumeric_lowercase <- function(x){tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))}
to_alphanumeric_lowercase_colnames_df <- function(df){
  names(df) <- to_alphanumeric_lowercase(names(df))
  return(df)
}

# read csv files independent from csv type; default stringsAsFactors to false
read.csv.auto.sep<-function(file,stringsAsFactors=F,...){
  df<-fread(file,stringsAsFactors=stringsAsFactors,...) %>% as.data.frame
  df<-to_alphanumeric_lowercase_colnames_df(df)
  return(df)
}


# generic function to remove "non-data" from vectors
hasdata<-function (x, return.index = F) {
  index <- which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))
  value <- x[which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))]
  if (return.index) {
    return(index)
  }
  return(value)
}
