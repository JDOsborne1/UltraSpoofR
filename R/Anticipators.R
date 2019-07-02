#### Anticipator Functions ####

ultraAnticipatorDict <- function(type){
  if(type == "CSV"){
    return(ultraCSV)
  } else {
    return(NULL)
  }
}

ultraCSV <- function(path){
  anticipator <- function(){
    read.csv(path)
  }
  return(anticipator)
}
