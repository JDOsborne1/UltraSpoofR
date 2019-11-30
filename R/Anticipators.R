#### Anticipator Functions ####

#' Anticipator Dictionary function
#'
#' Returns the anticipator generator function based on the data type given
#'
#' @param type A data type supported by the dict. one of c("CSV", "RDS)
#'
#' @return
#' @export
#'
#' @examples
ultraAnticipatorDict <- function(type){
  if(type == "CSV"){
    return(ultraCSV)
  } else if(type == "RDS") {
    return(ultraRDS)
  } else if(type == "ExcelRemote") {
    return(ultraExcelRemote)
  } else {
    return(NULL)
  }
}

#' CSV anticipator generator
#'
#' Generates an anticipator for the .csv file at the given path
#'
#' @param path The path of the csv file
#'
#' @return
#' @export
#'
#' @examples
ultraCSV <- function(path){
  anticipator <- function(){
    return(utils::read.csv(path))
  }
  return(anticipator)
}

#' RDS anticipator generator
#'
#' Generates an anticipator for the .rds file at the given path
#'
#' @param path The path of the RDS file
#'
#' @return
#' @export
#'
#' @examples
ultraRDS <- function(path){
  anticipator <- function(){
    return(base::readRDS(path))
  }
}

#' Remote Excel anticipator generator
#'
#' Generates an anticipator for the .rds file at the given path
#'
#' @param path The path of the Excel file
#'
#' @return
#' @export
#'
#' @examples
ultraExcelRemote <- function(path){
  anticipator <- function(){
    httr::GET(path, httr::write_disk(tf <- tempfile(fileext = ".xls")))
    return(readxl::read_excel(tf))
  }
}


