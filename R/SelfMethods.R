# Self referential methods
s
#' Call Staged Output
#'
#' @description Calls the stages attribute of the ultra_df to generate a multi step origin call, allowing for extra steps to be used after the source
#'
#' @param stages the stages of the ultra df analysis
#'
#' @return
#' @export
#'
#' @examples
ultraCallStagedOutput <- function(stages){
  output <- stages[["source"]]()
  non_origin_stages <- stages[names(stages) != "source"]
  for(i in non_origin_stages){
    output <- i(output)
  }
  return(output)
}

#' Maximum value getter
#'
#' @param ultra_df the ultra df
#' @param column the column to query
#'
#' @return
#' @export
#'
#' @examples
ultraGetMax <- function(ultra_df, column){
  return(ultra_df$meta[ultra_df$meta$Column == column, ]$Max)
}

#' Minimum value getter
#'
#' @param ultra_df the ultra df
#' @param column the column to query
#'
#' @return
#' @export
#'
#' @examples
ultraGetMin <- function(ultra_df, column){
  return(ultra_df$meta[ultra_df$meta$Column == column, ]$Min)
}
