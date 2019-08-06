# Self referential methods
#' Call Staged Output
#'
#' @description Calls the stages attribute of the ultra_df to generate a multi step origin call, allowing for extra steps to be used after the source
#'
#' @param stages
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
#' @param ultra_df
#' @param column
#'
#' @return
#' @export
#'
#' @examples
ultraGetMax <- function(ultra_df, column){
  return(ultra_df$meta[ultra_df$meta$Column == column, ]$Max)
}

#' Maximum value getter
#'
#' @param ultra_df
#' @param column
#'
#' @return
#' @export
#'
#' @examples
ultraGetMin <- function(ultra_df, column){
  return(ultra_df$meta[ultra_df$meta$Column == column, ]$Min)
}
