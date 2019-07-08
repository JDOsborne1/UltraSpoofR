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
