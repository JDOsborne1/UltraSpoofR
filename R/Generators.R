#### Functions to generate the UltraSpoof data structure from existing dataset ####

#' Wrapper
#'
#' The main function to wrap up dataframes into the ultrawrapper format
#'
#' @param type The type of file found at the path
#' @param origin_location the path of the source file
#'
#' @return
#' @export
#'
#' @examples
ultraWrapper <- function(origin_location, type){
  wrappingFunc <- ultraAnticipatorDict(type)
  origin_func = wrappingFunc(origin_location)
  ultra_df <- list(
    origin = origin_func
    , stages = list("source" = origin_func)
    #, spoof = NULL
    #, meta = NULL
    )

}



#' Spoofer
#'
#' Function to inject the spoofed data into the data structure, cloning the metadata of the spoofed object
#'
#' @param ultra_df The ultra df
#'
#' @return
#' @export
#'
#' @examples
ultraSpoof <- function(ultra_df){
  ultra_df[["spoof"]] <- ultraSpooferNew(ultra_df)
  return(ultra_df)
}
