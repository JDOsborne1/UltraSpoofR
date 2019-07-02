#### Functions to generate the UltraSpoof data structure from existing dataset ####

#' Wrapper
#'
#' The main function to wrap up dataframes into the ultrawrapper format
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
ultraWrapper <- function(origin, type){
  wrappingFunc <- ultraAnticipatorDict(type)
  ultra_df <- list(
    origin = wrappingFunc(origin)
    , spoof = NULL
    , meta = NULL
    )
}

#' Injecting a metadata structure into the ultra_df
#'
#' Populates the data structure with the metadata of the underlying origin data
#'
#' @param ultra_df
#'
#' @return
#' @export
#'
#' @examples
ultraMetaInjector <- function(ultra_df){

}

#' Spoofer
#'
#' Function to inject the spoofed data into the data structure
#'
#' @param ultra_df
#'
#' @return
#' @export
#'
#' @examples
ultraSpoof <- function(ultra_df){

}
