#### Functions to generate the UltraSpoof data structure from existing dataset ####

#' Wrapper
#'
#' The main function to wrap up dataframes into the ultrawrapper format
#'
#' @param origin the path of the source file
#' @param type The type of file found at the path
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

#' Injecting a metadata structure into the ultra_df
#'
#' Populates the data structure with the metadata of the underlying origin data
#'
#' @param ultra_df the ultra df object
#' @param dict The metadat dictionary
#'
#' @return
#' @export
#'
#' @examples
ultraMeta <- function(ultra_df, dict = NULL){
  if(is.null(dict)){
  ultra_df[["meta"]] <- ultraMetaGenerator(ultra_df$origin())
  } else {
    ultra_df[["meta"]] <- dict
  }
  return(ultra_df)
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
  # ultra_df[["spoof"]] <- ultraRandoDataset(ultra_df)
  ultra_df[["spoof"]] <- ultraSpooferNew(ultra_df)
  return(ultra_df)
}
