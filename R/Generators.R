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
    #, meta = NULL
    )
}

#' Injecting a metadata structure into the ultra_df
#'
#' Populates the data structure with the metadata of the underlying origin data
#'
#' @param ultra_df
#' @param dict
#'
#' @return
#' @export
#'
#' @examples
ultraMetaGenerator <- function(ultra_df, dict = NULL){
  if(is.null(dict)){
    # --TODO move into seperate functionality
    metas <- c("Description", "Type", "Min", "Max", "Levels")
    out_dict <- data.frame("placeholder" = metas,  row.names = metas)
    needed_colnames <- colnames(ultra_df$origin())
    calc_values <- c("Type", "Min", "Max", "Levels")
    for(i in needed_colnames){
      meta <- c()
      for(j in metas){
        if(!j %in% calc_values){
          meta_val <- readline(prompt = paste0("what is the ", j, " for ", i, ": " ))
        } else if(j == "Type") {
          meta_val <- guessDataType(ultra_df$origin()[, i])
        } else if(j == "Min"){
          if(meta[2] != "Value"){
            meta_val <- NA
          } else {
            meta_val <- min(ultra_df$origin()[, i])
          }
        } else if(j == "Max"){
          if(meta[2] != "Value"){
            meta_val <- NA
          } else {
            meta_val <- max(ultra_df$origin()[, i])
          }
        } else if(j == "Levels"){
          if(!meta[2] %in% c("Category", "Tag")){
            meta_val <- NA
          } else {
            meta_val <- paste(unique(ultra_df$origin()[, i]), collapse = "|")
          }
        }
        meta <- c(meta, meta_val)
      }
      new_col <- data.frame(name = meta)
      out_dict <- cbind(out_dict, new_col)
    }
    out_dict$placeholder <- NULL
    colnames(out_dict) <- needed_colnames


  } else {
    out_dict <- dict
  }
  ultra_df[["meta"]] <- out_dict
  return(ultra_df)
}



#' Spoofer
#'
#' Function to inject the spoofed data into the data structure, cloning the metadata of the spoofed object
#'
#' @param ultra_df
#'
#' @return
#' @export
#'
#' @examples
ultraSpoof <- function(ultra_df){

}
