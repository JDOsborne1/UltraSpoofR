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
    #, spoof = NULL
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
    calc_values <- c("Min", "Max", "Levels")
    out_dict <- head(
      data.frame(
        "Column" = NA
        , "Description" = NA
        , "Type" = NA
        , "Min" = NA
        , "Max" = NA
        , "Levels" = NA
      )
      , 0
    )

    needed_rownames <- colnames(ultra_df$origin())
    for(i in needed_rownames) {
      desc <- readline(prompt = paste0("what is the Description for ", i, ": " ))

      type <- guessDataType(ultra_df$origin()[, i])

      if(type != "Value"){
        Max <- NA
      } else {
        Max <- max(ultra_df$origin()[, i])
      }

      if(type != "Value"){
        Min <- NA
      } else {
        Min <- min(ultra_df$origin()[, i])
      }

      if(!type %in% c("Category", "Tag")){
        Levels <- NA
      } else {
        Levels <- paste(unique(ultra_df$origin()[, i]), collapse = "|")
      }

      meta <- data.frame(
        "Column" = i
        , "Description" = desc
        , "Type" = type
        , "Min" = Min
        , "Max" = Max
        , "Levels" = Levels
      )


      out_dict <- rbind(out_dict, meta)
    }

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
  ultra_df[["spoof"]] <- ultraRandoDataset(ultra_df)
  return(ultra_df)
}
