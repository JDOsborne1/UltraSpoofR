# Functions for determining the metadata of a column automatically

#distinctiveness <- function(vect){
#        stage1 <- normalise(vect)
#        # would need to sort the vector
#        stage2 <- sort(stage1)
#        # want to jitter the values to prevent the log-cost from exploding
#        stage3 <- jitter(stage2)
#        # want the differences between the values
#        stage4 <- diff(stage3)
#        # Want to apply a log-cost to the values in the vector This is not currently scaled for duplicates
#        stage5 <- log1p(stage4)
#        # Then combining the vector of costed differences to produce a single metric
#        stage6 <- sum(stage5)
#        return(stage6)
#}
#
#normalise <- function(vect){
#        return(vect/max(vect, na.rm=T))
#}

#' Uniqueness function
#'
#' A function to determine the uniqueness of a vector
#'
#' @param vect
#'
#' @return Value for the uniqueness between 1/n and 1, 1 being entirely unique and n being the number of elements in vect
#' @export
#'
#' @examples
uniqueness <- function(vect){
        return((length(unique(vect))-1)/length(vect))
}

#' Constant length checker
#'
#' function to check that the lengths of the characters in a vector are consistent
#'
#' @param vect
#'
#' @return Boolean, True when all same length, False otherwise
#' @export
#'
#' @examples
constCharLength <- function(vect){
  if(is.character(vect)){
    number_of_lengths <- length(unique(nchar(vect)))
    return(number_of_lengths == 1)
  } else {
    return(FALSE)
  }

}

#' Date form checker (PLACEHOLDER)
#'
#' Function to check that the object is a valid date format
#'
#' @param vect
#'
#' @return TRUE always due to placeholder status
#' @export
#'
#' @examples
dateForm <- function(vect){
        return(TRUE)
}

#' Post form checker (PLACEHOLDER)
#'
#' Function to check that the object is a valid postcode format
#'
#' @param vect
#'
#' @return TRUE always due to placeholder status
#' @export
#'
#' @examples
postForm <- function(vect){
        return(TRUE)
}

#' dataTypeGuesser
#'
#' Function to guess the datatypes of a given vector
#'
#' @param vect
#'
#' @return One of an array of data types which can be in a dataset
#' @export
#'
#' @examples
guessDataType <- function(vect){
        Type <- dplyr::case_when(
                class(vect) == "Date" ~ "Date"
                , class(vect) == "POSIXct" ~ "Date-Time"
                , (uniqueness(vect) < 0.1) & constCharLength(vect) ~ "Tag"
                , (uniqueness(vect) > 0.8) & constCharLength(vect) ~ "ID"
                , (uniqueness(vect) >= 0.1) & (uniqueness(vect) <= 0.8) & constCharLength(vect) & dateForm(vect) ~ "Date"
                , (uniqueness(vect) >= 0.1) & (uniqueness(vect) <= 0.8) & constCharLength(vect) & postForm(vect) ~ "Post Code"
                , (uniqueness(vect) < 0.1) & !constCharLength(vect) ~ "Category"
                , class(vect) == "character" ~ "PII"
                , class(vect) %in% c("numeric", "integer") ~ "Value"
                , T ~ "PII/Value"
        )
        return(Type)
}

#' Creating a metadata structure for the ultra_df
#'
#' Generates the metadata table for future use and underlying the spoofing mechanism
#'
#' @param ultra_df
#' @param dict
#'
#' @return
#' @export
#'
#' @examples
ultraMetaGenerator <- function(ultra_df){
    # --TODO move into seperate functionality
    calc_values <- c("Min", "Max", "Levels")
    out_dict <- head(
      tibble::tibble(
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

      meta <- tibble::tibble(
        "Column" = i
        , "Description" = desc
        , "Type" = type
        , "Min" = Min
        , "Max" = Max
        , "Levels" = Levels
      )


      out_dict <- rbind(out_dict, meta)
    }
    return(out_dict)
  }
