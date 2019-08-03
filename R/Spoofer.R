#' Random column generator
#'
#' @param ultra_df
#' @param Column_in_question
#'
#' @return
#' @export
#'
#' @examples
ultraRandoColumn <- function(ultra_df, Column_in_question){
  data_type <- ultra_df$meta[ultra_df$meta[, "Column"] == Column_in_question,]$Type

  if (data_type == "Value") {
  minval <- ultra_df$meta[ultra_df$meta[, "Column"] == Column_in_question,]$Min
  maxval <- ultra_df$meta[ultra_df$meta[, "Column"] == Column_in_question,]$Max
  rvals <- runif(100, minval, maxval)
  } else if (data_type %in% c("Category", "Tag")) {
  levels <- as.character(
    ultra_df$meta[ultra_df$meta$Column == Column_in_question, ]$Levels[[1]]
  )
  rvals <- sample(levels, 100, replace = TRUE)
  } else if (data_type %in% c("PII")) {
    rvals <- replicate(100, ultraNameSpoof())
  } else {
    rvals <- rep(NA, 100)
  }

  rvals <- tibble::tibble( rvals )
  colnames(rvals) <- Column_in_question
  return(rvals)
}

#' random dataset generator
#'
#' @param ultra_df
#'
#' @return
#' @export
#'
#' @examples
ultraRandoDataset <- function(ultra_df){
  cols <- list()
  for(i in ultra_df$meta$Column){
    col <- ultraRandoColumn(ultra_df, i)
    cols[i] <- col
  }
  df <- do.call(cbind, cols)
  df <- tibble::as.tibble(df)
  return(df)
}

#' New spoofing functio
#'
#' @description Function which uses the new devised method to spoof without introducing illegal combinations of values.
#'
#' @param ultra_df
#'
#' @return
#' @export
#'
#' @examples
ultraSpooferNew <- function(ultra_df){
  spoofed_data <- NULL

  return(spoofed_data)
}

#' Constrained Random generator
#'
#' @param vect
#'
#' @return
#' @export
#'
#' @examples
ultraAggRandom <- function(vect){
  out <- runif(1, min(vect), max(vect))
  return(out)
}

#' Random Name generator
#'
#' @description uses the embedded mtcars and state.name datasets to generate passable, if often amusing fake names in PII slots
#'
#' @return
#' @export
#'
#' @examples
ultraNameSpoof <- function(){
  first = sample(purrr::flatten(test), 1)[[1]]
  last = sample(state.name, 1)
  return(paste0(first, " ", last))
}
