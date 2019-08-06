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

#' Single Spoof run on grouped data
#'
#' @param grouped_origin
#' @param continuous_cols
#'
#' @return
#' @export
#'
#' @examples
ultraSingleSpoofRun <- function(grouped_origin, continuous_cols){
  dplyr::summarise_at(grouped_origin, dplyr::vars(dplyr::one_of(continuous_cols)) , ultraAggRandom)
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
  # Identifies Nominal Columns
  nominal_cols <- ultraNominalCheck(ultra_df)
  continuous_cols <- ultraContinuousCheck(ultra_df)
  # Groups on Nominal columns
  grouped_origin <- group_by_at(ultra_df$origin(), vars(one_of(nominal_cols)))


  # Uses random agg to summarise in the random aggregation
  spoofed_data <- ultraSingleSpoofRun(grouped_origin, continuous_cols)

  # Length checks and possible repeats



  return(spoofed_data)
}

#' Nominal Column Checker
#'
#' @param ultra_df
#'
#' @return
#' @export
#'
#' @examples
ultraNominalCheck <- function(ultra_df){
  out <- ultra_df$meta[ultra_df$meta$Type %in% c("Category", "Tag"), ]$Column
  return(out)
}

#' Continuous Column Checker
#'
#' @param ultra_df
#'
#' @return
#' @export
#'
#' @examples
ultraContinuousCheck <- function(ultra_df){
  out <- ultra_df$meta[!ultra_df$meta$Type %in% c("Category", "Tag"), ]$Column
  return(out)
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
  first = sample(purrr::flatten(stringr::str_split(rownames(mtcars), pattern = "\\s")), 1)[[1]]
  last = sample(state.name, 1)
  return(paste0(first, " ", last))
}
