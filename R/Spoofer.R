

#' Single Spoof run on grouped data
#'
#' @param grouped_origin the origin data that has been grouped already
#' @param continuous_cols the continuous columns
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
#' @param ultra_df the ultra df
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
  grouped_origin <- dplyr::group_by_at(ultra_df$origin(), dplyr::vars(dplyr::one_of(nominal_cols)))


  # Uses random agg to summarise in the random aggregation
  spoofed_data <- ultraSingleSpoofRun(grouped_origin, continuous_cols)

  # calculates the number of repeats it will require to reach 100
  required_runs <- ceiling(100 / nrow(spoofed_data))

  #initialise the stack
  stacked_spoofed_data <- spoofed_data

  # calls the spoofer that many times and binds them
  for( j in seq_len(required_runs - 1)){
    stacked_spoofed_data <- rbind(stacked_spoofed_data, ultraSingleSpoofRun(grouped_origin, continuous_cols))
  }

  # if above 100 rows use sampling to produce a dataset of 100 rows
  if(nrow(stacked_spoofed_data) > 100){
    final_spoofed_data <- dplyr::sample_n(stacked_spoofed_data, 100)
  } else {
    final_spoofed_data <- stacked_spoofed_data
  }


  return(final_spoofed_data)
}

#' Nominal Column Checker
#'
#' @param ultra_df The ultra df
#'
#' @return
#' @export
#'
#' @examples
ultraNominalCheck <- function(ultra_df){
  if("data.frame" %in% class(ultra_df$meta)){
    out <- ultra_df$meta[ultra_df$meta$Type %in% c("Category", "Tag"), ]$Column
  } else {
    out <- NULL
  }
  return(out)
}

#' Continuous Column Checker
#'
#' @param ultra_df the Ultra df
#'
#' @return
#' @export
#'
#' @examples
ultraContinuousCheck <- function(ultra_df){
  if("data.frame" %in% class(ultra_df$meta)){
  out <- ultra_df$meta[!ultra_df$meta$Type %in% c("Category", "Tag"), ]$Column
  } else {
    out <- NULL
}
  return(out)
}

#' Constrained Random generator
#'
#' @param vect the vector of values
#'
#' @return
#' @export
#'
#' @examples
ultraAggRandom <- function(vect){
  out <- stats::runif(1, base::min(vect), base::max(vect))
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
  first = sample(purrr::flatten(stringr::str_split(rownames(datasets::mtcars), pattern = "\\s")), 1)[[1]]
  last = sample(datasets::state.name, 1)
  return(paste0(first, " ", last))
}
