#' Random column generator
#'
#' @param Column
#' @param ultra_df
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
  } else if (data_type == "Category") {
  levels <- as.character(
    iris$meta[iris$meta$Column == Column_in_question, ]$Levels[[1]]
  )
  rvals <- sample(levels, 100, replace = TRUE)
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
  return(df)
}
