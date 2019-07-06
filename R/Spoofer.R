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
  minval <- ultra_df$meta[ultra_df$meta[, "Column"] == Column_in_question,]$Min
  maxval <- ultra_df$meta[ultra_df$meta[, "Column"] == Column_in_question,]$Max
  rvals <- runif(100, minval, maxval)
  colnames(rvals) <- Column
  rvals <- tibble::tibble( rvals )
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
  for(i in ultra_df$meta[ultra_df$meta$Type == "Value",]$Column){
    col <- ultraRandoColumn(ultra_df, i)
    cols[i] <- col
  }
  df <- do.call(cbind, cols)
  return(df)
}
