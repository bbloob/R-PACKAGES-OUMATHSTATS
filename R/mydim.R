#' Dimesnsion Function
#'
#' @param data an R object for example a matrix, array, or dataframe
#'
#' @return A numeric vector of length two. The first element is the number of rows,
#' and the second element is the number of columns in the dataset.
#' @export
#'
#' @examples ## mydim('mtbe')   Returns the dimensions of the 'mtbe' dataset
mydim <- function(data) {
  rows <- nrow(data)
  columns <- ncol(data)
  return(c(rows, columns))
}
