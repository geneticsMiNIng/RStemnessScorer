#' Check NA values in given data frame
#'
#' Check for NA values in each row. Skip these rows and print appropriate message.
#'
#' @param xdata pis
#' @param ydata opis
#' @details cos
#' @return Logical vector of non NA rows.
#' @author Katarzyna Sobiczewska
#' @examples
#' stay <- nonNA(xdata, ydata)
#' xdata <- xdata[stay,]
#' ydata <- ydata[stay]

nonNAdata <- function(data){
  d <- dim(data)
  data <- data[, .SD, .SDcols = (sapply(data, function(col) !any(is.na(col))))]
  sprintf("There occure %d columns with at least 1 NA. These columns were skipped.",
          d[2]-dim(data)[2]) -> warn
  message(warn)
  return(data)
}
