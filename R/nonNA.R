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

nonNA <- function(data){
  inas <- apply(data,1,function(x) sum(is.na(x)))
  stay <- !as.logical(inas)
  ind <- which(inas!=0)
  if( length(ind)>0 ){
    sprintf("There occure %s NAs in rows %s. These rows were skipped.", paste0(inas[ind], collapse=','),
            paste0(ind, collapse = ",")) -> warn
    message(warn)
  }
  return(stay)
}
