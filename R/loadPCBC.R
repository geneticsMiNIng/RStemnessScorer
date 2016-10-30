#' Load PCBC
#'
#' Load PCBC data from .csv files.
#'
#' @param fname name for target .csv data file and its metadata. Data file should be written
#' as \code{fname}.csv and metadata as meta_\code{fname}.csv.
#' @return \code{ggplot} data.frame object n x p, where n is for number of patients and p
#' for number of markers. First column of the object contains sample class (according to 
#' metadata \code{Diffname_short} information).
#' @author Katarzyna Sobiczewska
#' @examples
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 theme
#' @export

loadPCBC <- function(data, id='UID', class_column='Diffname_short'){
  PCBC.data <- read.delim2(sprintf('data/%s.csv', data), sep=',')
  PCBC.data.meta <- read.table(sprintf('data/meta_%s.csv', data), sep=',', header=TRUE)
  
  samples <- PCBC.data['X']
  samples <- sapply(samples, function(x) gsub('-','\\.',as.character(x)))
  PCBC.data <- as.data.frame(PCBC.data[,-c(1,2)], stringsAsFactors = FALSE)
  
  # PCBC.data <- sapply(PCBC.data, as.numeric)
  # rownames(PCBC.data) <- samples
  
  PCBC.data.meta <- PCBC.data.meta[,c(id,class_name)]
  rownames(PCBC.data.meta) <- gsub("-",".",PCBC.data.meta[,1])
  PCBC.data <- data.frame(class=PCBC.data.meta[samples,2], PCBC.data)
  
  return(PCBC.data)
}