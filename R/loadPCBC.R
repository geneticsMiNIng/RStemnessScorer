#' Load PCBC
#'
#' Load PCBC data from .csv files.
#'
#' @param fname name for target .csv data file and its metadata. Data file should be written
#' as \code{fname}.csv and metadata as meta_\code{fname}.csv.
#' @return \code{ggplot} data.table object n x p, where n is for number of patients and p
#' for number of markers. First column of the object contains sample class (according to 
#' metadata \code{Diffname_short} information).
#' @author Katarzyna Sobiczewska
#' @import data.table
#' @export

loadPCBC <- function(path, data, meta_id='UID', meta_class='Diffname_short'){

  message('Load PCBC dataset...')
  PCBC.data <- fread(file.path(path, paste0(data, '.csv')), header=TRUE)
  PCBC.data.meta <- fread(file.path(path, paste0('meta_',data, '.csv')), header = TRUE,
                          select=c(meta_id, meta_class))
  message('\tUnify metadata and data sample names...')
  data_id <- colnames(PCBC.data)[1]
  temp_id <- PCBC.data[[data_id]]
  PCBC.data[[data_id]] <- sapply(temp_id, function(x) gsub('-','\\.',as.character(x)))
  temp_id <- PCBC.data.meta[[meta_id]]
  PCBC.data.meta[[meta_id]] <- sapply(temp_id, function(x) gsub('-','\\.',as.character(x)))
  
  message('\tJoin class column...')
  setkeyv(PCBC.data.meta, meta_id)
  setkeyv(PCBC.data, data_id)
  PCBC.data <- PCBC.data.meta[PCBC.data, nomatch=0]    # inner join
  setkeyv(PCBC.data, meta_class)
  message(sprintf('\tDataset %d x %d is loaded.', dim(PCBC.data)[1], dim(PCBC.data)[2]))
  return(PCBC.data)
}