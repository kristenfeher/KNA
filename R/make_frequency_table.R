#' takes a list of binned KNA data.tables and makes a frequency table
#' @param X a list of binned KNA data.tables
#' @return a frequency table, where each row corresponds to an image. Each row sums to one. 
make_frequency_table <- function(X) {
  lX <- length(X)
  tab <- lapply(1:lX, function(i) X[[i]][, table(binID)/.N])
  tab <- do.call(rbind, tab)
  tab <- tab[, colSums(tab) > 0]
  return(tab)
}
