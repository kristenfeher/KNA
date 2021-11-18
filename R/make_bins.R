#' takes a list of KNA data.tables and jointly bins them
#' @param X A list of KNA data.tables, the output of make_KNA
#' @param l The number of intervals to divide each dimension into
#' @param C A vector of column names that will be binned. Recommended to be first two SNPCA components and mean nearest neighbour distance
#' @return Each KNA table is augmented with a bin ID column, the bin IDs are common across the images.
make_bins <- function(X, l = 50, C = c('PC1', 'PC2', 'NNmean')) {
  lX <- length(X)
  range_X <- do.call(rbind, lapply(1:lX, function(k) X[[k]][, lapply(.SD, range), .SDcols = (C)]))
  breaks_X <- range_X[, lapply(.SD, function(x) seq(min(x), max(x), length.out = l+1))]
  for (j in 1:lX) {
    X[[j]][,  paste0(C, '_cut') := lapply(1:length(C), function(i) cut(.SD[[i]], breaks_X[[i]], 1:l, include.lowest = TRUE, ordered_result = TRUE)), .SDcols = C]
    X[[j]][,  binID := do.call(interaction, .SD), .SDcols = paste0(C, '_cut')]
    X[[j]][, paste0(C, '_cut') := NULL]
  }
  
}
