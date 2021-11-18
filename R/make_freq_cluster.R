#' takes a list of binned KNA data.tables and the frequency table, and makes frequency cluster
#' @param X list of binned KNA data.tables
#' @param tab the frequency table
#' @param nclus The number of clusters to be found in the frequency table. Currently hard coded to use hierarchical clustering with average linkage. 
#' @param split_clusters There is often a 'leftover' cluster of bins that aren't populated over all the images, and these occur over a wide range of mean nearest neighbour values. This logical value indicates whether to split this cluster
#' @param NNmean_break The mean nearest neighbour distance to split the leftover cluster on
#' @return The list of binned KNA data.tables is augmented with a new column indicating cluster membership
make_freq_cluster <- function(X, tab, nclus = 4, split_clusters = TRUE, NNmean_break = 3) {
  lX <- length(X)
  w <- apply(tab, 2, function(x) sum(x > 0))
  hc <- hclust(dist(t(scale(tab[, w == lX]))), method = 'average')
  cut_tree <- cutree(hc, k = nclus) 
  key_dt <- data.table(
    binID = c(colnames(tab[, w == lX]), colnames(tab[, w < lX])), 
    freq_clus = c(cut_tree, rep(max(cut_tree) + 1, length(which(w < lX))))
  )
  setkey(key_dt, 'binID')
  for (m in 1:lX) {
    setkey(X[[m]], 'binID')
    X[[m]][key_dt, 'freq_clus' := i.freq_clus, on = 'binID']
  }
  
  if (split_clusters) {
    for (m in 1:lX) {
      X[[m]][freq_clus == nclus + 1 & NNmean > NNmean_break, freq_clus:=nclus + 2]
    }
  }
  
}
