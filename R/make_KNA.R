#' Takes a 2-d table of localisations, and augments it with columns corresponding to the K-neighbourhood analysis
#' @param D A SMLM image localisation table, consisting of x-y coordinates (columns)
#' @param K The number of nearest neighbours
#' @param SNPCA A pre-calculated spatially random SNPCA
#' @param ncomp The number of SNPCA components to retain
#' @param saveNN Logical value indicating whether to retain the K nearest neighbour distances
#' @param polarcoords Logical value indicating whether to convert first two SNPCA coordinates into polar coordinates
#' @param hue Logical value indicating whether to retain a hue for each localisation (for visualisation)
#' @param convert_density Logical value indicating whether to convert mean nearest neighbour value to a density
#' @return The original input data.table has been augmented with the desired quantities. Because this is using data.table, the output shouldn't be stored under a new variable (see example)
#' @examples 
#' # The PCA calculation will always be invariant up to a sign change, so it's a good idea to set the seed. Using higher values of K is possible - but the PCA/SVD starts taking a long time to calculate. Eventually this step should be able to be replaced by a formula. If you want to investigate higher values of K, it's better to keep it around 100, do the clustering (see below) then partition NND distributions for K>100 by the clustering (see appendix of paper)
#' set.seed(42)
#' SNPCA <- make_SNPCA(K = 100)

#' SMLM_images is a list of data.tables, each data.table corresponding to an SMLM image. It's assumed the images are cropped to eliminate background. 
#' # note! the coordinates must already be in a data.table format for this to work. Note the different way of calling a function - due to data.table's scoping and copying rules, the following functions updates the data.table it takes as an argument without having to store it in a new variable, i.e. don't do 'newDT <- make_KNA(DT, K = 100, SNPCA)' because DT is automatically updated. Equivalently, don't do 'DT <- make_KNA(DT, K = 100, SNPCA)'

#' lX <- length(SMLM_images)
#' for (i in 1:lX) {
#'   make_KNA(SMLM_images[[i]], K = 100, SNPCA)
#' }

#' # example plots
#' png('cell_K100.png', height = 5000, width = 5000)
#' i = 20
#' SMLM_images[[i]][, plot(x, y, pch = 16, cex = 0.7, col = hsv(h = hue))]
#' rect(20000, 17000, 27000, 24000, lwd = 2)
#' dev.off()

#' png('SNPCA_K100.png', height = 450, width = 850)
#' par(mfrow = c(1, 2), mar = c(6, 6, 6, 1), oma = c(0, 0, 0.3, 0)+0.05)
#' i = 2
#' SMLM_images[[i]][, plot(PC1, PC2, pch = 16, cex = 0.2, col = hsv(h = hue), xlab = 'SNPC1', ylab = 'SNPC2', cex.lab = 2, cex.axis = 2)]
#' SMLM_images[[i]][, plot(PC1, NNmean, pch = 16, cex = 0.2, col = hsv(h = hue), xlab = 'SNPC1', ylab = expression(log["10"]*' mean NND'), cex.lab = 2, cex.axis = 2)]
#' mtext(expression('Colour key based on '*theta*' K = 100'), outer = TRUE, cex = 2, line = -3)
#' dev.off()

#' make_bins(SMLM_images)
#' tab <- make_frequency_table(SMLM_images)
#' make_freq_cluster(SMLM_images, tab, nnsum_break = 3)

#' # visualising
#' colvec <- c('black', 'red', 'green', 'blue', 'cyan', 'magenta', 'goldenrod', 'coral', 'seagreen', 'purple', 'violetred', 'slateblue', 'orchid')
#' i = 10
#' png('freq_clus_cell.png', height = 5000, width = 5000)
#' SMLM_images[[i]][, plot(x, y, pch = 16, cex = 0.7, col = colvec[freq_clus])]
#' dev.off()

#' png('freq_clus_SNPCA.png', height = 5000, width = 5000)
#' SMLM_images[[i]][, plot(PC1, nnsum, pch = 16, cex = 0.7, col = colvec[freq_clus])]
#' legend(10, 10, legend = 1:max(SMLM_images[[i]]$freq_clus), fill = colvec[1:max(SMLM_images[[i]]$freq_clus)]) # plot a legend to see which colours correspond to cluster labels for step below
#' dev.off()


#' # manually splitting some clusters that have a large extent over PC1 (you will first have to plot NNmean vs. PC1 to select which clusters to split, see example above)

#' which_clus <- 6
#' current_names <- unique(unlist(mapply(function(x) unique(x$freq_clus), SMLM_images, SIMPLIFY = FALSE)))
#' new_clus <- max(current_names) + 1 # safety mechanism to not overwrite an existing frequency cluster
#' for (m in 1:lX) {
#'   SMLM_images[[m]][freq_clus == which_clus & PC1 > 0, c('freq_clus') := new_clus]
#' }

#' # visualising the frequency table 
#' CA <- ca(tab[, colSums(tab) == lX])
#' plot(CA)


#' # # if you wish to add an extra colour column for easy plotting, and you can easily repeat to shuffle colours for an aesthetically pleasing result (for-loops not shown)
#' i = 1
#' SMLM_images[[i]][, freq_colour := colvec[freq_clus]]

#' # # if you wish to match up KNA with clustering e.g. DBSCAN, and do other stuff with it
#' i = 1
#' col_names <- c('x', 'y') # localisation coordinate column names
#' SMLM_images[[i]][, dbscan_out := dbscan(.SD, eps = 10, minPts = 5)$cluster, .SDcols = col_names]
#' # dbscan diagnostic plot
#' SMLM_images[[i]][, plot(PC1, NNmean, pch = 16, cex = 0.5, col = factor(dbscan_out == 0))]

#' # advanced usage: a general method to map an arbitrary grouping of bins back to the localisations
#' # I generated a completely meaningless factor to demonstrate and the new column will be called  'evenodd'. This can be any kind of vector: factor, integer/real number vector. It will arise when you want to make a custom calculation using the frequency table and want to map it back to the localisations (maybe you didn't like the choice of clustering algorithm I hard coded into make_freq_cluster !)
#' my_new_bin_characteristic <- factor(c('even', 'odd')[(1:ncol(tab))%%2+1])
#' key_dt <- data.table(
#'   binID = colnames(tab),
#'   evenodd = my_new_bin_characteristic
#' )
#' setkey(key_dt, 'binID')
#' for (m in 1:lX) {
#'   setkey(SMLM_images[[m]], 'binID')
#'   SMLM_images[[m]][key_dt, 'evenodd' := i.evenodd, on = 'binID']
#' }


make_KNA <- function(D, K, SNPCA, ncomp = 4, saveNN = FALSE, polarcoords = TRUE, hue = TRUE, convert_density = FALSE) {
  
  XY <- colnames(D)
  c <- paste0('nn', 1:K) 
  p <- paste0('PC', 1:ncomp) 
  NNmean <- 'NNmean'
  d <- 'density' 
  h <- 'hue'
  polar <- c('r', 'theta')
  
  D[, (c):=(data.table(nn2(.SD, k = (K+1))[[2]][, -1])), .SDcols= XY]
  D[, (NNmean) := rowSums(.SD), .SDcols = (c)]
  
  if (convert_density) {
    D[, (d) := log10((K/2 *K^2)/(.SD^2 * pi)), .SDcols = (NNmean)]
  }
  
  D[, (NNmean) := log10((NNmean)/K)]
  
  D[, (c) := data.table(t(apply(.SD, 1, function(x) x/max(x)))), .SDcols = (c)]
  D[, (p) := data.table(scale(.SD, scale = SNPCA$scale, center = SNPCA$center) %*% SNPCA$rotation[, 1:ncomp]), .SDcols = (c[1:(K-1)])]
  
  if (!saveNN) {
    D[, (c) := NULL]
  }
  
  if (polarcoords) {
    D[, (polar) := data.table(t(apply(.SD, 1, function(x) {z <- complex(real = x[1], imaginary = x[2]); c(Mod(z), floor(Arg(z)/2/pi*360+180))}))), .SDcols = c(p)]
  }
  
  if (hue) {
    D[, (h) := ((theta-80)%%360)/360]
  }
}
