#' calculates spatial neighbourhood PCA (SNPCA) that is based on spatially random data, to be applied to the images
#' @title Calculates SNPCA from spatially random data
#' @param K The number of nearest neighbours, e.g. K = 100
#' @param npoints The number of spatially random points that are simulated
#' @param winsize The size of the window in which to simulate spatially random points
#' @param border The width of the border, points in which do not contribute to the SNPCA
#' @return An object of class prcomp (from R Stats package) containing a spatially random SNPCA, to be applied to a set of SMLM images
make_SNPCA <- function(K, npoints = 100000, winsize = 2, border = 0.5) {
  rnd <- coords(rpoispp(npoints, win = owin(c(0, winsize), c(0, winsize))))
  w <- rnd[, 1] > border & rnd[, 1] < (winsize-border) & rnd[, 2] > border & rnd[, 2] < (winsize-border)
  nn_rnd <-  nn2(rnd, rnd[w, ], k = (K+1), eps = 0.01)$nn.dists[, -1]
  # 
  nn_unit_rnd <- t(apply(nn_rnd, 1, function(x) x/max(x)))
  pr_rnd <- prcomp(nn_unit_rnd[, 1:(K-1)], center = TRUE, scale. = FALSE)
  return(pr_rnd)
}
