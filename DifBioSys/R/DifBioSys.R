solveDifEq <- function(L, n, m, dF, dif) {
  Rcpp::sourceCpp('difEq.cpp')
  difEq <- solveDiffusionEq(L,n,m, dF, dif)
  matrixDifEq <- matrix(difEq, ncol = 3, byrow = TRUE)
  Df <<- as.data.frame(matrixDifEq)
  colnames(Df) <<- c("x","t","u")
}

plotDiffusion <- function(x, xlab = "x", ylab = "t", scaleName = "Temp.") {

  library(ggplot2)
  library(viridis)

  ggplot(x, aes(x = x, y = t)) +
    geom_tile(aes(fill = u)) +
    scale_fill_viridis(name = scaleName, option = "C") +
    labs(x = xlab, y = ylab)
}
