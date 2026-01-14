#' Wagi BWM – ręcznie ustawione na razie bo nie wychodzi mi inaczej
#' @export
#cyber_bwm_weights <- function() {
#  c(0.30, 0.20, 0.18, 0.10, 0.08, 0.06, 0.05, 0.03)
#}
#' Wagi BWM dyynamiczne
#' @param BO wektor best-to-others (długość 8)
#' @param OW wektor others-to-worst (długość 8)
#' @export
cyber_bwm_weights <- function(BO = c(1, 2, 3, 4, 5, 6, 7, 8), OW = c(8, 7, 6, 5, 4, 3, 2, 1)) {
  library(lpSolve)
  n <- length(BO)
  f.obj <- rep(1, n)
  f.con <- matrix(0, nrow = 2*(n-1) + 1, ncol = n)
  f.dir <- c(rep(">=", n-1), rep("<=", n-1), "=")
  f.rhs <- c(BO[-1], 1/OW[-1], 1)
  k <- 1
  for (i in 2:n) {
    f.con[k, 1] <- 1
    f.con[k, i] <- -BO[i]
    k <- k + 1
  }
  for (i in 2:n) {
    f.con[k, i] <- 1
    f.con[k, n] <- -OW[i]
    k <- k + 1
  }
  f.con[k, ] <- 1  # suma wag = 1
  sol <- lp("min", f.obj, f.con, f.dir, f.rhs)
  w <- sol$solution
  if (sum(w) != 1) warning("Wagi nie sumują się do 1")
  w
}
