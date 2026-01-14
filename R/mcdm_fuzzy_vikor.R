#' Fuzzy VIKOR
#' @param v współczynnik strategii (0 = maksymin, 1 = maksymalna grupa użyteczności, domyślnie 0.5)
#' @param criteria_types wektor typów kryteriów ("cost" lub "benefit", długość 8)
#' @return data.frame z rankingiem typów ataków (im niższy Q, tym wyższy priorytet)
#' @importFrom dplyr %>% mutate arrange
#' @export
cyber_rank_fuzzy_vikor <- function(v = 0.5, criteria_types = c("cost", "cost", "cost", "cost", "cost", "benefit", "benefit", "cost")) {
  crit <- prepare_cyber_criteria()
  X <- as.matrix(crit[, 3:10])
  w <- cyber_bwm_weights()
  stopifnot(length(w) == 8)
  L <- pmax(0, 0.8 * X)
  M <- X
  U <- 1.2 * X
  X_fuzzy <- (L + 2 * M + U) / 4
