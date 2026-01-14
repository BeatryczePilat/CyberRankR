#' Ranking BWM + TOPSIS (klasyczny)
#'
#' @param criteria_types wektor typów kryteriów ("cost" lub "benefit", długość 8)
#' @return data.frame z rankingiem typów ataków według metody TOPSIS
#' @importFrom dplyr %>% mutate arrange
#' @export
cyber_rank_topsis <- function(criteria_types = c("cost", "cost", "cost", "cost", "cost", "benefit", "benefit", "cost")) {
  crit <- prepare_cyber_criteria()
  X <- as.matrix(crit[, 3:10])
  X[X == 0] <- 0.0001
  w <- cyber_bwm_weights()
  stopifnot(length(w) == 8)
  norm_X <- X / sqrt(colSums(X^2))
  V <- sweep(norm_X, 2, w, "*")
  is_benefit <- criteria_types == "benefit"
  V_pos <- ifelse(is_benefit, apply(V, 2, max), apply(V, 2, min))
  V_neg <- ifelse(is_benefit, apply(V, 2, min), apply(V, 2, max))
  d_pos <- sqrt(rowSums((V - V_pos)^2))
  d_neg <- sqrt(rowSums((V - V_neg)^2))
  closeness <- d_neg / (d_pos + d_neg)
  result <- crit %>%
    mutate(
      closeness = closeness,
      rank = rank(-closeness, ties.method = "min")
    ) %>%
    arrange(rank)
  class(result) <- c("cyberrank_topsis", class(result))
  result
}
