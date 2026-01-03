#' Ranking BWM + Fuzzy TOPSIS
#'
#' Metoda stosuje rozmycie trójkątne ±20% wokół wartości ostrych,
#' a następnie defuzzyfikację (centrum ciężkości) i klasyczny TOPSIS.
#'
#' @return data.frame z rankingiem typów ataków
#' @importFrom dplyr %>% mutate arrange
#' @export
cyber_rank_fuzzy_topsis <- function() {
  crit <- prepare_cyber_criteria()
  X <- as.matrix(crit[, 3:10])

  w <- cyber_bwm_weights()
  stopifnot(length(w) == 8)  # ochrona przed błędnymi wagami

  # Fuzzy trójkątne: l = 0.8x, m = x, u = 1.2x
  L <- pmax(0, 0.8 * X)
  M <- X
  U <- 1.2 * X

  # Defuzzyfikacja – centrum ciężkości trójkątnej liczby rozmytej
  X_fuzzy <- (L + 2 * M + U) / 4

  # Klasyczny TOPSIS na zdefuzyfikowanych wartościach
  norm_X <- X_fuzzy / sqrt(colSums(X_fuzzy^2))
  V <- sweep(norm_X, 2, w, "*")

  V_pos <- apply(V, 2, max)
  V_neg <- apply(V, 2, min)

  d_pos <- sqrt(rowSums((V - V_pos)^2))
  d_neg <- sqrt(rowSums((V - V_neg)^2))

  closeness <- d_neg / (d_pos + d_neg)

  result <- crit %>%
    mutate(
      closeness = closeness,
      rank(-closeness, ties.method = "min")
    ) %>%
   arrange(rank)

  class(result) <- c("cyberrank_fuzzy_topsis", class(result))
  result
}
