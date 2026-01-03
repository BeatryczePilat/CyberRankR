#' Ranking BWM + TOPSIS (klasyczny)
#'
#' @return data.frame z rankingiem typów ataków według metody TOPSIS
#' @importFrom dplyr %>% mutate arrange
#' @export
cyber_rank_topsis <- function() {
  crit <- prepare_cyber_criteria()
  X <- as.matrix(crit[, 3:10])
  X[X == 0] <- 0.0001  # Unikamy dzielenia przez zero

  w <- cyber_bwm_weights()
  stopifnot(length(w) == 8)  # ochrona przed błędnymi wagami

  # Normalizacja wektorowa
  norm_X <- X / sqrt(colSums(X^2))

  # Ważona macierz decyzyjna
  V <- sweep(norm_X, 2, w, "*")

  # Idealne rozwiązania (pozytywne i negatywne)
  V_pos <- apply(V, 2, max)
  V_neg <- apply(V, 2, min)

  # Odległości euklidesowe
  d_pos <- sqrt(rowSums((V - V_pos)^2))
  d_neg <- sqrt(rowSums((V - V_neg)^2))

  # Wskaźnik closeness
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
