#' Fuzzy VIKOR
#' @param v współczynnik strategii (0 = maksymin, 1 = maksymalna grupa użyteczności, domyślnie 0.5)
#' @return data.frame z rankingiem typów ataków (im niższy Q, tym wyższy priorytet)
#' @importFrom dplyr %>% mutate arrange
#' @export
cyber_rank_fuzzy_vikor <- function(v = 0.5) {
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

  # Normalizacja (większość kryteriów to cost – dzielimy przez max)
  norm_X <- X_fuzzy / apply(X_fuzzy, 2, max)

  # S – suma ważona (group utility)
  S <- rowSums(norm_X * w)

  # R – maksymalne odchylenie (individual regret)
  R <- apply(norm_X * w, 1, max)

  # Granice dla S i R
  S_star <- min(S)
  S_minus <- max(S)
  R_star <- min(R)
  R_minus <- max(R)

  # Wskaźnik Q
  Q <- v * (S - S_star) / (S_minus - S_star) +
    (1 - v) * (R - R_star) / (R_minus - R_star)

  result <- crit %>%
    mutate(
      Q = Q,
      rank = rank(Q)  # im niższy Q, tym wyższy priorytet
    ) %>%
    arrange(rank)

  class(result) <- c("cyberrank_fuzzy_vikor", class(result))
  result
}
