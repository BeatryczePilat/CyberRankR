#' Fuzzy VIKOR (własna impl. z defuzz)
#' @param v parametr strategii (0.5 = równowaga)
#' @export
cyber_rank_fuzzy_vikor <- function(v = 0.5) {
  crit <- prepare_cyber_criteria()
  X <- as.matrix(crit[, 3:10])
  w <- cyber_bwm_weights()
  stopifnot(length(w) == 8)  # zatrzyma się, jeśli coś nie tak
  # Fuzzy defuzz jak wyżej
  L <- pmax(0, 0.8 * X)
  M <- X
  U <- 1.2 * X
  X_fuzzy <- (L + 2 * M + U) / 4

  w <- cyber_bwm_weights()

  # Normalizacja (max dla benefit, ale tu większość cost – uproszczone)
  norm_X <- X_fuzzy / apply(X_fuzzy, 2, max)

  # S (group utility), R (individual regret)
  S <- rowSums(norm_X * w)
  R <- apply(norm_X * w, 1, max)

  S_star <- min(S); S_minus <- max(S)
  R_star <- min(R); R_minus <- max(R)

  # Q
  Q <- v * (S - S_star) / (S_minus - S_star) + (1 - v) * (R - R_star) / (R_minus - R_star)

  result <- crit %>%
    mutate(Q = Q,
           rank = rank(Q)) %>%  # Niższy Q = lepszy
    arrange(rank)

  class(result) <- c("cyberrank_fuzzy_vikor", class(result))
  result
}
