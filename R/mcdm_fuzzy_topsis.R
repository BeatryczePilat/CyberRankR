#' Fuzzy TOPSIS (własna impl. z defuzz dla prostoty)
#' @export
cyber_rank_fuzzy_topsis <- function() {
  crit <- prepare_cyber_criteria()
  X <- as.matrix(crit[, 3:10])
  w <- cyber_bwm_weights()
  stopifnot(length(w) == 8)  # zatrzyma się, jeśli coś nie tak
  # Fuzzy: l=0.8x, m=x, u=1.2x (trójkątne)
  L <- pmax(0, 0.8 * X)
  M <- X
  U <- 1.2 * X

  # Defuzz (centrum ciężkości: (l + 2m + u)/4)
  X_fuzzy <- (L + 2 * M + U) / 4

  w <- cyber_bwm_weights()

  # Reszta jak klasyczny TOPSIS (na defuzz)
  norm_X <- X_fuzzy / sqrt(colSums(X_fuzzy^2))
  V <- sweep(norm_X, 2, w, "*")
  V_pos <- apply(V, 2, max)
  V_neg <- apply(V, 2, min)
  d_pos <- sqrt(rowSums((V - V_pos)^2))
  d_neg <- sqrt(rowSums((V - V_neg)^2))
  closeness <- d_neg / (d_pos + d_neg)

  result <- crit %>%
    mutate(closeness = closeness,
           rank = rank(-closeness, ties.method = "min")) %>%
    arrange(rank)

  class(result) <- c("cyberrank_fuzzy_topsis", class(result))
  result
}
