#' Porównanie trzech metod
#' @export
cyber_rank_all <- function() {
  cat("CyberRankR – trzy metody MCDA (własne impl., bez zależności)\n\n")

  topsis_res <- cyber_rank_topsis()
  fuzzy_topsis_res <- cyber_rank_fuzzy_topsis()
  fuzzy_vikor_res <- cyber_rank_fuzzy_vikor()

  # Porównanie
  comparison <- data.frame(
    `Attack Type` = topsis_res$`Attack Type`,
    TOPSIS_rank = topsis_res$rank,
    Fuzzy_TOPSIS_rank = fuzzy_topsis_res$rank,
    Fuzzy_VIKOR_rank = fuzzy_vikor_res$rank
  )
  print(comparison)

  list(
    classic_topsis = topsis_res,
    fuzzy_topsis = fuzzy_topsis_res,
    fuzzy_vikor = fuzzy_vikor_res
  )
}

#' Główna funkcja
#' @export
run_cyberrank <- function() {
  res <- cyber_rank_all()
  print(res$classic_topsis)  # Przykład
  plot(res$classic_topsis)
  invisible(res)
}
