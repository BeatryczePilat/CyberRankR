#' Porównanie wyników trzech metod MCDA
#'
#' @return lista z wynikami trzech metod oraz wydrukowana tabela porównawcza
#' @export
cyber_rank_all <- function() {
  cat("CyberRankR – trzy metody MCDA (BWM + TOPSIS, Fuzzy TOPSIS, Fuzzy VIKOR)\n\n")

  topsis_res       <- cyber_rank_topsis()
  fuzzy_topsis_res <- cyber_rank_fuzzy_topsis()
  fuzzy_vikor_res  <- cyber_rank_fuzzy_vikor()

  # Tabela porównawcza rankingów
  comparison <- data.frame(
    `Attack Type`      = topsis_res$`Attack Type`,
    TOPSIS_rank        = topsis_res$rank,
    Fuzzy_TOPSIS_rank  = fuzzy_topsis_res$rank,
    Fuzzy_VIKOR_rank   = fuzzy_vikor_res$rank,
    stringsAsFactors   = FALSE
  )

  cat("Porównanie rankingów (Top 10):\n")
  print(head(comparison, 10))

  # Zwróć pełną listę wyników
  list(
    classic_topsis = topsis_res,
    fuzzy_topsis   = fuzzy_topsis_res,
    fuzzy_vikor    = fuzzy_vikor_res,
    comparison     = comparison
  )
}

#' Główna funkcja pakietu
#' @export
run_cyberrank <- function() {
  cat("\n=== Uruchamiam CyberRankR ===\n")
  res <- cyber_rank_all()

  cat("\nPełny ranking (klasyczny TOPSIS):\n")
  print(res$classic_topsis)

  cat("\ wykres bąbelkowy...\n")
  compare_cyber_plots()

  invisible(res)
}
