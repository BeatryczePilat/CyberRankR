#' @title Teoria Dominacji dla Rankingu
#' @description
#' Funkcja pomocnicza. Wyznacza ranking konsensusu na podstawie reguły większości.
#' Iteracyjnie sprawdza, która alternatywa najczęściej wygrywa na danej pozycji.
#'
#' @param r1 Wektor numeryczny rang metody 1.
#' @param r2 Wektor numeryczny rang metody 2.
#' @param r3 Wektor numeryczny rang metody 3.
#' @return Wektor numeryczny z finalnym rankingiem.
#' @keywords internal
.oblicz_ranking_dominacji <- function(r1, r2, r3) {
  n <- length(r1)
  finalny_ranking <- rep(0, n)

  macierz_rang <- cbind(r1, r2, r3)

  dostepne <- rep(TRUE, n)

  for (obecna_pozycja in 1:n) {
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf

    najlepszy_r1 <- which.min(obecna_macierz[, 1])
    najlepszy_r2 <- which.min(obecna_macierz[, 2])
    najlepszy_r3 <- which.min(obecna_macierz[, 3])

    kandydaci <- c(najlepszy_r1, najlepszy_r2, najlepszy_r3)

    tabela_czestosci <- table(kandydaci)
    zwyciezca_idx <- as.numeric(names(tabela_czestosci)[which.max(tabela_czestosci)])

    if (length(tabela_czestosci) == 3) {
      c1 <- najlepszy_r1; c2 <- najlepszy_r2; c3 <- najlepszy_r3

      c1_wygrane <- sum(macierz_rang[c1, ] < macierz_rang[c2, ]) + sum(macierz_rang[c1, ] < macierz_rang[c3, ])
      c2_wygrane <- sum(macierz_rang[c2, ] < macierz_rang[c1, ]) + sum(macierz_rang[c2, ] < macierz_rang[c3, ])
      c3_wygrane <- sum(macierz_rang[c3, ] < macierz_rang[c1, ]) + sum(macierz_rang[c3, ] < macierz_rang[c2, ])

      wygrane <- c(c1_wygrane, c2_wygrane, c3_wygrane)

      if (which.max(wygrane) == 1) zwyciezca_idx <- c1
      else if (which.max(wygrane) == 2) zwyciezca_idx <- c2
      else zwyciezca_idx <- c3
    }

    finalny_ranking[zwyciezca_idx] <- obecna_pozycja
    dostepne[zwyciezca_idx] <- FALSE
  }

  return(finalny_ranking)
}

#' @title Rozmyty Meta-Ranking
#' @description
#' Agreguje wyniki z metod Fuzzy VIKOR, TOPSIS i WASPAS, aby stworzyć
#' jeden, robustny ranking konsensusu.
#'
#' @param macierz_decyzyjna Rozmyta macierz danych.
#' @param typy_kryteriow Wektor typów ("min", "max").
#' @param wagi (Opcjonalnie) Wagi kryteriów.
#' @param bwm_najlepsze (Opcjonalnie) Wektor BWM Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor BWM Others-to-Worst.
#' @param lambda Parametr dla WASPAS (domyślnie 0.5).
#' @param v Parametr dla VIKOR (domyślnie 0.5).
#'
#' @return Lista zawierająca ramkę danych z porównaniem rankingów oraz macierz korelacji.
#' @importFrom RankAggreg BruteAggreg RankAggreg
#' @importFrom stats cor
#' @export
rozmyty_meta_ranking <- function(macierz_decyzyjna,
                                 typy_kryteriow,
                                 wagi = NULL,
                                 bwm_najlepsze = NULL,
                                 bwm_najgorsze = NULL,
                                 lambda = 0.5,
                                 v = 0.5) {

  if (is.null(wagi) && (is.null(bwm_najlepsze) || is.null(bwm_najgorsze))) {
    message("Brak wag i parametrów BWM. Obliczam wagi metodą Entropii...")
    wagi_surowe <- oblicz_wagi_entropii(macierz_decyzyjna)
    wagi <- rep(wagi_surowe, each = 3)
  }

  args_baza <- list(macierz_decyzyjna = macierz_decyzyjna, typy_kryteriow = typy_kryteriow)
  if (!is.null(wagi)) args_baza$wagi <- wagi
  if (!is.null(bwm_najlepsze)) {
    args_baza$bwm_najlepsze <- bwm_najlepsze
    args_baza$bwm_najgorsze <- bwm_najgorsze
    args_baza$bwm_kryteria <- attr(macierz_decyzyjna, "nazwy_kryteriow")
  }

  args_vikor <- c(args_baza, list(v = v))
  res_vikor <- do.call(rozmyty_vikor, args_vikor)

  res_topsis <- do.call(rozmyty_topsis, args_baza)

  args_waspas <- c(args_baza, list(lambda = lambda))
  res_waspas <- do.call(rozmyty_waspas, args_waspas)

  r_vikor  <- res_vikor$wyniki$Ranking
  r_topsis <- res_topsis$wyniki$Ranking
  r_waspas <- res_waspas$wyniki$Ranking

  suma_pkt <- r_vikor + r_topsis + r_waspas
  ranking_suma <- rank(suma_pkt, ties.method = "first")

  ranking_dominacja <- .oblicz_ranking_dominacji(r_vikor, r_topsis, r_waspas)

  macierz_dla_ra <- rbind(
    order(r_vikor),
    order(r_topsis),
    order(r_waspas)
  )

  n_alt <- nrow(macierz_decyzyjna)

  if (n_alt <= 10) {
    ra_wynik <- RankAggreg::BruteAggreg(macierz_dla_ra, n_alt, distance = "Spearman")
  } else {
    ra_wynik <- RankAggreg::RankAggreg(macierz_dla_ra, n_alt, method = "GA", distance = "Spearman", verbose = FALSE)
  }

  top_lista <- ra_wynik$top.list
  wektor_ra <- numeric(n_alt)

  for(pozycja in 1:n_alt) {
    indeks_alternatywy <- as.numeric(top_lista[pozycja])
    wektor_ra[indeks_alternatywy] <- pozycja
  }

  porownanie_df <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    R_VIKOR = r_vikor,
    R_TOPSIS = r_topsis,
    R_WASPAS = r_waspas,
    Meta_Suma = ranking_suma,
    Meta_Dominacja = ranking_dominacja,
    Meta_Agregacja = wektor_ra
  )

  macierz_kor <- cor(porownanie_df[,-1], method = "spearman")

  wagi_wyjsciowe <- if (!is.null(wagi)) wagi[seq(1, length(wagi), 3)] else NULL

  wynik <- list(
    porownanie = porownanie_df,
    korelacje = macierz_kor,
    wagi_ostre = wagi_wyjsciowe
  )

  return(wynik)
}
#' @title Szybki meta-ranking cyberbezpieczeństwa
#' @description Jedno słowo → pełny meta-ranking z domyślnymi danymi i wagami BWM
#' @param wagi "entropia" lub "bwm" (domyślnie "bwm")
#' @param ... dodatkowe argumenty przekazywane do rozmyty_meta_ranking
#' @return Obiekt meta-rankingu (niewidoczny), ale od razu drukuje tabelkę
#' @export
cyber <- function(wagi = "bwm", ...) {

  data(cyber_expert_panel, envir = environment())

    CRITERIA_DIRECTION <- c(
    severity   = "max",
    anomaly    = "min",
    complexity = "max",
    impact     = "max",
    stealth    = "min"
  )

  macierz <- przygotuj_dane_mcda(
    dane = cyber_expert_panel,
    skladnia = "
    severity   =~ crit_severity;
    anomaly    =~ crit_anomaly;
    complexity =~ crit_complexity;
    impact     =~ crit_impact;
    stealth    =~ crit_stealth
  ",
    kolumna_alternatyw = "Alternative"
  )

  typy <- CRITERIA_DIRECTION

  if (wagi == "entropia") {
    message("Używam wag entropii...")
    wynik <- rozmyty_meta_ranking(macierz, typy, ...)
  } else {
    message("Używam wag BWM (domyślna konfiguracja)...")
    bwm_najlepsze <- c(
      severity   = 3,
      anomaly    = 1,  # BEST
      complexity = 7,
      impact     = 2,
      stealth    = 4
    )
    bwm_najgorsze <- c(
      severity   = 4,
      anomaly    = 7,
      complexity = 1,  # WORST
      impact     = 5,
      stealth    = 3
    )
    wynik <- rozmyty_meta_ranking(
      macierz, typy,
      bwm_najlepsze = bwm_najlepsze,
      bwm_najgorsze = bwm_najgorsze,
      ...
    )
  }

  if (wagi != "entropia") {
    bwm_wagi_ostre <- oblicz_wagi_bwm(names(bwm_najlepsze), as.numeric(bwm_najlepsze), as.numeric(bwm_najgorsze))$wagi_kryteriow
    wagi_do_plotow <- rep(bwm_wagi_ostre, each = 3)
  } else {
    wagi_do_plotow <- rep(wynik$wagi_ostre, each = 3)
  }

  message("\nGeneruję wizualizacje...")

  w_topsis <- rozmyty_topsis(macierz, typy_kryteriow = typy, wagi = wagi_do_plotow)
  w_vikor  <- rozmyty_vikor(macierz, typy_kryteriow = typy, wagi = wagi_do_plotow)

  print(plot(w_topsis))
  print(plot(w_vikor))

  cat("\n┌───────────────────────────────────────────────┐\n")
  cat("│               META-RANKING CYBER                 │\n")
  cat("└───────────────────────────────────────────────┘\n\n")

  print(wynik$porownanie, row.names = FALSE)

  cat("\nKorelacje Spearmana:\n")
  print(round(wynik$korelacje, 2))

  invisible(wynik)
}
