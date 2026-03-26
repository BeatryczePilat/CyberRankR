library(CyberRankR)

data(cyber_expert_panel)

poprawione_typy <- c(
  severity   = "max",
  anomaly    = "min",
  complexity = "max",
  impact     = "max",
  stealth    = "min"
)

macierz_rozmyta <- przygotuj_dane_mcda(
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

wektor_najlepsze <- c(severity=3, anomaly=1, complexity=7, impact=2, stealth=4)
wektor_najgorsze <- c(severity=4, anomaly=7, complexity=1, impact=5, stealth=3)

wynik_bwm <- oblicz_wagi_bwm(
  nazwy_kryteriow = names(wektor_najlepsze),
  najlepsze_do_innych = as.numeric(wektor_najlepsze),
  inne_do_najgorszego = as.numeric(wektor_najgorsze)
)

wagi_rozmyte <- rep(wynik_bwm$wagi_kryteriow, each = 3)

cat("\n[OK] Dane i wagi przygotowane. Przechodzę do analizy...\n")

topsis_wynik <- rozmyty_topsis(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = poprawione_typy,
  wagi = wagi_rozmyte
)

vikor_wynik <- rozmyty_vikor(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = poprawione_typy,
  wagi = wagi_rozmyte,
  v = 0.5
)

waspas_wynik <- rozmyty_waspas(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = poprawione_typy,
  wagi = wagi_rozmyte,
  lambda = 0.5
)

meta_wynik <- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = poprawione_typy,
  wagi = wagi_rozmyte
)

cat("[OK] Modele przeliczone.\n")

cat("\nGenerowanie wykresów...\n")
plot_topsis <- plot(topsis_wynik)
print(plot_topsis)

plot_vikor <- plot(vikor_wynik)
print(plot_vikor)

plot_waspas <- plot(waspas_wynik)
print(plot_waspas)

cat("\nGenerowanie tabel APA...\n")

tabela_topsis <- tabela_apa(topsis_wynik, tytul = "Tabela 1. Wyniki metody Fuzzy TOPSIS dla typów ataków")
print(tabela_topsis)

tabela_vikor <- tabela_apa(vikor_wynik, tytul = "Tabela 2. Wyniki metody Fuzzy VIKOR ze wskaźnikami kompromisu")
print(tabela_vikor)

tabela_waspas <- tabela_apa(waspas_wynik, tytul = "Tabela 3. Wyniki metody Fuzzy WASPAS z podziałem na WSM i WPM")
print(tabela_waspas)

tabela_meta <- tabela_apa(meta_wynik, tytul = "Tabela 4. Finalny Meta-Ranking priorytetyzacji ataków cybernetycznych")
print(tabela_meta)

cat("\n[SUKCES] Cały potok analityczny zakończony!\n")
