# ==============================================================================
# KOMPLEKSOWY SKRYPT TESTOWY PAKIETU CyberRankR
# ==============================================================================

library(CyberRankR)

# --- KROK 1: Załadowanie i przygotowanie danych ---
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

# --- KROK 2: Bezpieczne wyliczenie wag (BWM) ---
wektor_najlepsze <- c(severity=3, anomaly=1, complexity=7, impact=2, stealth=4)
wektor_najgorsze <- c(severity=4, anomaly=7, complexity=1, impact=5, stealth=3)

wynik_bwm <- oblicz_wagi_bwm(
  nazwy_kryteriow = names(wektor_najlepsze),
  najlepsze_do_innych = as.numeric(wektor_najlepsze),
  inne_do_najgorszego = as.numeric(wektor_najgorsze)
)

# Powielenie wag do formatu rozmytego (L, M, U) dla każdej z kolumn
wagi_rozmyte <- rep(wynik_bwm$wagi_kryteriow, each = 3)

cat("\n[OK] Dane i wagi przygotowane. Przechodzę do analizy...\n")

# --- KROK 3: Uruchomienie indywidualnych metod MCDA ---

# A) Fuzzy TOPSIS
# Szuka dystansu do rozwiązania idealnego i anty-idealnego.
topsis_wynik <- rozmyty_topsis(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = poprawione_typy,
  wagi = wagi_rozmyte
)

# B) Fuzzy VIKOR
# Metoda kompromisowa. Parametr v = 0.5 oznacza zrównoważoną wagę strategii "większości kryteriów"
vikor_wynik <- rozmyty_vikor(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = poprawione_typy,
  wagi = wagi_rozmyte,
  v = 0.5
)

# C) Fuzzy WASPAS
# Łączy sumę ważoną (WSM) z iloczynem ważonym (WPM).
# Parametr lambda = 0.5 daje równą wagę obu podejściom.
waspas_wynik <- rozmyty_waspas(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = poprawione_typy,
  wagi = wagi_rozmyte,
  lambda = 0.5
)

# D) Meta-Ranking (Agregacja 3 powyższych metod)
meta_wynik <- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = poprawione_typy,
  wagi = wagi_rozmyte
)

cat("[OK] Modele przeliczone.\n")

# --- KROK 4: Generowanie Wykresów (Wizualizacje) ---

cat("\nGenerowanie wykresów...\n")
# 1. Mapa Efektywności TOPSIS (Idealny punkt to prawy dolny róg)
plot_topsis <- plot(topsis_wynik)
print(plot_topsis)

# 2. Mapa Strategiczna VIKOR (Najlepszy kompromis to zielona strefa)
plot_vikor <- plot(vikor_wynik)
print(plot_vikor)

# 3. Mapa Spójności WASPAS (Zgodność WSM vs WPM)
plot_waspas <- plot(waspas_wynik)
print(plot_waspas)


# --- KROK 5: Generowanie Tabel w formacie APA ---.

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
