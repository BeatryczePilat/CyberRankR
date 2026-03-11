CyberRankR
================

# CyberRankR

CyberRankR to pakiet do wielokryterialnej analizy decyzyjnej (MCDA) w
środowisku rozmytym. Umożliwia pełny pipeline analityczny dla oceny
zagrożeń cybernetycznych: - przygotowanie rozmytej macierzy
decyzyjnej, - wyznaczanie wag metodą BWM (Best–Worst Method), - ranking
metodami: Fuzzy TOPSIS, Fuzzy VIKOR, Fuzzy WASPAS, - agregację wyników w
meta-ranking konsensusowy. Pakiet zawiera również funkcje do
wizualizacji wyników oraz generowania tabel w stylu APA.

------------------------------------------------------------------------

## Instalacja

Wersja deweloperska z GitHuba:

``` r
install.packages("devtools")
devtools::install_github("BeatryczePilat/CyberRankR")
```

------------------------------------------------------------------------

## Podstawowy przykład użycia

### 1. Załaduj pakiet i dane

``` r
library(CyberRankR)
data(cyber_expert_panel)
```

------------------------------------------------------------------------

### 2. Przygotuj rozmytą macierz decyzyjną

``` r
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
```

------------------------------------------------------------------------

### 3. Zdefiniuj typy kryteriów

``` r
typy_kryteriow <- c(
  severity   = "max",
  anomaly    = "min",
  complexity = "max",
  impact     = "max",
  stealth    = "min"
)
```

------------------------------------------------------------------------

### 4. Wyznacz wagi metodą BWM

``` r
wektor_najlepsze <- c(severity=3, anomaly=1, complexity=7, impact=2, stealth=4)
wektor_najgorsze <- c(severity=4, anomaly=7, complexity=1, impact=5, stealth=3)

wynik_bwm <- oblicz_wagi_bwm(
  nazwy_kryteriow = names(wektor_najlepsze),
  najlepsze_do_innych = as.numeric(wektor_najlepsze),
  inne_do_najgorszego = as.numeric(wektor_najgorsze)
)

wagi_rozmyte <- rep(wynik_bwm$wagi_kryteriow, each = 3)
```

------------------------------------------------------------------------

### 5. Analiza MCDA

Fuzzy TOPSIS

``` r
topsis_wynik <- rozmyty_topsis(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy_kryteriow,
  wagi = wagi_rozmyte
)
```

Fuzzy VIKOR

``` r
vikor_wynik <- rozmyty_vikor(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy_kryteriow,
  wagi = wagi_rozmyte,
  v = 0.5
)
```

Fuzzy WASPAS

``` r
waspas_wynik <- rozmyty_waspas(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy_kryteriow,
  wagi = wagi_rozmyte,
  lambda = 0.5
)
```

Meta-ranking (konsensus metod)

``` r
meta_wynik <- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy_kryteriow,
  wagi = wagi_rozmyte
)
```

------------------------------------------------------------------------

### 6. Wizualizacja wyników i tabele APA

``` r
plot(topsis_wynik) 
plot(vikor_wynik) 
plot(waspas_wynik)
tabela_apa(topsis_wynik) 
tabela_apa(vikor_wynik) 
tabela_apa(waspas_wynik) 
tabela_apa(meta_wynik)
```

------------------------------------------------------------------------

## Dokumentacja

Pełny tutorial dostępny jest w vignette:

``` r
browseVignettes("CyberRankR")
```
