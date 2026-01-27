
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CyberRankR

Pakiet CyberRankR to kompleksowe narzędzie do Wielokryterialnej Analizy
Decyzyjnej (MCDA) w środowisku rozmytym. Umożliwia pełną ścieżkę
analityczną: od surowych ankiet, przez wyznaczanie wag metodą BWM
(Best-Worst Method), aż po rankingi metodami TOPSIS, VIKOR i WASPAS.

## Instalacja

Możesz zainstalować wersję deweloperską z serwisu GitHub (po
opublikowaniu): R \# install.packages(“devtools”)
devtools::install_github(“BeatryczePilat/CyberRankR”)

## Podstawowy przykład użycia pakietu CyberRankR

Oto podstawowy przykład użycia pakietu z wykorzystaniem wbudowanych
danych.

# 1. Załaduj pakiet

library(CyberRankR)

# 2. Wczytaj wbudowane dane

data(“cyber_attacks_processed”)

# 3. Przygotuj rozmytą macierz decyzyjną

# (automatyczne skalowanie do 1–9 i rozmycie trójkątne)

macierz \<- przygotuj_dane_mcda( dane = cyber_attacks_processed,
skladnia = ” severity =~ severity; malware =~ malware; anomaly =~
anomaly; packet_load =~ packet_load; penetration =~ penetration;
security_det =~ security_det; asset_crit =~ asset_crit; geo_risk =~
geo_risk “, kolumna_alternatyw =”Attack Type” )

# 4. Oblicz ranking metodą Fuzzy TOPSIS

# (wagi domyślnie z entropii Shannona)

wynik \<- rozmyty_topsis( macierz_decyzyjna = macierz, typy_kryteriow =
CRITERIA_DIRECTION )

# 5. Wyświetl wyniki rankingu

print(wynik\$wyniki)

# 6. Wyświetl mapę decyzyjną TOPSIS

plot(wynik)

# ────────────────────────────────────────────────────────────────

# 7. Bonus: wersja z wagami BWM (częściej stabilniejsza)

# ────────────────────────────────────────────────────────────────

# Przykładowe preferencje BWM (anomaly najlepsze, security_det najgorsze)

bwm_najlepsze \<- c(3,4,1,5,6,9,8,2) bwm_najgorsze \<-
c(6,5,8,4,3,1,2,7)

wynik_bwm \<- rozmyty_topsis( macierz_decyzyjna = macierz,
typy_kryteriow = CRITERIA_DIRECTION, bwm_najlepsze = bwm_najlepsze,
bwm_najgorsze = bwm_najgorsze )

print(wynik_bwm\$wyniki) plot(wynik_bwm) \## 8. Szybkie użycie – funkcja
cyber()

Po zainstalowaniu pakietu możesz uzyskać kompletny meta-ranking (VIKOR +
TOPSIS + WASPAS + konsensus) **jednym poleceniem**, bez ręcznego
przygotowywania macierzy i wag.

``` r
# Domyślna wersja – wagi BWM (zalecana)
cyber()

# Wersja alternatywna – wagi z entropii Shannona
cyber("entropia")
```
