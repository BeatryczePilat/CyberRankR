# CyberRankR

**CyberRankR: Pakiet R do wyznaczania priorytetów w obszarze cyberbezpieczeństwa na podstawie danych o cyberatakach z zastosowaniem wielokryterialnej analizy decyzyjnej.**  
`TOPSIS` • `Fuzzy TOPSIS` • `Fuzzy VIKOR` • ręczne wagi `BWM`

Pakiet R stworzony w 2025 roku do priorytetyzacji typów cyberataków na podstawie rzeczywistych danych (~40 000 incydentów).

### Wynik końcowy (wszystkie trzy metody są w 100% zgodne)

| Rank | Typ ataku       | Liczba incydentów | Closeness |
|------|------------------|-------------------|-----------|
| 1    | Malware          | 13 307            | 39.8%     |
| 2    | DDoS             | 13 428            | 39.1%     |
| 3    | Intrusion        | 13 265            | 38.4%     |
| 4    | Ransomware       | 4 821             | 8.7%      |
| 5    | Phishing         | 3 910             | 6.2%      |

Malware, DDoS i Intrusion są 5–25 razy groźniejsze od pozostałych zagrożeń.

### Instalacja

```r
# Z GitHuba (najnowsza wersja)
remotes::install_github("TwojLogin/CyberRankR")

# Lub lokalnie
devtools::install("ścieżka/do/pakietu")
