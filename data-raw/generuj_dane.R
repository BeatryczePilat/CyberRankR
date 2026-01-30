# =============================================================================
# generuj_dane.R
# =============================================================================

library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(usethis)

set.seed(123)

# Zakładamy, że plik jest w tym miejscu (dostosuj jeśli trzeba)
csv_path <- "inst/ext-data/cybersecurity_attacks.csv"

# Sprawdzenie czy plik istnieje (dla bezpieczeństwa)
if (!file.exists(csv_path)) {
  stop("Nie znaleziono pliku CSV. Upewnij się, że ścieżka jest poprawna.")
}

# ------------------------------------------------------------
# 1. Wczytanie i Wstępne Czyszczenie
# ------------------------------------------------------------
cyber_raw <- read_csv(csv_path, col_types = cols(.default = "c"))

# ------------------------------------------------------------
# 2. Definicja Panelu Eksperckiego i Alternatyw
# ------------------------------------------------------------

# KROK A: Wybieramy "Panel Ekspertów" (Top 20 krajów z największą liczbą danych)
# To symuluje, że mamy 20 ekspertów (reprezentujących regiony), którzy oceniają ataki.
top_countries <- cyber_raw %>%
  count(`Geo-location Data`, sort = TRUE) %>%
  head(20) %>%
  pull(`Geo-location Data`)

# KROK B: Wybieramy "Alternatywy" (3 główne typy ataków)
# Chcemy rankingować: Malware vs DDoS vs Intrusion
target_attacks <- c("Malware", "DDoS", "Intrusion")

# ------------------------------------------------------------
# 3. Przetwarzanie Ocen (Ekspert -> Alternatywa)
# ------------------------------------------------------------

cyber_expert_panel <- cyber_raw %>%
  # Filtrujemy tylko wybranych ekspertów i alternatywy
  filter(
    `Geo-location Data` %in% top_countries,
    `Attack Type` %in% target_attacks
  ) %>%
  mutate(
    # --- Konwersja Kryteriów na Liczby ---

    # 1. Severity (Dotkliwość) -> Skala 1-9
    severity_score = case_when(
      `Severity Level` == "Low"    ~ 2,
      `Severity Level` == "Medium" ~ 5,
      `Severity Level` == "High"   ~ 9,
      TRUE ~ 5
    ),

    # 2. Anomaly Score (Poziom Anomalii) -> Ciągła
    anomaly_score = parse_number(`Anomaly Scores`),

    # 3. Technical Complexity (Długość pakietu) -> Ciągła
    tech_complexity = parse_number(`Packet Length`),

    # 4. Impact Depth (Głębokość penetracji sieci) -> Skala 1-9
    impact_depth = case_when(
      `Network Segment` == "Segment A" ~ 3, # Płytko
      `Network Segment` == "Segment B" ~ 6,
      `Network Segment` == "Segment C" ~ 9, # Głęboko (Critical)
      TRUE ~ 5
    ),

    # 5. Detection Difficulty (Trudność wykrycia) -> Odwrócona logika
    # Jeśli firewall to złapał, to łatwo wykryć (niski score). Jeśli nie - wysoki.
    logs_present = (!is.na(`Firewall Logs`) & `Firewall Logs` != ""),
    detect_difficulty = ifelse(logs_present, 2, 8)
  ) %>%
  # --- KLUCZOWY MOMENT: Agregacja ---
  # Dla każdego Kraju (Eksperta) i Ataku (Alternatywy) liczymy średnią ocenę.
  group_by(`Geo-location Data`, `Attack Type`) %>%
  summarise(
    crit_severity   = mean(severity_score, na.rm = TRUE),
    crit_anomaly    = mean(anomaly_score, na.rm = TRUE),
    crit_complexity = mean(tech_complexity, na.rm = TRUE),
    crit_impact     = mean(impact_depth, na.rm = TRUE),
    crit_stealth    = mean(detect_difficulty, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Zmiana nazw dla jasności pakietu
  rename(
    ExpertID = `Geo-location Data`,
    Alternative = `Attack Type`
  ) %>%
  # Sortowanie dla porządku
  arrange(ExpertID, Alternative)

# ------------------------------------------------------------
# 4. Diagnostyka i Zapis
# ------------------------------------------------------------

cat("Liczba wierszy (Ekspert x Alternatywa):", nrow(cyber_expert_panel), "\n")
print(head(cyber_expert_panel))
# Sprawdzamy, czy brakuje konkretnego wiersza
brakujacy_kraj <- "Aurangabad, Nagaland"
brakujacy_atak <- "Intrusion"

czy_brakuje <- !any(cyber_expert_panel$ExpertID == brakujacy_kraj &
                      cyber_expert_panel$Alternative == brakujacy_atak)

if (czy_brakuje) {
  message("Wykryto brak danych dla: ", brakujacy_kraj, " - ", brakujacy_atak)
  message("Dokonuję imputacji średnią z pozostałych krajów dla tego typu ataku...")

  # 1. Obliczamy średnią ocenę ataku "Intrusion" (z pozostałych 19 ekspertów)
  srednia_intrussion <- cyber_expert_panel %>%
    filter(Alternative == brakujacy_atak) %>%
    summarise(
      crit_severity   = mean(crit_severity, na.rm = TRUE),
      crit_anomaly    = mean(crit_anomaly, na.rm = TRUE),
      crit_complexity = mean(crit_complexity, na.rm = TRUE),
      crit_impact     = mean(crit_impact, na.rm = TRUE),
      crit_stealth    = mean(crit_stealth, na.rm = TRUE)
    )

  # 2. Tworzymy brakujący wiersz
  nowy_wiersz <- data.frame(
    ExpertID = brakujacy_kraj,
    Alternative = brakujacy_atak,
    srednia_intrussion # Wstawiamy obliczone średnie
  )

  # 3. Doklejamy do głównej ramki
  cyber_expert_panel <- bind_rows(cyber_expert_panel, nowy_wiersz) %>%
    arrange(ExpertID, Alternative)
  message("Naprawiono. Nowa liczba wierszy: ", nrow(cyber_expert_panel))
} else {
  message("Dane są kompletne (60 wierszy).")
}
# ------------------------------------------------------------
# 5. Zapis danych do pakietu
# ------------------------------------------------------------
usethis::use_data(cyber_expert_panel, overwrite = TRUE)
