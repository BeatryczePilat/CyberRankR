# data-raw/generuj_dane.R
# =============================================================================
# Skrypt wczytujący i przetwarzający dane z CSV "cybersecurity_attacks.csv"
# dla problemu MCDA w cyberbezpieczeństwie. Dane są agregowane po typie ataku
# i przygotowane do użycia w metodach jak TOPSIS, Fuzzy TOPSIS itp.
# Dostosowano z oryginalnego skryptu symulującego dane MCDA.
# =============================================================================

# Ustawiamy ziarno losowości, aby wyniki były powtarzalne (choć dla rzeczywistych danych mniej istotne)
set.seed(123)

# Wymagane biblioteki (dodaj do DESCRIPTION jeśli potrzeba)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Ścieżka do pliku CSV (dostosuj jeśli potrzeba; zakładam strukturę pakietu)
csv_path <- "inst/ext-data/cybersecurity_attacks.csv"

# Wczytanie surowych danych z CSV
cyber_raw <- read_csv(csv_path, col_types = cols(.default = "c"))

message("Wczytano ", nrow(cyber_raw), " rekordów z CSV.")

# Przetwarzanie danych (podobnie jak w prepare_cyberdata.R i cyber_attacks_10k.R)
cyber_attacks_processed <- cyber_raw %>%
  mutate(
    Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%S"),
    `Severity Level` = factor(`Severity Level`, levels = c("Low", "Medium", "High")),
    severity_num = case_when(
      `Severity Level` == "Low"    ~ 1,
      `Severity Level` == "Medium" ~ 2,
      `Severity Level` == "High"   ~ 3,
      TRUE                         ~ 2  # Domyślna wartość dla brakujących
    ),
    malware_ind = ifelse(`Malware Indicators` == "IoC Detected", 1, 0),
    anomaly_score = as.numeric(`Anomaly Scores`),  # Konwersja na numeryczne
    packet_length = as.numeric(`Packet Length`),   # Konwersja na numeryczne
    segment_depth = case_when(
      `Network Segment` == "Segment A" ~ 1,
      `Network Segment` == "Segment B" ~ 2,
      `Network Segment` == "Segment C" ~ 3,
      TRUE ~ 2  # Domyślna wartość
    ),
    security_signals = (
      (`Alerts/Warnings` == "Alert Triggered") +
        (!is.na(`Firewall Logs`) & str_trim(`Firewall Logs`) != "") +
        (!is.na(`IDS/IPS Alerts`) & str_trim(`IDS/IPS Alerts`) != "")
    ),
    asset_crit_len = nchar(coalesce(`User Information`, "")) + nchar(coalesce(`Device Information`, "")),
    high_risk_geo = ifelse(
      grepl("China|Russia|Iran|Korea|Pakistan", `Geo-location Data`, ignore.case = TRUE),
      1, 0  # 1 jeśli kraj wysokiego ryzyka, 0 inaczej
    ),
    `Attack Type` = as.factor(`Attack Type`)
  ) %>%
  # Filtrowanie brakujących wartości (np. anomaly_score nie może być NA)
  filter(!is.na(anomaly_score), !is.na(packet_length)) %>%
  # Agregacja po typie ataku (alternatywy w MCDA) – średnie i liczniki dla kryteriów
  group_by(`Attack Type`) %>%
  summarise(
    n = n(),  # Liczba incydentów (do filtrowania min_attacks w prepare_cyber_criteria)
    severity = mean(severity_num, na.rm = TRUE),        # Średni poziom powagi (cost)
    malware = mean(malware_ind, na.rm = TRUE),          # Średni wskaźnik malware (cost)
    anomaly = mean(anomaly_score, na.rm = TRUE),        # Średni wynik anomalii (cost)
    packet_load = mean(packet_length, na.rm = TRUE),    # Średnia długość pakietów (cost)
    penetration = mean(segment_depth, na.rm = TRUE),    # Średnia głębokość penetracji (cost)
    security_det = mean(security_signals, na.rm = TRUE),# Średnia liczba sygnałów bezpieczeństwa (benefit)
    asset_crit = mean(asset_crit_len, na.rm = TRUE),    # Średnia krytyczność zasobu (benefit)
    geo_risk = mean(high_risk_geo, na.rm = TRUE),       # Średni ryzyko geo (cost)
    .groups = "drop"
  ) %>%
  # Filtrowanie typów ataków z minimum 20 incydentami (jak w prepare_cyber_criteria)
  filter(n >= 20) %>%
  arrange(desc(n))

# Wyświetlenie podsumowania (opcjonalne, dla debugowania)
cat("\nTypy ataków po agregacji:\n")
print(table(cyber_attacks_processed$`Attack Type`))

message("\nPrzetworzono dane. Jest ", nrow(cyber_attacks_processed), " różnych typów ataków po agregacji.")

# KROK KLUCZOWY: Zapisanie przetworzonych danych do folderu pakietu /data
# Funkcja use_data automatycznie kompresuje dane do formatu .rda
usethis::use_data(cyber_attacks_processed, overwrite = TRUE)
