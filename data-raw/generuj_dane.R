# =============================================================================
# generuj_dane.R
# =============================================================================

library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(usethis)

set.seed(123)

csv_path <- "inst/ext-data/cybersecurity_attacks.csv"

# ------------------------------------------------------------
# 1. Wczytanie danych surowych
# ------------------------------------------------------------
cyber_raw <- read_csv(csv_path, col_types = cols(.default = "c"))
message("Wczytano ", nrow(cyber_raw), " rekordów z CSV.")

# ------------------------------------------------------------
# 2. Inżynieria cech
# ------------------------------------------------------------
cyber_attacks_processed <- cyber_raw %>%
  mutate(
    Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%S"),
    Country = str_trim(`Geo-location Data`),
    `Severity Level` = factor(`Severity Level`, levels = c("Low","Medium","High")),
    severity_num = case_when(
      `Severity Level` == "Low"    ~ 1,
      `Severity Level` == "Medium" ~ 2,
      `Severity Level` == "High"   ~ 3,
      TRUE ~ 2
    ),
    malware_ind = ifelse(`Malware Indicators` == "IoC Detected", 1, 0),
    anomaly_score = parse_number(`Anomaly Scores`),
    packet_length  = parse_number(`Packet Length`),
    segment_depth = case_when(
      `Network Segment` == "Segment A" ~ 1,
      `Network Segment` == "Segment B" ~ 2,
      `Network Segment` == "Segment C" ~ 3,
      TRUE ~ 2
    ),
    security_signals =
      (`Alerts/Warnings` == "Alert Triggered") +
      (!is.na(`Firewall Logs`) & str_trim(`Firewall Logs`) != "") +
      (!is.na(`IDS/IPS Alerts`) & str_trim(`IDS/IPS Alerts`) != ""),
    asset_crit_len =
      nchar(coalesce(`User Information`, "")) +
      nchar(coalesce(`Device Information`, "")),
    high_risk_geo = ifelse(
      grepl("China|Russia|Iran|Korea|Pakistan", Country, ignore.case = TRUE), 1, 0
    ),
    `Attack Type` = as.factor(`Attack Type`)
  ) %>%
  filter(
    !is.na(anomaly_score),
    !is.na(packet_length),
    !is.na(Country),
    Country != ""
  )

cat("Wiersze po inżynierii cech: ", nrow(cyber_attacks_processed), "\n")

# ------------------------------------------------------------
# 3. Tworzenie 3 alternatyw na (Country × Attack Type)
# ------------------------------------------------------------

# Najpierw upewniamy się, że każda grupa ma co najmniej 20 wierszy
group_sizes <- cyber_attacks_processed %>%
  group_by(Country, `Attack Type`) %>%
  summarise(n_rows = n(), .groups = "drop")

# Zachowujemy tylko grupy >= 20
valid_groups <- group_sizes %>%
  filter(n_rows >= 20) %>%
  select(Country, `Attack Type`)

cyber_attacks_processed <- cyber_attacks_processed %>%
  semi_join(valid_groups, by = c("Country", "Attack Type")) %>%
  group_by(Country, `Attack Type`) %>%
  mutate(alternative_id = ntile(row_number(), 3)) %>%
  ungroup() %>%
  mutate(Alternative = paste(Country, `Attack Type`, alternative_id, sep = ".")) %>%
  group_by(Alternative, Country, `Attack Type`) %>%
  summarise(
    n = n(),
    severity     = mean(severity_num, na.rm = TRUE),
    malware      = mean(malware_ind, na.rm = TRUE),
    anomaly      = mean(anomaly_score, na.rm = TRUE),
    packet_load  = mean(packet_length, na.rm = TRUE),
    penetration  = mean(segment_depth, na.rm = TRUE),
    security_det = mean(security_signals, na.rm = TRUE),
    asset_crit   = mean(asset_crit_len, na.rm = TRUE),
    geo_risk     = mean(high_risk_geo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Country, `Attack Type`, Alternative)

# ------------------------------------------------------------
# 4. Diagnostyka
# ------------------------------------------------------------
cat("\nLiczba alternatyw MCDA:\n")
print(nrow(cyber_attacks_processed))

cat("\nAlternatywy na (Country × Attack Type):\n")
print(
  cyber_attacks_processed %>%
    count(Country, `Attack Type`) %>%
    summarise(min = min(n), max = max(n))
)

# ------------------------------------------------------------
# 5. Zapis danych do pakietu
# ------------------------------------------------------------
usethis::use_data(cyber_attacks_processed, overwrite = TRUE)
