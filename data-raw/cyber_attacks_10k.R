# data-raw/cyber_attacks_10k.R
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

csv_path <- "inst/ext-data/cybersecurity_attacks.csv"
cyber_raw <- read_csv(csv_path, col_types = cols(.default = "c"))

cyber_attacks_10k <- cyber_raw %>%
  mutate(
    Timestamp = ymd_hms(Timestamp),
    severity_num = case_when(
      `Severity Level` == "Low" ~ 1,
      `Severity Level` == "Medium" ~ 2,
      `Severity Level` == "High" ~ 3,
      TRUE ~ 2
    ),
    malware_ind = ifelse(`Malware Indicators` == "IoC Detected", 1, 0),
    anomaly_score = as.numeric(`Anomaly Scores`),
    packet_length = as.numeric(`Packet Length`),
    segment_depth = case_when(
      `Network Segment` == "Segment A" ~ 1,
      `Network Segment` == "Segment B" ~ 2,
      `Network Segment` == "Segment C" ~ 3,
      TRUE ~ 2
    ),
    security_signals = (
      (`Alerts/Warnings` == "Alert Triggered") +
        str_trim(`IDS/IPS Alerts`) != "" +
        str_trim(`Firewall Logs`) != ""
    ),
    asset_crit_len = nchar(coalesce(`User Information`, "")) + nchar(coalesce(`Device Information`, "")),
    high_risk_geo = str_detect(`Geo-location Data`, regex("China|Russia|Iran|Korea|Pakistan", ignore_case = TRUE)),
    attack_type = as.factor(`Attack Type`)
  ) %>%
  filter(!is.na(anomaly_score), !is.na(packet_length))

usethis::use_data(cyber_attacks_10k, overwrite = TRUE)
