## data-raw/prepare_cyberdata.R

library(readr)
library(dplyr)

#CAŁY PLIK
cyber_raw <- read_csv("inst/ext-data/cybersecurity_attacks.csv")

message("Wczytano ", nrow(cyber_raw), " rekordów")

cyber_attacks_10k <- cyber_raw %>%
  mutate(
    Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%S"),
    `Severity Level` = factor(`Severity Level`, levels = c("Low", "Medium", "High")),
    severity_num = case_when(
      `Severity Level` == "Low"    ~ 1,
      `Severity Level` == "Medium" ~ 2,
      `Severity Level` == "High"   ~ 3,
      TRUE                         ~ 2
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
        (`Firewall Logs` != "" & !is.na(`Firewall Logs`)) +
        (`IDS/IPS Alerts` != "" & !is.na(`IDS/IPS Alerts`))
    ),
    asset_crit_len = nchar(`User Information`) + nchar(`Device Information`),
    high_risk_geo = ifelse(
      grepl("China|Russia|Iran|Korea|Pakistan", `Geo-location Data`, ignore.case = TRUE),
      TRUE, FALSE
    ),
    `Attack Type` = as.factor(`Attack Type`)
  ) %>%
  filter(!is.na(anomaly_score))

cat("\nTypy ataków w pełnym zbiorze:\n")
print(table(cyber_attacks_10k$`Attack Type`))

# Zapis jako dane pakietu
usethis::use_data(cyber_attacks_10k, overwrite = TRUE)

message("\nDane zapisane. Jest ", nlevels(cyber_attacks_10k$`Attack Type`), " różnych typów ataków.")
