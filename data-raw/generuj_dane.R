library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(usethis)

set.seed(123)
csv_path <- "inst/ext-data/cybersecurity_attacks.csv"

cyber_raw <- read_csv(csv_path, col_types = cols(.default = "c"))

top_countries <- cyber_raw %>%
  count(`Geo-location Data`, sort = TRUE) %>%
  head(20) %>%
  pull(`Geo-location Data`)

target_attacks <- c("Malware", "DDoS", "Intrusion")

cyber_expert_panel <- cyber_raw %>%
  filter(
    `Geo-location Data` %in% top_countries,
    `Attack Type` %in% target_attacks
  ) %>%
  mutate(
    severity_score = case_when(
      `Severity Level` == "Low"    ~ 2,
      `Severity Level` == "Medium" ~ 5,
      `Severity Level` == "High"   ~ 9,
      TRUE ~ 5
    ),
    anomaly_score = as.numeric(`Anomaly Scores`),
    tech_complexity = as.numeric(`Packet Length`),
    impact_depth = case_when(
      `Network Segment` == "Segment A" ~ 3,
      `Network Segment` == "Segment B" ~ 6,
      `Network Segment` == "Segment C" ~ 9,
      TRUE ~ 5
    ),
    detect_difficulty = ifelse(is.na(`Firewall Logs`) | `Firewall Logs` == "", 8, 2)
  ) %>%
  group_by(ExpertID = `Geo-location Data`, Alternative = `Attack Type`) %>%
  summarise(
    crit_severity   = quantile(severity_score, 0.9, na.rm = TRUE),
    crit_anomaly    = quantile(anomaly_score, 0.9, na.rm = TRUE),
    crit_complexity = quantile(tech_complexity, 0.9, na.rm = TRUE),
    crit_impact     = max(impact_depth, na.rm = TRUE),
    crit_stealth    = quantile(detect_difficulty, 0.9, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ExpertID, Alternative)
set.seed(42)
weights_matrix <- matrix(runif(20 * 5), nrow = 20, ncol = 5)
weights_matrix <- weights_matrix / rowSums(weights_matrix)
colnames(weights_matrix) <- c("crit_severity", "crit_anomaly", "crit_complexity", "crit_impact", "crit_stealth")

cat("Liczba wierszy (Ekspert x Alternatywa):", nrow(cyber_expert_panel), "\n")
print(head(cyber_expert_panel))
brakujacy_kraj <- "Aurangabad, Nagaland"
brakujacy_atak <- "Intrusion"

czy_brakuje <- !any(cyber_expert_panel$ExpertID == brakujacy_kraj &
                      cyber_expert_panel$Alternative == brakujacy_atak)

if (czy_brakuje) {
  message("Wykryto brak danych dla: ", brakujacy_kraj, " - ", brakujacy_atak)
  message("Dokonuję imputacji średnią z pozostałych krajów dla tego typu ataku...")

  srednia_intrussion <- cyber_expert_panel %>%
    filter(Alternative == brakujacy_atak) %>%
    summarise(
      crit_severity   = mean(crit_severity, na.rm = TRUE),
      crit_anomaly    = mean(crit_anomaly, na.rm = TRUE),
      crit_complexity = mean(crit_complexity, na.rm = TRUE),
      crit_impact     = mean(crit_impact, na.rm = TRUE),
      crit_stealth    = mean(crit_stealth, na.rm = TRUE)
    )


  nowy_wiersz <- data.frame(
    ExpertID = brakujacy_kraj,
    Alternative = brakujacy_atak,
    srednia_intrussion
  )

  cyber_expert_panel <- bind_rows(cyber_expert_panel, nowy_wiersz) %>%
    arrange(ExpertID, Alternative)
  message("Naprawiono. Nowa liczba wierszy: ", nrow(cyber_expert_panel))
} else {
  message("Dane są kompletne (60 wierszy).")
}

usethis::use_data(cyber_expert_panel, overwrite = TRUE)
