#' Przygotowanie macierzy kryteriów dla wszystkich metod
#' @export
prepare_cyber_criteria <- function(min_attacks = 20) {
  data(cyber_attacks_10k, package = "CyberRankR")

  cyber_attacks_10k %>%
    group_by(`Attack Type`) %>%
    summarise(
      n             = n(),
      severity      = mean(severity_num,      na.rm = TRUE),  # min
      malware       = mean(malware_ind,       na.rm = TRUE),  # min
      anomaly       = mean(anomaly_score,     na.rm = TRUE),  # min
      packet_load   = mean(packet_length,     na.rm = TRUE),  # min
      penetration   = mean(segment_depth,     na.rm = TRUE),  # min
      security_det  = mean(security_signals,  na.rm = TRUE),  # max
      asset_crit    = mean(asset_crit_len,    na.rm = TRUE),  # max
      geo_risk      = mean(high_risk_geo,     na.rm = TRUE),  # min
      .groups = "drop"
    ) %>%
    filter(n >= min_attacks) %>%
    arrange(desc(n))
}
