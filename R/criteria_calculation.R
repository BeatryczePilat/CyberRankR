#' Przygotowanie macierzy kryteriów dla wszystkich metod
#'
#' @param min_attacks minimalna liczba incydentów wymagana do uwzględnienia typu ataku (domyślnie 20)
#' @return data.frame z zagregowanymi kryteriami dla każdego typu ataku
#' @importFrom dplyr %>% group_by summarise filter arrange desc n
#' @export

prepare_cyber_criteria <- function(min_attacks = 20) {
  data(cyber_attacks_10k, package = "CyberRankR")

  cyber_attacks_10k %>%
    group_by(`Attack Type`) %>%
    summarise(
      n             = n(),
      severity      = mean(severity_num, na.rm = TRUE),      # im wyższa, tym gorzej (min)
      malware       = mean(malware_ind, na.rm = TRUE),       # im więcej IoC, tym gorzej (min)
      anomaly       = mean(anomaly_score, na.rm = TRUE),     # im wyższa, tym gorzej (min)
      packet_load   = mean(packet_length, na.rm = TRUE),     # im większy ruch, tym gorzej (min)
      penetration   = mean(segment_depth, na.rm = TRUE),     # im głębszy segment, tym gorzej (min)
      security_det  = mean(security_signals, na.rm = TRUE),  # im więcej wykryć, tym lepiej (max)
      asset_crit    = mean(asset_crit_len, na.rm = TRUE),    # im dłuższy opis, tym ważniejszy zasób (max)
      geo_risk      = mean(high_risk_geo, na.rm = TRUE),     # im więcej z risky krajów, tym gorzej (min)
      .groups = "drop"
    ) %>%
    filter(n >= min_attacks) %>%
    arrange(desc(n))
}
