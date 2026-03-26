#' Zagregowane dane o incydentach cyberbezpieczeństwa
#' @format Ramka danych o 60 wierszach i 7 kolumnach:
#' \describe{
#'   \item{ExpertID}{Identyfikator eksperta (Geo-location Data)}
#'   \item{Alternative}{Typ ataku cybernetycznego (DDoS, Malware, Intrusion)}
#'   \item{crit_severity}{Dotkliwość ataku (skala 1-9)}
#'   \item{crit_anomaly}{Średni poziom anomalii (wartość ciągła)}
#'   \item{crit_complexity}{Techniczna złożoność ataku / długość pakietu}
#'   \item{crit_impact}{Głębokość penetracji sieci (skala 1-9)}
#'   \item{crit_stealth}{Trudność wykrycia ataku (skala odwrócona, 1-9)}
#' }
#'
#' @usage data(cyber_expert_panel)
#' @name cyber_expert_panel
NULL
