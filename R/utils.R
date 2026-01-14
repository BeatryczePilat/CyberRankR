# R/utils.R
# =============================================================================
# Pomocnicze stałe i funkcje narzędziowe dla pakietu CyberRankR
# =============================================================================

#' Kierunek optymalizacji dla poszczególnych kryteriów
#'
#' Stała definiująca, czy dane kryterium jest typu "cost" (im mniejsza wartość,
#' tym lepiej) czy "benefit" (im większa wartość, tym lepiej).
#' Wykorzystywana do poprawnego przetwarzania macierzy decyzyjnej w metodach
#' TOPSIS, Fuzzy TOPSIS oraz VIKOR.
#'
#' @format Nazwany wektor znakowy o długości 8
#' \describe{
#'   \item{severity}{poziom powagi incydentu – cost}
#'   \item{malware}{wskaźnik obecności malware – cost}
#'   \item{anomaly}{wynik anomalii – cost}
#'   \item{packet_load}{średnia długość pakietów – cost}
#'   \item{penetration}{głębokość penetracji sieci – cost}
#'   \item{security_det}{liczba sygnałów bezpieczeństwa – benefit}
#'   \item{asset_crit}{krytyczność zasobu (długość opisu) – benefit}
#'   \item{geo_risk}{pochodzenie z kraju wysokiego ryzyka – cost}
#' }
#'
#' @export
CRITERIA_DIRECTION <- c(
  severity      = "cost",
  malware       = "cost",
  anomaly       = "cost",
  packet_load   = "cost",
  penetration   = "cost",
  security_det  = "benefit",
  asset_crit    = "benefit",
  geo_risk      = "cost"
)


#' Sprawdzenie poprawności kierunku kryteriów
#'
#' Funkcja pomocnicza weryfikująca, czy nazwy w CRITERIA_DIRECTION
#' zgadzają się z kolumnami macierzy decyzyjnej.
#'
#' @param crit_names nazwy kolumn kryteriów
#' @keywords internal
check_criteria_direction <- function(crit_names) {
  expected <- names(CRITERIA_DIRECTION)
  if (!identical(sort(expected), sort(crit_names))) {
    stop("Niezgodność nazw kryteriów z CRITERIA_DIRECTION", call. = FALSE)
  }
}
