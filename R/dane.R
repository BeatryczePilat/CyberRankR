#' Zagregowane dane o incydentach cyberbezpieczeństwa do analizy MCDA
#'
#' Zbiór danych zawierający zagregowane informacje o incydentach
#' cyberbezpieczeństwa, pogrupowane według typu ataku.
#' Dane zostały przygotowane do wykorzystania w metodach
#' wielokryterialnego wspomagania decyzji (MCDA), takich jak
#' TOPSIS, Fuzzy TOPSIS czy PROMETHEE.
#'
#' Każdy wiersz reprezentuje jedną alternatywę decyzyjną zdefiniowaną jako kombinacja kraju, typu ataku oraz scenariusza (3 alternatywy na każdą parę kraj × typ ataku).
#' Kolumny opisują średnie wartości kryteriów obliczone na podstawie
#' co najmniej 20 zarejestrowanych incydentów.
#'
#' @format Ramka danych (data frame) z następującymi zmiennymi:
#' \describe{
#'   \item{Alternative}{Identyfikator alternatywy MCDA (Country.AttackType.id)}
#'   \item{Country}{Kraj pochodzenia ataku}
#'   \item{Attack Type}{Typ ataku cybernetycznego}
#'   \item{n}{Liczba zarejestrowanych incydentów danego typu}
#'   \item{severity}{Średni poziom powagi ataku
#'     (skala porządkowa 1--3: 1 = Low, 2 = Medium, 3 = High; kryterium kosztowe)}
#'   \item{malware}{Średni udział incydentów z wykrytym malware
#'     (0 = brak, 1 = wykryto; kryterium kosztowe)}
#'   \item{anomaly}{Średni wynik anomalii sieciowej
#'     (zmienna ciągła; kryterium kosztowe)}
#'   \item{packet_load}{Średnia długość pakietów sieciowych
#'     (wartości liczbowe; kryterium kosztowe)}
#'   \item{penetration}{Średnia głębokość penetracji sieci
#'     (skala porządkowa 1--3; kryterium kosztowe)}
#'   \item{security_det}{Średnia liczba sygnałów bezpieczeństwa
#'     (alerty, logi, IDS/IPS; kryterium typu benefit)}
#'   \item{asset_crit}{Średnia krytyczność zasobów,
#'     mierzona długością informacji o użytkowniku i urządzeniu
#'     (kryterium typu benefit)}
#'   \item{geo_risk}{Średni wskaźnik ryzyka geograficznego
#'     (0 = kraj niskiego ryzyka, 1 = kraj wysokiego ryzyka;
#'     kryterium kosztowe)}
#' }
#'
#' @usage data(cyber_attacks_processed)
#' @name cyber_attacks_processed
NULL
