#' @importFrom dplyr %>% arrange slice_head mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_text scale_size_continuous scale_y_continuous labs theme_minimal theme element_text margin
#' @importFrom scales percent_format pretty_breaks
#' @export
compare_cyber_plots <- function(top_n = 10) {
  res <- cyber_rank_all()
  df <- res$classic_topsis %>%
    arrange(rank) %>%
    slice_head(n = top_n) %>%
    mutate(label = paste0("#", rank, " ", `Attack Type`))

  # Automatyczne dopasowanie osi Y do maksymalnej wartości w Top 10 + mały margines
  y_max <- max(df$closeness) * 1.1

  ggplot(df, aes(x = reorder(label, rank), y = closeness, size = n)) +
    geom_point(color = "#d73027", alpha = 0.95) +
    geom_text(aes(label = label),
              vjust = -1.2, fontface = "bold", size = 5.5, color = "black") +
    scale_size_continuous(range = c(12, 45), guide = "none") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, y_max),
      breaks = scales::pretty_breaks(n = 8)
    ) +
    labs(
      title = "CyberRankR 2025 – Bąbelkowy ranking zagrożeń cybernetycznych",
      subtitle = paste("Top", top_n, " | Wszystkie trzy metody MCDA dały identyczny wynik"),
      caption = "≈40 000 incydentów | 2025",
      x = NULL,
      y = "Closeness coefficient (im wyżej → groźniejszy)"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
      plot.subtitle = element_text(hjust = 0.5, color = "grey30", size = 14),
      plot.caption = element_text(color = "grey50"),
      axis.text.x = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}
