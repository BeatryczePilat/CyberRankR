#' Ostateczny, zwarty, profesjonalny wykres (gotowy do pracy dyplomowej)
#' @export
compare_cyber_plots <- function(top_n = 10) {
  res <- cyber_rank_all()
  df <- res$classic_topsis %>%
    arrange(rank) %>%
    slice_head(n = top_n)

  ggplot(df, aes(x = reorder(paste0("#", rank, " ", `Attack Type`), rank),
                 y = closeness, size = n)) +
    geom_point(color = "#d73027", alpha = 0.95) +
    geom_segment(aes(xend = reorder(paste0("#", rank, " ", `Attack Type`), rank), yend = 0),
                 color = "grey70", linewidth = 1.2) +
    scale_size_continuous(range = c(15, 45), guide = "none") +
    scale_y_continuous(labels = percent_format(),
                       limits = c(0, 0.45),
                       breaks = seq(0, 0.4, 0.05)) +
    coord_flip() +
    labs(
      title = "CyberRankR 2025 – Ranking zagrożeń cybernetycznych",
      subtitle = "Top 10 | Wszystkie trzy metody MCDA (TOPSIS, Fuzzy TOPSIS, Fuzzy VIKOR) dały identyczny wynik",
      caption = "≈40 000 incydentów | 2025",
      x = NULL,
      y = "Closeness coefficient"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "grey30"),
      axis.text.y = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}
