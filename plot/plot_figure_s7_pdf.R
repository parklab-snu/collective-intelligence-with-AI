library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyverse)
library(viridis)

Simulation_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-Intelligence-with-AI/Original/avg_niche_seed42.RData"

load(file.path(Simulation_path))

set.seed(42)
m <- 50
alpha <- runif(m+1, min = -5, max = 5)

players_intime <- Result$players_intime

result <- t(sapply(seq_len(dim(players_intime)[1]), function(i) {
  tapply(players_intime[i, , 2], players_intime[i, , 1], mean)
}))

abs_delta <- abs(sweep(result, 2, alpha, FUN = "-"))
mean_delta <- rowMeans(abs_delta, na.rm = TRUE)

accuracy <- Result$accuracy

mean_delta_df <- data.frame(
  Generation = seq(0, 200000, length.out = 100),
  mean_abs_delta = mean_delta[1:100]
)

accuracy_df <- data.frame(
  Generation = 0:200000,
  accuracy = Result$accuracy[1:200001]
)

common_theme <- theme_classic(
  base_family = "Arial"
) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.9
    ),
    axis.title = element_text(size = 17),
    axis.text = element_text(
      size = 14,
      color = "black"
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.7
    ),
    legend.text = element_text(size = 13),
    legend.key.width = grid::unit(1.5, "cm"),
    plot.title = element_text(
      size = 17,
      face = "bold",
      hjust = 0.5
    )
  )

p_difference <- ggplot(
  mean_delta_df,
  aes(x = Generation, y = mean_abs_delta)
) +
  geom_line(
    linewidth = 1.5,
    lineend = "round",
    color = "#298C8C"
  ) +
  scale_x_continuous(
    limits = c(0, 200000),
    breaks = c(0, 50000, 100000, 150000, 200000),
    labels = c(0, 5, 10, 15, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = NULL,
    title = "Mean difference from the true coefficient"
  ) +
  common_theme

p_accuracy <- ggplot(
  accuracy_df,
  aes(x = Generation, y = accuracy)
) +
  geom_line(
    linewidth = 1.5,
    lineend = "round",
    color = "#298C8C"
  ) +
  scale_x_continuous(
    limits = c(0, 200000),
    breaks = c(0, 50000, 100000, 150000, 200000),
    labels = c(0, 5, 10, 15, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("0.0", "0.5", "1.0"),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(
    ylim = c(-0.05, 1.05)
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = NULL,
    title = "Collective accuracy"
  ) +
  common_theme

combined <- (
  p_difference |
    p_accuracy
) +
  plot_layout(
    guides = "collect"
  )

ggsave(
  file.path(
    dirname(Simulation_path),
    "mean_difference_collective_accuracy_.pdf"
  ),
  combined,
  width = 11,
  height = 5.5,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)
