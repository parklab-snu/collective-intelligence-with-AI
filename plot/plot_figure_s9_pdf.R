library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyverse)
library(viridis)

Simulation_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/balanced_sweep_random"
Figure_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-Intelligence-with-AI/Figures"

bias_list <- c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)

result_list <- list()

for(i in bias_list){
  for(j in 1:30){
    filename <- sprintf("Balanced_i%02f_j%02f.RData", i, j)
    filepath <- file.path(Simulation_path, filename)
    
    load(filepath)
    
    mean_reliance <- mean(
      Result$median_AI_belief[190000:200000],
      na.rm = TRUE
    )
    
    result_list[[length(result_list) + 1]] <- data.frame(
      bias = i,
      replicate = j,
      median_reliance = mean_reliance
    )
  }
}

result_df <- bind_rows(result_list)

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

p_reliance <- ggplot(
  result_df,
  aes(x = bias, y = median_reliance, group = bias)
) +
  geom_jitter(
    width = 0.012,
    height = 0,
    size = 2.2,
    alpha = 0.5,
    color = "grey75"
  ) +
  geom_boxplot(
    width = 0.055,
    outlier.shape = NA,
    fill = NA,
    linewidth = 1,
    color = "#98489E"
  ) +
  scale_x_continuous(
    limits = c(-0.7, 0.7),
    breaks = c(-0.6, -0.3, 0, 0.3, 0.6),
    labels = c("-0.6", "-0.3", "0", "0.3", "0.6"),
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
    x = "Bias",
    y = "Median reliance on AI"
  ) +
  common_theme

ggsave(
  file.path(
    Figure_path,
    "Balanced_bias_random_reliance.pdf"
  ),
  p_reliance,
  width = 5.5,
  height = 4.8,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)
