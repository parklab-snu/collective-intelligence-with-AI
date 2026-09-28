if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("patchwork")) install.packages("patchwork")
if (!require("here")) install.packages("here")

library(ggplot2)
library(dplyr)
library(patchwork)
library(here)

simulation_path <- here("Simulations", "FigureS9_simulation")

bias_list <- seq(-0.6, 0.6, by = 0.1)

read_stationary <- function(path, filename, source) {
  df <- bind_rows(lapply(bias_list, function(i) {
    env <- new.env()
    
    load(
      file.path(path, sprintf(filename, i)),
      envir = env
    )
    
    result <- env$Result
    
    data.frame(
      bias_i = i,
      Generation = seq_along(result$accuracy),
      accuracy = result$accuracy,
      human_accuracy = result$human_accuracy,
      median_AI_belief = result$median_AI_belief
    )
  }))
  
  df %>%
    filter(Generation >= 190000, Generation <= 200000) %>%
    group_by(bias_i) %>%
    summarise(
      source = source,
      accuracy = mean(accuracy, na.rm = TRUE),
      human_accuracy = mean(human_accuracy, na.rm = TRUE),
      median_AI_belief = mean(median_AI_belief, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(bias_i)
}

stationary_feedback <- read_stationary(
  simulation_path,
  "chatbot_unrevised_balanced_i%02f.RData",
  "Unrevised balanced"
)

stationary_niche <- read_stationary(
  simulation_path,
  "chatbot_combined_balanced_i%02f.RData",
  "Combined balanced"
)

stationary_balanced <- read_stationary(
  simulation_path,
  "chatbot_balanced_i%02f.RData",
  "Balanced"
)

df <- bind_rows(
  stationary_feedback,
  stationary_niche,
  stationary_balanced
)

my_colors <- c(
  "Unrevised balanced" = "#3A85A6",
  "Combined balanced" = "#FC8644",
  "Balanced" = "#C173C3"
)

my_shapes <- c(
  "Unrevised balanced" = 17,
  "Combined balanced" = 15,
  "Balanced" = 16
)

x_scale <- scale_x_continuous(
  breaks = c(-0.6, -0.3, 0, 0.3, 0.6)
)

single_theme <- theme_classic() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1
    ),
    axis.title = element_text(size = 20),
    axis.text = element_text(
      size = 16,
      color = "black"
    ),
    axis.title.x.bottom = element_text(
      margin = margin(t = 8)
    ),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.key.height = grid::unit(0.7, "cm"),
    plot.title = element_text(
      size = 18,
      hjust = 0.5
    )
  )

make_metric_plot <- function(metric, title, ylim = NULL) {
  p <- ggplot(
    df,
    aes(
      x = bias_i,
      y = .data[[metric]],
      color = source,
      shape = source
    )
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 4.8) +
    x_scale +
    scale_color_manual(
      values = my_colors,
      name = "Incentive"
    ) +
    scale_shape_manual(
      values = my_shapes,
      name = "Incentive"
    ) +
    labs(
      x = "Bias",
      y = NULL,
      title = title
    ) +
    single_theme
  
  if (!is.null(ylim)) {
    p <- p +
      coord_cartesian(ylim = ylim)
  }
  
  p
}

p_accuracy <- make_metric_plot(
  "accuracy",
  "Collective accuracy",
  ylim = c(0, 1)
)

p_hacc <- make_metric_plot(
  "human_accuracy",
  "Counterfactual human CI"
)

p_belief <- make_metric_plot(
  "median_AI_belief",
  "Median reliance on AI",
  ylim = c(0, 1)
)

p_combined <- (
  p_accuracy | p_hacc | p_belief
) +
  plot_layout(
    guides = "collect",
    axis_titles = "collect_x"
  ) &
  theme(
    legend.position = "right"
  )

ggsave(
  here("Figures", "Supplementary figure 9.pdf"),
  p_combined,
  width = 13,
  height = 4.5,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)
