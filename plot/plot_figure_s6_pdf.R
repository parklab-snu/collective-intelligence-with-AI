library(ggplot2)
library(dplyr)
library(patchwork)

project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"
save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-Intelligence-with-AI/Figures"

feedback_path <- file.path(
  project_path,
  "Simulations/FigureS6_simulation"
)

niche_path <- file.path(
  project_path,
  "Simulations/FigureS6_simulation"
)

balanced_path <- file.path(
  project_path,
  "Simulations/FigureS6_simulation"
)

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
      median_AI_belief = result$median_AI_belief,
      bias_sq = result$bias_sq,
      variance = result$variance,
      interest_diversity = result$interest_diversity
    )
  }))
  
  df %>%
    filter(Generation >= 190000, Generation <= 200000) %>%
    group_by(bias_i) %>%
    summarise(
      source = source,
      accuracy = mean(accuracy),
      human_accuracy = mean(human_accuracy),
      median_AI_belief = mean(median_AI_belief),
      bias_sq = mean(bias_sq),
      variance = mean(variance),
      interest_diversity = mean(interest_diversity),
      .groups = "drop"
    ) %>%
    arrange(bias_i)
}

stationary_feedback <- read_stationary(
  feedback_path,
  "chatbot_feedback_i%02f.RData",
  "Feedback"
)

stationary_niche <- read_stationary(
  niche_path,
  "chatbot_niche_i%02f.RData",
  "Niche-expert"
)

stationary_balanced <- read_stationary(
  balanced_path,
  "chatbot_balanced_i%02f.RData",
  "Balanced"
)

df <- bind_rows(
  stationary_feedback,
  stationary_niche,
  stationary_balanced
)

my_colors <- c(
  "Feedback" = "#3A85A6",
  "Niche-expert" = "#FC8644",
  "Balanced" = "#C173C3"
)

my_shapes <- c(
  "Feedback" = 17,
  "Niche-expert" = 15,
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
    axis.title.x.top = element_text(
      margin = margin(b = 8)
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

make_metric_plot <- function(
    metric,
    title,
    ylim = NULL,
    hide_x = FALSE
) {
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
    geom_point(size = 5) +
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
      x = if (hide_x) NULL else "Bias",
      y = NULL,
      title = title
    ) +
    single_theme
  
  if (!is.null(ylim)) {
    p <- p +
      coord_cartesian(ylim = ylim)
  }
  
  if (hide_x) {
    p <- p +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(5.5, 5.5, 30, 5.5)
      )
  }
  
  p
}

p_accuracy <- make_metric_plot(
  "accuracy",
  "Collective accuracy",
  ylim = c(0, 1),
  hide_x = TRUE
)

p_hacc <- make_metric_plot(
  "human_accuracy",
  "Counterfactual human CI",
  hide_x = TRUE
)

p_belief <- make_metric_plot(
  "median_AI_belief",
  "Median reliance on AI",
  ylim = c(0, 1),
  hide_x = TRUE
)

p_var <- make_metric_plot(
  "variance",
  "Collective variance",
  ylim = c(0, 750)
) +
  labs(x = NULL)

p_bias <- make_metric_plot(
  "bias_sq",
  "Collective bias",
  ylim = c(0, 800)
)

p_div <- make_metric_plot(
  "interest_diversity",
  "Interest Diversity",
  ylim = c(0, 51)
) +
  labs(x = NULL)

p_combined <- (
  p_accuracy | p_hacc | p_belief
) / (
  p_var | p_bias | p_div
) +
  plot_layout(
    guides = "collect",
    axis_titles = "collect_x"
  ) &
  theme(
    legend.position = "right"
  )

ggsave(
  file.path(save_path, "Supplementary figure 6.pdf"),
  p_combined,
  width = 13,
  height = 8,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)
