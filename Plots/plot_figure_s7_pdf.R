library(ggplot2)
library(dplyr)
library(patchwork)

#Set your project path
project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"
save_path <- file.path(project_path, "Figures/")

balanced_weight_path <- file.path(
  project_path,
  "Simulations/FigureS7_simulation"
)

w_list <- c(
  0.1, 0.2, 0.3, 0.4, 0.5,
  0.6, 0.7, 0.8, 0.9
)

read_balanced_weight_stationary <- function(path) {
  df <- bind_rows(lapply(w_list, function(j) {
    env <- new.env()

    load(
      file.path(
        path,
        sprintf("chatbot_balanced_i%02f.RData", j)
      ),
      envir = env
    )

    result <- env$Result

    data.frame(
      balancing_weight = j,
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
    filter(
      Generation >= 190000,
      Generation <= 200000
    ) %>%
    group_by(balancing_weight) %>%
    summarise(
      accuracy = mean(accuracy, na.rm = TRUE),
      human_accuracy = mean(human_accuracy, na.rm = TRUE),
      median_AI_belief = mean(median_AI_belief, na.rm = TRUE),
      bias_sq = mean(bias_sq, na.rm = TRUE),
      variance = mean(variance, na.rm = TRUE),
      interest_diversity = mean(interest_diversity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(balancing_weight)
}

df <- read_balanced_weight_stationary(
  balanced_weight_path
)

x_scale <- scale_x_continuous(
  breaks = c(
    0.1, 0.3, 0.5, 0.7, 0.9
  ),
  limits = c(0.1, 0.9)
)

single_theme <- theme_classic() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1
    ),
    axis.title = element_text(
      size = 20
    ),
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
      x = balancing_weight,
      y = .data[[metric]]
    )
  ) +
    geom_line(
      linewidth = 1.2,
      color = "#C173C3"
    ) +
    geom_point(
      size = 5,
      color = "#C173C3"
    ) +
    x_scale +
    labs(
      x = if (hide_x) NULL else "Balancing weight",
      y = NULL,
      title = title
    ) +
    single_theme

  if (!is.null(ylim)) {
    p <- p +
      coord_cartesian(
        ylim = ylim
      )
  }

  if (hide_x) {
    p <- p +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(
          5.5, 5.5, 30, 5.5
        )
      )
  }

  p
}

p_accuracy <- make_metric_plot(
  "accuracy",
  "Collective accuracy",
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
  hide_x = TRUE
)

p_var <- make_metric_plot(
  "variance",
  "Collective variance"
) +
  labs(
    x = NULL
  )

p_bias <- make_metric_plot(
  "bias_sq",
  "Collective bias"
)

p_div <- make_metric_plot(
  "interest_diversity",
  "Interest Diversity"
) +
  labs(
    x = NULL
  )

p_combined <- (
  p_accuracy | p_hacc | p_belief
) / (
  p_var | p_bias | p_div
) +
  plot_layout(
    axis_titles = "collect_x"
  )

ggsave(
  file.path(
    save_path,
    "Supplementary figure 7.pdf"
  ),
  p_combined,
  width = 13,
  height = 8,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)