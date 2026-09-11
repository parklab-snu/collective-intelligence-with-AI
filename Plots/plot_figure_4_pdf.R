if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("patchwork")) install.packages("patchwork")

library(ggplot2)
library(dplyr)
library(patchwork)

#Set your project path
project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"
save_path <- file.path(project_path, "Figures/")

feedback_env <- new.env()
niche_env <- new.env()
balanced_env <- new.env()

load(file.path(project_path, "Simulations/Figure4_simulation/chatbot_feedback_0.4.RData"), envir = feedback_env)
load(file.path(project_path, "Simulations/Figure4_simulation/chatbot_niche_0.4.RData"), envir = niche_env)
load(file.path(project_path, "Simulations/Figure4_simulation/chatbot_balanced_0.4.RData"), envir = balanced_env)

feedback_result <- feedback_env$Result
niche_result <- niche_env$Result
balanced_result <- balanced_env$Result

generation <- seq_len(200000)

incentive_order <- c(
  "Feedback",
  "Niche-expert",
  "Balanced"
)

incentive_colors <- c(
  "Feedback" = "#3381a3",
  "Niche-expert" = "#fc8644",
  "Balanced" = "#c273c3"
)

df_trajectory <- bind_rows(
  data.frame(
    Generation = generation,
    accuracy = feedback_result$accuracy,
    human_accuracy = feedback_result$human_accuracy,
    Incentive = "Feedback"
  ),
  data.frame(
    Generation = generation,
    accuracy = niche_result$accuracy,
    human_accuracy = niche_result$human_accuracy,
    Incentive = "Niche-expert"
  ),
  data.frame(
    Generation = generation,
    accuracy = balanced_result$accuracy,
    human_accuracy = balanced_result$human_accuracy,
    Incentive = "Balanced"
  )
) %>%
  mutate(
    Incentive = factor(
      Incentive,
      levels = incentive_order
    )
  )

trajectory_theme <- theme_classic(
  base_family = "Arial",
  base_size = 24
) +
  theme(
    axis.line = element_blank(),
    panel.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 1.2
    ),
    panel.border = element_blank(),
    axis.title.x = element_text(
      size = 28,
      margin = margin(t = 14)
    ),
    axis.title.y = element_text(
      size = 28,
      margin = margin(r = 14)
    ),
    axis.text = element_text(
      size = 23,
      color = "black"
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 1
    ),
    axis.ticks.length = grid::unit(0.22, "cm"),
    legend.position = "bottom",
    legend.title = element_text(
      size = 20,
      face = "bold"
    ),
    legend.text = element_text(size = 20),
    legend.key.width = grid::unit(2.1, "cm"),
    legend.spacing.x = grid::unit(0.5, "cm"),
    legend.margin = margin(t = 12),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )

make_trajectory <- function(variable, y_label, y_breaks, y_limits) {
  ggplot(
    df_trajectory,
    aes(
      x = Generation,
      y = .data[[variable]],
      color = Incentive
    )
  ) +
    geom_line(
      linewidth = 2.2,
      lineend = "round"
    ) +
    scale_color_manual(
      values = incentive_colors,
      breaks = incentive_order
    ) +
    scale_x_continuous(
      breaks = c(0, 100000, 200000),
      labels = c(0, 10, 20),
      limits = c(0, 200000),
      expand = expansion(mult = c(0, 0.005))
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      limits = y_limits,
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
      y = y_label,
      color = "Incentive"
    ) +
    guides(
      color = guide_legend(
        nrow = 1,
        byrow = TRUE,
        title.position = "left",
        override.aes = list(linewidth = 2.5)
      )
    ) +
    trajectory_theme
}

trajectory_accuracy <- make_trajectory(
  "accuracy",
  "Collective accuracy",
  c(0.0, 0.5, 1.0),
  c(-0.05, 1.05)
)

trajectory_human <- make_trajectory(
  "human_accuracy",
  "Counterfactual human CI",
  c(0.0, 0.5, 1.0),
  c(-0.05, 1.05)
)

trajectory_row <- trajectory_accuracy +
  trajectory_human +
  plot_layout(
    ncol = 2,
    guides = "collect",
    axis_titles = "collect_x"
  ) &
  theme(
    legend.position = "bottom"
  )

feedback_players <- as.data.frame(
  feedback_result$players_intime[200, , ]
)

niche_players <- as.data.frame(
  niche_result$players_intime[200, , ]
)

balanced_players <- as.data.frame(
  balanced_result$players_intime[200, , ]
)

median_reliance_feedback <- feedback_result$median_AI_belief[200000]
median_reliance_niche <- niche_result$median_AI_belief[200000]
median_reliance_balanced <- balanced_result$median_AI_belief[200000]

scatter_theme <- theme_classic(
  base_family = "Arial",
  base_size = 23
) +
  theme(
    panel.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 1.2
    ),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_text(
      size = 28,
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      size = 28,
      margin = margin(r = 12)
    ),
    axis.text = element_text(
      size = 22,
      color = "black"
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 1.1
    ),
    axis.ticks.length = grid::unit(0.24, "cm"),
    plot.title = element_text(
      size = 28,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 10)
    )
  )

make_belief_plot <- function(data, incentive, y_label) {
  ggplot(
    data,
    aes(x = V1, y = V2)
  ) +
    geom_point(
      size = 2.4,
      color = incentive_colors[incentive],
      alpha = 0.3
    ) +
    geom_abline(
      intercept = 5,
      slope = -0.2,
      linetype = "dashed",
      linewidth = 2,
      color = "black"
    ) +
    scale_x_continuous(
      limits = c(0, 50),
      breaks = c(0, 25, 50),
      expand = expansion(add = 1.5)
    ) +
    scale_y_continuous(
      limits = c(-16, 16),
      breaks = c(-15, 0, 15),
      expand = expansion(add = 1)
    ) +
    labs(
      title = incentive,
      x = NULL,
      y = y_label
    ) +
    scatter_theme
}

make_reliance_plot <- function(
    data,
    incentive,
    median_reliance,
    y_label
) {
  ggplot(
    data,
    aes(x = V1, y = V3)
  ) +
    geom_point(
      size = 2.4,
      color = incentive_colors[incentive],
      alpha = 0.3
    ) +
    geom_hline(
      yintercept = median_reliance,
      linetype = "dashed",
      linewidth = 2,
      color = "black"
    ) +
    scale_x_continuous(
      limits = c(0, 50),
      breaks = c(0, 25, 50),
      expand = expansion(add = 1.5)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = c("0.0", "0.5", "1.0"),
      expand = expansion(add = 0.04)
    ) +
    labs(
      x = "Interest",
      y = y_label
    ) +
    scatter_theme
}

belief_feedback <- make_belief_plot(
  feedback_players,
  "Feedback",
  "Belief"
)

belief_niche <- make_belief_plot(
  niche_players,
  "Niche-expert",
  NULL
)

belief_balanced <- make_belief_plot(
  balanced_players,
  "Balanced",
  NULL
)

reliance_feedback <- make_reliance_plot(
  feedback_players,
  "Feedback",
  median_reliance_feedback,
  "Reliance on AI"
)

reliance_niche <- make_reliance_plot(
  niche_players,
  "Niche-expert",
  median_reliance_niche,
  NULL
)

reliance_balanced <- make_reliance_plot(
  balanced_players,
  "Balanced",
  median_reliance_balanced,
  NULL
)

belief_row <- belief_feedback +
  belief_niche +
  belief_balanced +
  plot_layout(ncol = 3)

reliance_row <- reliance_feedback +
  reliance_niche +
  reliance_balanced +
  plot_layout(
    ncol = 3,
    axis_titles = "collect_x"
  )

final_plot <- (
  trajectory_row /
    belief_row /
    reliance_row +
    plot_layout(
      heights = c(1.25, 1, 1)
    ) +
    plot_annotation(
      tag_levels = "A"
    )
) &
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.tag = element_text(
      family = "Arial",
      size = 28,
      face = "bold",
      color = "black",
      hjust = 0,
      vjust = 1
    ),
    plot.tag.position = c(0.02, 0.12),
    plot.tag.location = "panel"
  )

ggsave(
  file.path(
    save_path,
    "Figure 4.pdf"
  ),
  final_plot,
  width = 16,
  height = 16,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)
