library(ggplot2)
library(dplyr)
library(patchwork)

save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/Figure4"

feedback_env <- new.env()
niche_env <- new.env()
balanced_env <- new.env()

load(file.path(save_path, "Feedback_sequential_reversed.RData"), envir = feedback_env)
load(file.path(save_path, "Niche_sequential_reversed.RData"), envir = niche_env)
load(file.path(save_path, "Balanced_sequential_reversed.RData"), envir = balanced_env)

feedback_result <- feedback_env$Result
niche_result <- niche_env$Result
balanced_result <- balanced_env$Result

generation <- seq(1000, 200000, by = 1000)

incentive_order <- c(
  "Feedback",
  "Niche expert",
  "Balanced"
)

incentive_colors <- c(
  "Feedback" = "#3381a3",
  "Niche expert" = "#fc8644",
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
    Incentive = "Niche expert"
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
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    ),
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
    legend.text = element_text(
      size = 20
    ),
    legend.key.width = grid::unit(2.1, "cm"),
    legend.spacing.x = grid::unit(0.5, "cm"),
    legend.margin = margin(t = 12)
  )

trajectory_accuracy <- ggplot(
  df_trajectory,
  aes(
    x = Generation,
    y = accuracy,
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
    breaks = c(0.4, 0.7, 1),
    limits = c(0.38, 1.02),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = "Collective accuracy",
    color = "Incentive structure"
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

trajectory_human <- ggplot(
  df_trajectory,
  aes(
    x = Generation,
    y = human_accuracy,
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
    breaks = c(-0.4, 0.3, 1.0),
    limits = c(-0.45, 1.05),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = "Counterfactual human CI",
    color = "Incentive structure"
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

scatter_theme <- theme_classic(
  base_family = "Arial",
  base_size = 23
) +
  theme(
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    ),
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

belief_feedback <- ggplot(
  feedback_players,
  aes(x = V1, y = V2)
) +
  geom_point(
    size = 2.4,
    color = incentive_colors["Feedback"],
    alpha = 0.3
  ) +
  scale_x_continuous(
    limits = c(0, 50),
    breaks = c(0, 25, 50),
    expand = expansion(add = 1.5)
  ) +
  scale_y_continuous(
    limits = c(-15, 15),
    breaks = c(-15, 0, 15),
    expand = expansion(add = 1)
  ) +
  labs(
    title = "Feedback",
    x = NULL,
    y = "Belief"
  ) +
  scatter_theme

belief_niche <- ggplot(
  niche_players,
  aes(x = V1, y = V2)
) +
  geom_point(
    size = 2.4,
    color = incentive_colors["Niche expert"],
    alpha = 0.3
  ) +
  scale_x_continuous(
    limits = c(0, 50),
    breaks = c(0, 25, 50),
    expand = expansion(add = 1.5)
  ) +
  scale_y_continuous(
    limits = c(-15, 15),
    breaks = c(-15, 0, 15),
    expand = expansion(add = 1)
  ) +
  labs(
    title = "Niche expert",
    x = NULL,
    y = NULL
  ) +
  scatter_theme

belief_balanced <- ggplot(
  balanced_players,
  aes(x = V1, y = V2)
) +
  geom_point(
    size = 2.4,
    color = incentive_colors["Balanced"],
    alpha = 0.3
  ) +
  scale_x_continuous(
    limits = c(0, 50),
    breaks = c(0, 25, 50),
    expand = expansion(add = 1.5)
  ) +
  scale_y_continuous(
    limits = c(-15, 15),
    breaks = c(-15, 0, 15),
    expand = expansion(add = 1)
  ) +
  labs(
    title = "Balanced",
    x = NULL,
    y = NULL
  ) +
  scatter_theme

reliance_feedback <- ggplot(
  feedback_players,
  aes(x = V1, y = V3)
) +
  geom_point(
    size = 2.4,
    color = incentive_colors["Feedback"],
    alpha = 0.3
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
    y = "Reliance on AI"
  ) +
  scatter_theme

reliance_niche <- ggplot(
  niche_players,
  aes(x = V1, y = V3)
) +
  geom_point(
    size = 2.4,
    color = incentive_colors["Niche expert"],
    alpha = 0.3
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
    y = NULL
  ) +
  scatter_theme

reliance_balanced <- ggplot(
  balanced_players,
  aes(x = V1, y = V3)
) +
  geom_point(
    size = 2.4,
    color = incentive_colors["Balanced"],
    alpha = 0.3
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
    y = NULL
  ) +
  scatter_theme

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

final_plot <- trajectory_row /
  belief_row /
  reliance_row +
  plot_layout(
    heights = c(1.25, 1, 1)
  ) &
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

ggsave(
  filename = file.path(
    save_path,
    "trajectory_and_player_distributions_s_r.png"
  ),
  plot = final_plot,
  width = 16,
  height = 16,
  dpi = 300,
  bg = "white"
)