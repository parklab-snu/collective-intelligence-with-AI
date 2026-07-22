library(ggplot2)
library(dplyr)
library(tidyr)

save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/"

idx <- unique(c(seq(1, 200000, by = 100), 200000))

trajectory_order <- c(
  "Human-only CI",
  "AI-assisted CI",
  "Counterfactual human CI"
)

read_result <- function(file) {
  data_env <- new.env()
  load(file.path(save_path, file), envir = data_env)
  data_env$Result
}

make_panel_data <- function(
    original_file,
    ai_file,
    ai_model,
    incentive
) {
  original_result <- read_result(original_file)
  ai_result <- read_result(ai_file)
  
  data.frame(
    Generation = idx,
    `Human-only CI` = original_result[["accuracy"]][idx],
    `AI-assisted CI` = ai_result[["accuracy"]][idx],
    `Counterfactual human CI` = ai_result[["human_accuracy"]][idx],
    check.names = FALSE
  ) %>%
    pivot_longer(
      cols = -Generation,
      names_to = "Trajectory",
      values_to = "Accuracy"
    ) %>%
    mutate(
      Trajectory = factor(
        Trajectory,
        levels = trajectory_order
      ),
      AI_model = ai_model,
      Incentive = incentive
    )
}

df_acc <- bind_rows(
  make_panel_data(
    "Original/avg_feedback.RData",
    "AI_knows_all/avg_feedback_Acc70_bias0.36_error0.3.RData",
    "AI knows all",
    "Feedback"
  ),
  make_panel_data(
    "Original/avg_niche.RData",
    "AI_knows_all/avg_niche_Acc70_bias0.36_error0.3.RData",
    "AI knows all",
    "Niche expert"
  ),
  make_panel_data(
    "Original/clu_feedback.RData",
    "AI_answers_question/clu_feedback_Acc70_bias0.36_error0.3.RData",
    "AI answers question",
    "Feedback"
  ),
  make_panel_data(
    "Original/clu_niche.RData",
    "AI_answers_question/clu_niche_Acc70_bias0.36_error0.3.RData",
    "AI answers question",
    "Niche expert"
  )
) %>%
  mutate(
    AI_model = factor(
      AI_model,
      levels = c(
        "AI knows all",
        "AI answers question"
      )
    ),
    Incentive = factor(
      Incentive,
      levels = c(
        "Feedback",
        "Niche expert"
      )
    )
  )

panel_tags <- data.frame(
  AI_model = factor(
    c(
      "AI knows all",
      "AI knows all",
      "AI answers question",
      "AI answers question"
    ),
    levels = c(
      "AI knows all",
      "AI answers question"
    )
  ),
  Incentive = factor(
    c(
      "Feedback",
      "Niche expert",
      "Feedback",
      "Niche expert"
    ),
    levels = c(
      "Feedback",
      "Niche expert"
    )
  ),
  Tag = c("A", "B", "C", "D")
)

trajectory_colors <- c(
  "Human-only CI" = "#298C8C",
  "AI-assisted CI" = "#A00000",
  "Counterfactual human CI" = "#A6A6A6"
)

plot <- ggplot(
  df_acc,
  aes(
    x = Generation,
    y = Accuracy,
    color = Trajectory
  )
) +
  geom_line(
    linewidth = 2,
    lineend = "round"
  ) +
  geom_text(
    data = panel_tags,
    aes(
      x = 5000,
      y = -0.45,
      label = Tag
    ),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0,
    size = 5.5,
    family = "Arial",
    fontface = "bold"
  ) +
  facet_grid(
    AI_model ~ Incentive,
    switch = "y"
  ) +
  scale_color_manual(
    values = trajectory_colors,
    breaks = trajectory_order
  ) +
  scale_x_continuous(
    breaks = seq(0, 200000, by = 40000),
    labels = seq(0, 20, by = 4),
    expand = expansion(mult = c(0, 0.005))
  ) +
  scale_y_continuous(
    breaks = seq(-0.5, 1, by = 0.5),
    expand = expansion(mult = c(0, 0)),
    position = "right"
  ) +
  coord_cartesian(
    xlim = c(0, 200000),
    ylim = c(-0.5, 1.07)
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = "Accuracy",
    color = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 1.8
      )
    )
  ) +
  theme_classic(
    base_family = "Arial",
    base_size = 15
  ) +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.9
    ),
    axis.title.x = element_text(
      size = 17,
      margin = margin(t = 10)
    ),
    axis.title.y.right = element_text(
      size = 17,
      margin = margin(l = 10)
    ),
    axis.text = element_text(
      size = 14,
      color = "black"
    ),
    axis.text.y.right = element_text(
      margin = margin(l = 5)
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.7
    ),
    axis.ticks.length = grid::unit(0.15, "cm"),
    strip.placement = "outside",
    strip.background.x = element_rect(
      fill = "grey88",
      color = NA
    ),
    strip.background.y = element_rect(
      fill = "grey94",
      color = NA
    ),
    strip.text.x = element_text(
      size = 17,
      face = "bold",
      margin = margin(
        t = 8,
        b = 8
      )
    ),
    strip.text.y.left = element_text(
      size = 15,
      face = "bold",
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      margin = margin(
        t = 8,
        r = 12,
        b = 8,
        l = 12
      )
    ),
    legend.position = "bottom",
    legend.text = element_text(
      size = 14
    ),
    legend.key.width = grid::unit(1.8, "cm"),
    legend.spacing.x = grid::unit(0.4, "cm"),
    legend.margin = margin(t = 10),
    panel.spacing = grid::unit(0.8, "lines"),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 8,
      l = 10
    )
  )

ggsave(
  filename = file.path(
    save_path,
    "accuracy_trajectories_ABCD_right_axis.png"
  ),
  plot = plot,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)


#--------------------
library(ggplot2)
library(dplyr)
library(tidyr)

save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/"

idx <- unique(c(seq(1, 200000, by = 100), 200000))

trajectory_order <- c(
  "Human-only CI",
  "AI-assisted CI",
  "Counterfactual human CI"
)

read_result <- function(file) {
  data_env <- new.env()
  load(file.path(save_path, file), envir = data_env)
  data_env$Result
}

make_panel_data <- function(
    original_file,
    ai_file,
    ai_model,
    incentive
) {
  original_result <- read_result(original_file)
  ai_result <- read_result(ai_file)
  
  data.frame(
    Generation = idx,
    `Human-only CI` = original_result[["accuracy"]][idx],
    `AI-assisted CI` = ai_result[["accuracy"]][idx],
    `Counterfactual human CI` = ai_result[["human_accuracy"]][idx],
    check.names = FALSE
  ) %>%
    pivot_longer(
      cols = -Generation,
      names_to = "Trajectory",
      values_to = "Accuracy"
    ) %>%
    mutate(
      Trajectory = factor(
        Trajectory,
        levels = trajectory_order
      ),
      AI_model = ai_model,
      Incentive = incentive
    )
}

df_acc <- bind_rows(
  make_panel_data(
    "Original/avg_feedback.RData",
    "AI_knows_all/avg_feedback_Acc70_bias0.36_error0.3.RData",
    "AI knows all",
    "Feedback"
  ),
  make_panel_data(
    "Original/avg_niche.RData",
    "AI_knows_all/avg_niche_Acc70_bias0.36_error0.3.RData",
    "AI knows all",
    "Niche expert"
  ),
  make_panel_data(
    "Original/clu_feedback.RData",
    "AI_answers_question/clu_feedback_Acc70_bias0.36_error0.3.RData",
    "AI answers question",
    "Feedback"
  ),
  make_panel_data(
    "Original/clu_niche.RData",
    "AI_answers_question/clu_niche_Acc70_bias0.36_error0.3.RData",
    "AI answers question",
    "Niche expert"
  )
) %>%
  mutate(
    AI_model = factor(
      AI_model,
      levels = c(
        "AI knows all",
        "AI answers question"
      )
    ),
    Incentive = factor(
      Incentive,
      levels = c(
        "Feedback",
        "Niche expert"
      )
    )
  )

panel_tags <- data.frame(
  AI_model = factor(
    c(
      "AI knows all",
      "AI knows all",
      "AI answers question",
      "AI answers question"
    ),
    levels = c(
      "AI knows all",
      "AI answers question"
    )
  ),
  Incentive = factor(
    c(
      "Feedback",
      "Niche expert",
      "Feedback",
      "Niche expert"
    ),
    levels = c(
      "Feedback",
      "Niche expert"
    )
  ),
  Tag = c("A", "B", "C", "D")
)

trajectory_colors <- c(
  "Human-only CI" = "#298C8C",
  "AI-assisted CI" = "#A00000",
  "Counterfactual human CI" = "#A6A6A6"
)

plot <- ggplot(
  df_acc,
  aes(
    x = Generation,
    y = Accuracy,
    color = Trajectory
  )
) +
  geom_line(
    linewidth = 2,
    lineend = "round"
  ) +
  geom_text(
    data = panel_tags,
    aes(
      x = 5000,
      y = -0.45,
      label = Tag
    ),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0,
    size = 5.5,
    family = "Arial",
    fontface = "bold"
  ) +
  facet_grid(
    AI_model ~ Incentive
  ) +
  scale_color_manual(
    values = trajectory_colors,
    breaks = trajectory_order
  ) +
  scale_x_continuous(
    breaks = seq(0, 200000, by = 40000),
    labels = seq(0, 20, by = 4),
    expand = expansion(mult = c(0, 0.005))
  ) +
  scale_y_continuous(
    breaks = seq(-0.5, 1, by = 0.5),
    expand = expansion(mult = c(0, 0)),
    position = "left"
  ) +
  coord_cartesian(
    xlim = c(0, 200000),
    ylim = c(-0.5, 1.07)
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = "Accuracy",
    color = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 1.8
      )
    )
  ) +
  theme_classic(
    base_family = "Arial",
    base_size = 15
  ) +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.9
    ),
    axis.title.x = element_text(
      size = 17,
      margin = margin(t = 10)
    ),
    axis.title.y.left = element_text(
      size = 17,
      margin = margin(r = 10)
    ),
    axis.text = element_text(
      size = 14,
      color = "black"
    ),
    axis.text.y.left = element_text(
      margin = margin(r = 5)
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.7
    ),
    axis.ticks.length = grid::unit(0.15, "cm"),
    strip.background.x = element_rect(
      fill = "grey88",
      color = NA
    ),
    strip.background.y = element_rect(
      fill = "grey94",
      color = NA
    ),
    strip.text.x = element_text(
      size = 17,
      face = "bold",
      margin = margin(
        t = 8,
        b = 8
      )
    ),
    strip.text.y.right = element_text(
      size = 15,
      face = "bold",
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      margin = margin(
        t = 8,
        r = 12,
        b = 8,
        l = 12
      )
    ),
    legend.position = "bottom",
    legend.text = element_text(
      size = 14
    ),
    legend.key.width = grid::unit(1.8, "cm"),
    legend.spacing.x = grid::unit(0.4, "cm"),
    legend.margin = margin(t = 10),
    panel.spacing = grid::unit(0.8, "lines"),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 8,
      l = 10
    )
  )

ggsave(
  filename = file.path(
    save_path,
    "accuracy_trajectories_ABCD_default_axis.png"
  ),
  plot = plot,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

#------------------
library(ggplot2)
library(dplyr)
library(tidyr)

save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/"

idx <- unique(c(seq(1, 200000, by = 100), 200000))

trajectory_order <- c(
  "Human-only CI",
  "AI-assisted CI",
  "Counterfactual human CI"
)

incentive_order <- c(
  "Feedback",
  "Niche expert"
)

read_result <- function(file) {
  data_env <- new.env()
  load(file.path(save_path, file), envir = data_env)
  data_env$Result
}

make_panel_data <- function(
    original_file,
    ai_file,
    ai_model,
    incentive
) {
  original_result <- read_result(original_file)
  ai_result <- read_result(ai_file)
  
  data.frame(
    Generation = idx,
    `Human-only CI` = original_result[["accuracy"]][idx],
    `AI-assisted CI` = ai_result[["accuracy"]][idx],
    `Counterfactual human CI` = ai_result[["human_accuracy"]][idx],
    check.names = FALSE
  ) %>%
    pivot_longer(
      cols = -Generation,
      names_to = "Trajectory",
      values_to = "Accuracy"
    ) %>%
    mutate(
      Trajectory = factor(
        Trajectory,
        levels = trajectory_order
      ),
      AI_model = ai_model,
      Incentive = factor(
        incentive,
        levels = incentive_order
      )
    )
}

df_acc <- bind_rows(
  make_panel_data(
    "Original/avg_feedback.RData",
    "AI_knows_all/avg_feedback_Acc70_bias0.36_error0.3.RData",
    "AI knows all",
    "Feedback"
  ),
  make_panel_data(
    "Original/avg_niche.RData",
    "AI_knows_all/avg_niche_Acc70_bias0.36_error0.3.RData",
    "AI knows all",
    "Niche expert"
  ),
  make_panel_data(
    "Original/clu_feedback.RData",
    "AI_answers_question/clu_feedback_Acc70_bias0.36_error0.3.RData",
    "AI answers question",
    "Feedback"
  ),
  make_panel_data(
    "Original/clu_niche.RData",
    "AI_answers_question/clu_niche_Acc70_bias0.36_error0.3.RData",
    "AI answers question",
    "Niche expert"
  )
)

trajectory_colors <- c(
  "Human-only CI" = "#298C8C",
  "AI-assisted CI" = "#A00000",
  "Counterfactual human CI" = "#A6A6A6"
)

make_accuracy_plot <- function(data, tags) {
  ggplot(
    data,
    aes(
      x = Generation,
      y = Accuracy,
      color = Trajectory
    )
  ) +
    geom_line(
      linewidth = 2,
      lineend = "round"
    ) +
    geom_text(
      data = tags,
      aes(
        x = 5000,
        y = -0.45,
        label = Tag
      ),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0,
      size = 5.5,
      family = "Arial",
      fontface = "bold"
    ) +
    facet_wrap(
      ~ Incentive,
      nrow = 1
    ) +
    scale_color_manual(
      values = trajectory_colors,
      breaks = trajectory_order
    ) +
    scale_x_continuous(
      breaks = seq(0, 200000, by = 40000),
      labels = seq(0, 20, by = 4),
      expand = expansion(mult = c(0, 0.005))
    ) +
    scale_y_continuous(
      breaks = seq(-0.5, 1, by = 0.5),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(
      xlim = c(0, 200000),
      ylim = c(-0.5, 1.07)
    ) +
    labs(
      x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
      y = "Accuracy",
      color = NULL
    ) +
    guides(
      color = guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(
          linewidth = 1.8
        )
      )
    ) +
    theme_classic(
      base_family = "Arial",
      base_size = 15
    ) +
    theme(
      axis.line = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.9
      ),
      axis.title.x = element_text(
        size = 17,
        margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        size = 17,
        margin = margin(r = 10)
      ),
      axis.text = element_text(
        size = 14,
        color = "black"
      ),
      axis.ticks = element_line(
        color = "black",
        linewidth = 0.7
      ),
      axis.ticks.length = grid::unit(0.15, "cm"),
      strip.background = element_rect(
        fill = "grey88",
        color = NA
      ),
      strip.text = element_text(
        size = 17,
        face = "bold",
        margin = margin(
          t = 8,
          b = 8
        )
      ),
      legend.position = "bottom",
      legend.text = element_text(
        size = 14
      ),
      legend.key.width = grid::unit(1.8, "cm"),
      legend.spacing.x = grid::unit(0.4, "cm"),
      legend.margin = margin(t = 10),
      panel.spacing = grid::unit(0.8, "lines"),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.margin = margin(
        t = 10,
        r = 12,
        b = 8,
        l = 10
      )
    )
}

tags_knows_all <- data.frame(
  Incentive = factor(
    c("Feedback", "Niche expert"),
    levels = incentive_order
  ),
  Tag = c("A", "B")
)

tags_answers_question <- data.frame(
  Incentive = factor(
    c("Feedback", "Niche expert"),
    levels = incentive_order
  ),
  Tag = c("C", "D")
)

plot_knows_all <- df_acc %>%
  filter(AI_model == "AI knows all") %>%
  make_accuracy_plot(tags_knows_all)

plot_answers_question <- df_acc %>%
  filter(AI_model == "AI answers question") %>%
  make_accuracy_plot(tags_answers_question)

ggsave(
  filename = file.path(
    save_path,
    "accuracy_trajectories_AI_knows_all.png"
  ),
  plot = plot_knows_all,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    save_path,
    "accuracy_trajectories_AI_answers_question.png"
  ),
  plot = plot_answers_question,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
