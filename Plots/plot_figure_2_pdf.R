if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("patchwork")) install.packages("patchwork")
if (!require("here")) install.packages("here")

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(here)

idx <- unique(c(seq(1, 200000, by = 100), 200000))

trajectory_order <- c(
  "Without AI",
  "With AI",
  "Counterfactual collective accuracy"
)

incentive_order <- c("Feedback", "Niche-expert")

trajectory_colors <- c(
  "Without AI" = "#0072B2",
  "With AI" = "#D55E00",
  "Counterfactual collective accuracy" = "#999999"
)

read_result <- function(file) {
  env <- new.env()
  load(here(file), envir = env)
  env$Result
}

make_panel_data <- function(original_pattern, ai_pattern, ai_model, incentive) {
  df <- bind_rows(lapply(1:30, function(i) {
    original <- read_result(sprintf(original_pattern, i))
    ai <- read_result(sprintf(ai_pattern, i))
    data.frame(
      Replicate = i,
      Generation = idx,
      `Without AI` = original$accuracy[idx],
      `With AI` = ai$accuracy[idx],
      `Counterfactual collective accuracy` = ai$human_accuracy[idx],
      check.names = FALSE
    ) %>%
      pivot_longer(
        -c(Replicate, Generation),
        names_to = "Trajectory",
        values_to = "Accuracy"
      )
  }))
  
  df %>%
    group_by(Generation, Trajectory) %>%
    summarise(
      Lower = quantile(Accuracy, 0.025, na.rm = TRUE),
      Upper = quantile(Accuracy, 0.975, na.rm = TRUE),
      Accuracy = median(Accuracy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Trajectory = factor(Trajectory, levels = trajectory_order),
      Incentive = factor(incentive, levels = incentive_order),
      AI_model = ai_model
    )
}

df_acc <- bind_rows(
  make_panel_data(
    "Simulations/Figure2,S1,S3,S5_simulation/without_feedback_avg_i%2d.RData",
    "Simulations/Figure2,S1,S3,S5_simulation/omni_feedback_0.4_i%2d.RData",
    "AI knows all",
    "Feedback"
  ),
  make_panel_data(
    "Simulations/Figure2,S1,S3,S5_simulation/without_niche_avg_i%2d.RData",
    "Simulations/Figure2,S1,S3,S5_simulation/omni_niche_0.4_i%2d.RData",
    "AI knows all",
    "Niche-expert"
  ),
  make_panel_data(
    "Simulations/Figure2,S1,S3,S5_simulation/without_feedback_clu_i%2d.RData",
    "Simulations/Figure2,S1,S3,S5_simulation/chatbot_feedback_0.4_i%2d.RData",
    "AI answers question",
    "Feedback"
  ),
  make_panel_data(
    "Simulations/Figure2,S1,S3,S5_simulation/without_niche_clu_i%2d.RData",
    "Simulations/Figure2,S1,S3,S5_simulation/chatbot_niche_0.4_i%2d.RData",
    "AI answers question",
    "Niche-expert"
  )
)

make_panel <- function(data, incentive_name, tag, show_y_axis = FALSE) {
  p <- data %>%
    filter(Incentive == incentive_name) %>%
    ggplot(aes(
      Generation,
      Accuracy,
      color = Trajectory,
      fill = Trajectory
    )) +
    geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.9,
      color = "black"
    ) +
    geom_ribbon(
      aes(
        ymin = Lower,
        ymax = Upper,
        group = Trajectory
      ),
      alpha = 0.18,
      color = NA
    ) +
    geom_line(
      aes(group = factor(
        Trajectory,
        levels = c(
          "Counterfactual collective accuracy",
          "Without AI",
          "With AI"
        )
      )),
      linewidth = 2,
      lineend = "round"
    ) +
    annotate(
      "text",
      x = 5000,
      y = -1.0,
      label = tag,
      hjust = 0,
      vjust = 0,
      size = 6,
      family = "Arial",
      fontface = "bold"
    ) +
    scale_color_manual(
      values = trajectory_colors,
      breaks = trajectory_order
    ) +
    scale_fill_manual(
      values = trajectory_colors,
      breaks = trajectory_order
    ) +
    scale_x_continuous(
      breaks = seq(0, 200000, by = 40000),
      labels = seq(0, 20, by = 4),
      expand = expansion(mult = c(0, 0.005))
    ) +
    scale_y_continuous(
      breaks = c(-1, -0.5, 0, 0.5, 1),
      labels = sprintf(
        "%.2f",
        c(-1, -0.5, 0, 0.5, 1)
      ),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(
      xlim = c(0, 200000),
      ylim = c(-1.07, 1.07)
    ) +
    labs(
      x = NULL,
      y = if (show_y_axis) "Collective accuracy" else NULL,
      color = NULL
    ) +
    guides(
      color = guide_legend(
        nrow = 1,
        override.aes = list(linewidth = 1.8)
      ),
      fill = "none"
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
      legend.text = element_text(size = 14),
      legend.key.width = grid::unit(1.8, "cm"),
      legend.spacing.x = grid::unit(0.4, "cm"),
      plot.margin = margin(0, 6, 0, 6)
    )
  
  if (!show_y_axis) {
    p <- p +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }
  
  p
}

plot_A <- df_acc %>%
  filter(AI_model == "AI knows all") %>%
  make_panel("Feedback", "A", TRUE)

plot_B <- df_acc %>%
  filter(AI_model == "AI knows all") %>%
  make_panel("Niche-expert", "B")

plot_C <- df_acc %>%
  filter(AI_model == "AI answers question") %>%
  make_panel("Feedback", "C", TRUE)

plot_D <- df_acc %>%
  filter(AI_model == "AI answers question") %>%
  make_panel("Niche-expert", "D")

make_box <- function(label, size, box_margin = margin()) {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = label,
      size = size,
      family = "Arial",
      fontface = "bold",
      hjust = 0.5
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void() +
    theme(
      panel.background = element_rect(
        fill = "grey88",
        color = NA
      ),
      plot.margin = box_margin
    )
}

feedback_header <- make_box(
  "Feedback",
  size = 7,
  box_margin = margin(0, 6, 0, 6)
)

niche_header <- make_box(
  "Niche-expert",
  size = 7,
  box_margin = margin(0, 6, 0, 6)
)

omniscient_label <- ggplot() +
  annotate(
    "text",
    x = 0.5,
    y = 0.56,
    label = "Omniscient AI",
    size = 7.5,
    family = "Arial",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.43,
    label = "(Averaging)",
    size = 6,
    family = "Arial",
    fontface = "bold"
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = "grey88",
      color = NA
    ),
    plot.margin = margin(0, 14, 0, 0)
  )

chatbot_label <- ggplot() +
  annotate(
    "text",
    x = 0.5,
    y = 0.56,
    label = "Chatbot AI",
    size = 7.5,
    family = "Arial",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.43,
    label = "(Clustering)",
    size = 6,
    family = "Arial",
    fontface = "bold"
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = "grey88",
      color = NA
    ),
    plot.margin = margin(0, 14, 0, 0)
  )

x_title <- wrap_elements(
  full = grid::textGrob(
    expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    gp = grid::gpar(
      fontfamily = "Arial",
      fontsize = 17
    )
  ),
  clip = FALSE
)

layout_design <- "
ABC
DEF
GGG
HIJ
KLL
MMM
"

combined_plot <- (
  plot_spacer() +
    feedback_header +
    niche_header +
    omniscient_label +
    plot_A +
    plot_B +
    plot_spacer() +
    chatbot_label +
    plot_C +
    plot_D +
    plot_spacer() +
    x_title +
    guide_area() +
    plot_layout(
      design = layout_design,
      widths = c(0.45, 1, 1),
      heights = c(0.14, 1, 0.10, 1, 0.12, 0.10),
      guides = "collect"
    )
) &
  theme(
    legend.position = "bottom",
    legend.justification = c(0.8, 0.5),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

ggsave(
  here("Figures", "Figure 2.pdf"),
  combined_plot,
  width = 14,
  height = 10.5,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)
