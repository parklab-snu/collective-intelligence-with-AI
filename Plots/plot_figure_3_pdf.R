if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("patchwork")) install.packages("patchwork")
if (!require("here")) install.packages("here")

library(ggplot2)
library(dplyr)
library(patchwork)
library(here)

lambda_list <- c(-40, -30, -20, -10, 0, 10, 20, 30, 40)
bias_list <- c(-0.4, -0.2, 0.2, 0.4)

font_family <- "Arial"

x_title_size <- 16
y_title_size <- 16
axis_number_size <- 13
direction_text_size <- 4
legend_title_size <- 18
legend_text_size <- 16
panel_label_size <- 8
header_text_size <- 6.2
row_label_text_size <- 5.3
aggregation_text_size <- 4.3

line_width <- 1.2
point_size <- 4
panel_border_width <- 0.7

grid <- expand.grid(
  i = lambda_list,
  j = bias_list
)

case_specs <- list(
  omniscient_feedback = list(
    path = here("Simulations", "Figure3_simulation"),
    prefix = "omni_feedback",
    has_k = FALSE
  ),
  omniscient_niche = list(
    path = here("Simulations", "Figure3_simulation"),
    prefix = "omni_niche",
    has_k = FALSE
  ),
  chatbot_feedback = list(
    path = here("Simulations", "Figure3_simulation"),
    prefix = "chatbot_feedback",
    has_k = FALSE
  ),
  chatbot_niche = list(
    path = here("Simulations", "Figure3_simulation"),
    prefix = "chatbot_niche",
    has_k = FALSE
  )
)

read_stationary <- function(save_path, prefix, has_k) {
  df <- bind_rows(lapply(seq_len(nrow(grid)), function(k) {
    i <- grid$i[k]
    j <- grid$j[k]
    
    if (has_k) {
      filename <- sprintf(
        paste0(prefix, "_k%02d_i%03d_j%f.RData"),
        1, i, j
      )
    } else {
      filename <- sprintf(
        paste0(prefix, "_i%02f_j%02f.RData"),
        j, i
      )
    }
    
    env <- new.env()
    load(file.path(save_path, filename), envir = env)
    
    result <- env$Result
    
    data.frame(
      lambda = i,
      bias_i = j,
      Generation = seq_along(result$accuracy),
      accuracy = result$accuracy,
      median_AI_belief = result$median_AI_belief
    )
  }))
  
  df %>%
    filter(Generation >= 190001, Generation <= 200000) %>%
    group_by(lambda, bias_i) %>%
    summarise(
      accuracy = mean(accuracy),
      median_AI_belief = mean(median_AI_belief),
      .groups = "drop"
    ) %>%
    mutate(
      bias_i = factor(
        bias_i,
        levels = bias_list,
        labels = as.character(bias_list)
      )
    ) %>%
    arrange(lambda, bias_i)
}

omniscient_feedback <- read_stationary(
  case_specs$omniscient_feedback$path,
  case_specs$omniscient_feedback$prefix,
  case_specs$omniscient_feedback$has_k
)

omniscient_niche <- read_stationary(
  case_specs$omniscient_niche$path,
  case_specs$omniscient_niche$prefix,
  case_specs$omniscient_niche$has_k
)

chatbot_feedback <- read_stationary(
  case_specs$chatbot_feedback$path,
  case_specs$chatbot_feedback$prefix,
  case_specs$chatbot_feedback$has_k
)

chatbot_niche <- read_stationary(
  case_specs$chatbot_niche$path,
  case_specs$chatbot_niche$prefix,
  case_specs$chatbot_niche$has_k
)

bias_colors <- c(
  "-0.4" = "#00658d",
  "-0.2" = "#008f7b",
  "0.2" = "#5fab29",
  "0.4" = "#ffa600"
)

bias_shapes <- c(
  "-0.4" = 17,
  "-0.2" = 15,
  "0.2" = 16,
  "0.4" = 18
)

x_scale <- scale_x_continuous(
  breaks = c(-40, -20, 0, 20, 40)
)

single_theme <- theme_classic(
  base_family = font_family
) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = panel_border_width
    ),
    #aspect.ratio = 0.9,
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = y_title_size,
      family = font_family
    ),
    axis.text = element_text(
      size = axis_number_size,
      color = "black",
      family = font_family
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.5
    ),
    legend.title = element_text(
      size = legend_title_size,
      family = font_family
    ),
    legend.text = element_text(
      size = legend_text_size,
      family = font_family
    ),
    legend.key.height = grid::unit(0.6, "cm"),
    plot.margin = margin(0, 5, 0, 5)
  )

make_panel <- function(
    data,
    variable,
    y_label,
    panel_label
) {
  ggplot(
    data,
    aes(
      x = lambda,
      y = .data[[variable]],
      color = bias_i,
      shape = bias_i,
      group = bias_i
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      label = panel_label,
      hjust = 1.25,
      vjust = -0.45,
      size = panel_label_size,
      fontface = "bold",
      family = font_family,
      color = "black"
    ) +
    scale_color_manual(
      values = bias_colors,
      name = "Bias"
    ) +
    scale_shape_manual(
      values = bias_shapes,
      name = "Bias"
    ) +
    x_scale +
    scale_y_continuous(
      breaks = c(0, 0.5, 1),
      labels = c("0.0", "0.5", "1.0"),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(
      ylim = c(-0.05, 1.05),
      clip = "on"
    ) +
    labs(
      x = NULL,
      y = y_label
    ) +
    single_theme
}

make_x_annotation <- function() {
  ggplot() +
    annotate(
      "text",
      x = 0,
      y = 0.82,
      label = "\u03bb",
      size = x_title_size / ggplot2::.pt,
      family = font_family,
      color = "black"
    ) +
    annotate(
      "segment",
      x = -33,
      xend = 33,
      y = 0.50,
      yend = 0.52,
      linewidth = 0.7,
      color = "black",
      arrow = grid::arrow(
        ends = "both",
        type = "closed",
        length = grid::unit(0.11, "in")
      )
    ) +
    annotate(
      "text",
      x = -28,
      y = 0.13,
      label = "Penalize",
      size = direction_text_size,
      family = font_family,
      color = "black"
    ) +
    annotate(
      "text",
      x = 28,
      y = 0.13,
      label = "Incentivize",
      size = direction_text_size,
      family = font_family,
      color = "black"
    ) +
    scale_x_continuous(
      limits = c(-40, 40),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0))
    ) +
    theme_void(
      base_family = font_family
    ) +
    theme(
      plot.margin = margin(0, 5, 0, 5)
    )
}

make_block <- function(data, panel_labels) {
  p_accuracy <- make_panel(
    data,
    "accuracy",
    "Collective accuracy",
    panel_labels[1]
  )
  
  p_belief <- make_panel(
    data,
    "median_AI_belief",
    "Median reliance on AI",
    panel_labels[2]
  )
  
  (
    p_accuracy /
      make_x_annotation() +
      plot_layout(heights = c(1, 0.22))
  ) |
    (
      p_belief /
        make_x_annotation() +
        plot_layout(heights = c(1, 0.22))
    )
}

make_header <- function(label) {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = label,
      size = header_text_size,
      family = font_family,
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
      plot.margin = margin(0, 18, -20, 18)
    )
}

make_row_label <- function(label, aggregation) {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.54,
      label = label,
      size = row_label_text_size,
      family = font_family,
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.43,
      label = aggregation,
      size = aggregation_text_size,
      family = font_family,
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
      plot.margin = margin(30, 12, 30, 5)
    )
}

block_omniscient_feedback <- make_block(
  omniscient_feedback,
  c("A", "B")
)

block_omniscient_niche <- make_block(
  omniscient_niche,
  c("C", "D")
)

block_chatbot_feedback <- make_block(
  chatbot_feedback,
  c("E", "F")
)

block_chatbot_niche <- make_block(
  chatbot_niche,
  c("G", "H")
)

feedback_header <- make_header("Feedback")
niche_header <- make_header("Niche-expert")

omniscient_label <- make_row_label(
  "Omniscient AI",
  "(Averaging)"
)

chatbot_label <- make_row_label(
  "Chatbot AI",
  "(Clustering)"
)

base_plot <- (
  plot_spacer() +
    feedback_header +
    plot_spacer() +
    niche_header +
    omniscient_label +
    block_omniscient_feedback +
    plot_spacer() +
    block_omniscient_niche +
    plot_spacer() +
    chatbot_label +
    block_chatbot_feedback +
    plot_spacer() +
    block_chatbot_niche +
    plot_layout(
      design = "
      ABCD
      EFGH
      IIII
      JKLM
      ",
      widths = c(0.25, 1, 0.015, 1),
      heights = c(0.16, 1, 0, 1),
      guides = "collect"
    )
) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.margin = margin(0)
    )
  ) &
  theme(
    legend.position = "right"
  )

ggsave(
  here("Figures", "Figure 3.pdf"),
  base_plot,
  width = 18,
  height = 9,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)

