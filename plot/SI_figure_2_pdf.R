library(ggplot2)
library(dplyr)
library(patchwork)

project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"

answers_feedback_path <- file.path(
  project_path,
  "AI_answers_question/Feedback_biassweep"
)

answers_niche_path <- file.path(
  project_path,
  "AI_answers_question/Nicheexpert_biassweep"
)

knows_feedback_path <- file.path(
  project_path,
  "AI_knows_all/Feedback_repeat"
)

knows_niche_path <- file.path(
  project_path,
  "AI_knows_all/Nicheexpert_repeat"
)

bias_list <- seq(-0.6, 0.6, by = 0.1)

read_stationary <- function(path, filename) {
  df <- bind_rows(lapply(bias_list, function(i) {
    env <- new.env()
    
    load(
      file.path(path, sprintf(filename, 1, 0, i)),
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

answers_feedback <- read_stationary(
  answers_feedback_path,
  "adv_feedback_k%02d_i%03d_j%02f.RData"
)

answers_niche <- read_stationary(
  answers_niche_path,
  "adv_niche_k%02d_i%03d_j%02f.RData"
)

knows_feedback <- read_stationary(
  knows_feedback_path,
  "adv_feedback_k%02d_i%03d_j%02f.RData"
)

knows_niche <- read_stationary(
  knows_niche_path,
  "adv_feedback_k%02d_i%03d_j%02f.RData"
)

line_width <- 1
point_size <- 3.4
color1 <- "#00658d"

x_scale <- scale_x_continuous(
  breaks = c(-0.6, -0.3, 0, 0.3, 0.6)
)

single_theme <- theme_classic(base_size = 11) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.65
    ),
    axis.title.x = element_text(
      size = 12,
      margin = margin(t = 6)
    ),
    axis.text = element_text(
      size = 10,
      color = "black"
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.5
    ),
    plot.title = element_text(
      size = 11.5,
      hjust = 0.5,
      margin = margin(b = 4)
    ),
    plot.margin = margin(4, 4, 4, 4)
  )

make_metric_plot <- function(
    data,
    metric,
    title,
    ylim = NULL,
    hide_x = FALSE,
    reference = FALSE,
    reference_y = 1
) {
  p <- ggplot(
    data,
    aes(
      x = bias_i,
      y = .data[[metric]]
    )
  )
  
  if (reference) {
    p <- p +
      geom_hline(
        yintercept = reference_y,
        linetype = "dashed",
        linewidth = 0.8,
        color = "#7E7E7E"
      )
  }
  
  p <- p +
    geom_line(
      linewidth = line_width,
      color = color1
    ) +
    geom_point(
      size = point_size,
      color = color1
    ) +
    x_scale +
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
        axis.ticks.x = element_blank()
      )
  }
  
  p
}

make_block <- function(
    data,
    variance_limit,
    bias_limit,
    reference_y = 1
) {
  p_accuracy <- make_metric_plot(
    data,
    "accuracy",
    "Collective accuracy",
    ylim = c(0, 1),
    hide_x = TRUE,
    reference = TRUE,
    reference_y = reference_y
  )
  
  p_hacc <- make_metric_plot(
    data,
    "human_accuracy",
    "Counterfactual human CI",
    hide_x = TRUE,
    reference = TRUE,
    reference_y = reference_y
  )
  
  p_belief <- make_metric_plot(
    data,
    "median_AI_belief",
    "Median reliance on AI",
    ylim = c(0, 1),
    hide_x = TRUE
  )
  
  p_var <- make_metric_plot(
    data,
    "variance",
    "Collective variance",
    ylim = variance_limit
  ) +
    labs(x = NULL)
  
  p_bias <- make_metric_plot(
    data,
    "bias_sq",
    "Collective bias",
    ylim = bias_limit
  )
  
  p_div <- make_metric_plot(
    data,
    "interest_diversity",
    "Interest Diversity",
    ylim = c(0, 51)
  ) +
    labs(x = NULL)
  
  (
    p_accuracy | p_hacc | p_belief
  ) / (
    p_var | p_bias | p_div
  )
}

make_header <- function(label) {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = label,
      size = 6.2,
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
      plot.margin = margin(0, 20, 6, 20)
    )
}

make_row_label <- function(label) {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = label,
      size = 5.3,
      fontface = "bold",
      lineheight = 0.9
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void() +
    theme(
      panel.background = element_rect(
        fill = "grey88",
        color = NA
      ),
      plot.margin = margin(25, 16, 25, 8)
    )
}

block_knows_feedback <- make_block(
  knows_feedback,
  variance_limit = c(0, 160),
  bias_limit = c(0, 160),
  reference_y = 1
)

block_knows_niche <- make_block(
  knows_niche,
  variance_limit = c(0, 160),
  bias_limit = c(0, 160),
  reference_y = 0
)

block_answers_feedback <- make_block(
  answers_feedback,
  variance_limit = c(0, 1000),
  bias_limit = c(0, 800),
  reference_y = 1
)

block_answers_niche <- make_block(
  answers_niche,
  variance_limit = c(0, 1000),
  bias_limit = c(0, 800),
  reference_y = 1
)

feedback_header <- make_header("Feedback")
niche_header <- make_header("Niche expert")

knows_label <- make_row_label("Omniscient AI")
answers_label <- make_row_label("Chatbot AI")

layout_design <- "
ABCD
EFGH
IIII
JKLM
"

layout_widths <- c(0.25, 1, 0.015, 1)
layout_heights <- c(0.12, 1, 0.025, 1)

base_plot <- (
  plot_spacer() +
    feedback_header +
    plot_spacer() +
    niche_header +
    knows_label +
    block_knows_feedback +
    plot_spacer() +
    block_knows_niche +
    plot_spacer() +
    answers_label +
    block_answers_feedback +
    plot_spacer() +
    block_answers_niche +
    plot_layout(
      design = layout_design,
      widths = layout_widths,
      heights = layout_heights
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
  )

ggsave(
  file.path(project_path, "Supplementary_Figure_2.pdf"),
  base_plot,
  width = 18,
  height = 10.5,
  device = grDevices::cairo_pdf,
  units = "in",
  bg = "white"
)