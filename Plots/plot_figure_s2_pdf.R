if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("patchwork")) install.packages("patchwork")
if (!require("tidyr")) install.packages("tidyr")
if (!require("ggh4x")) install.packages("ggh4x")
if (!require("here")) install.packages("here")

library(ggh4x)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(here)

simulation_path <- here("Simulations","FigureS2,S5_simulation")

bias_list <- seq(-0.6, 0.6, by = 0.1)

metric_order <- c(
  "\nCollective accuracy",
  "Counterfactual\ncollective accuracy",
  "\nMedian reliance on AI",
  "Collective variance",
  "Collective bias",
  "Interest Diversity"
)

read_stationary <- function(path, filename) {
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
    filter(
      Generation >= 190001,
      Generation <= 200000
    ) %>%
    group_by(bias_i) %>%
    summarise(
      accuracy = mean(accuracy, na.rm = TRUE),
      human_accuracy = mean(human_accuracy, na.rm = TRUE),
      median_AI_belief = mean(median_AI_belief, na.rm = TRUE),
      bias_sq = mean(bias_sq, na.rm = TRUE),
      variance = mean(variance, na.rm = TRUE),
      interest_diversity = mean(interest_diversity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(bias_i)
}

to_long <- function(data) {
  data %>%
    rename(
      `\nCollective accuracy` = accuracy,
      `Counterfactual\ncollective accuracy` = human_accuracy,
      `\nMedian reliance on AI` = median_AI_belief,
      `Collective variance` = variance,
      `Collective bias` = bias_sq,
      `Interest Diversity` = interest_diversity
    ) %>%
    pivot_longer(
      cols = -bias_i,
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    mutate(
      Metric = factor(
        Metric,
        levels = metric_order
      )
    )
}

answers_feedback <- read_stationary(
  simulation_path,
  "chatbot_feedback_i%02f.RData"
) %>%
  to_long()

answers_niche <- read_stationary(
  simulation_path,
  "chatbot_niche_i%02f.RData"
) %>%
  to_long()

knows_feedback <- read_stationary(
  simulation_path,
  "omni_feedback_i%02f.RData"
) %>%
  to_long()

knows_niche <- read_stationary(
  simulation_path,
  "omni_niche_i%02f.RData"
) %>%
  to_long()

line_width <- 1
point_size <- 3.4
color1 <- "#D55E00"

make_plot <- function(
    data,
    variance_limit,
    bias_limit,
    reference_y = 1
) {
  
  ggplot(
    data,
    aes(
      x = bias_i,
      y = Value
    )
  ) +
    geom_hline(
      data = data.frame(
        Metric = factor(
          c(
            "\nCollective accuracy",
            "Counterfactual\ncollective accuracy"
          ),
          levels = metric_order
        ),
        reference_y = c(
          reference_y,
          reference_y
        )
      ),
      aes(
        yintercept = reference_y
      ),
      linetype = "dashed",
      linewidth = 0.8,
      color = "#7E7E7E",
      inherit.aes = FALSE
    ) +
    geom_line(
      linewidth = line_width,
      color = color1
    ) +
    geom_point(
      size = point_size,
      color = color1
    ) +
    facet_wrap(
      ~ Metric,
      ncol = 3,
      scales = "free_y"
    ) +
    facetted_pos_scales(
      y = list(
        Metric == "\nCollective accuracy" ~
          scale_y_continuous(
            limits = c(0, 1),
            breaks = c(0, 0.25, 0.5, 0.75, 1)
          ),
        
        Metric == "\nMedian reliance on AI" ~
          scale_y_continuous(
            limits = c(0, 1),
            breaks = c(0, 0.25, 0.5, 0.75, 1)
          ),
        
        Metric == "Collective variance" ~
          scale_y_continuous(
            limits = variance_limit
          ),
        
        Metric == "Collective bias" ~
          scale_y_continuous(
            limits = bias_limit
          ),
        
        Metric == "Interest Diversity" ~
          scale_y_continuous(
            limits = c(0, 51)
          )
      )
    ) +
    scale_x_continuous(
      limits = c(-0.6, 0.6),
      breaks = c(-0.6, -0.3, 0, 0.3, 0.6),
      labels = c(
        "-0.6",
        "-0.3",
        "0",
        "0.3",
        "0.6"
      ),
      expand = expansion(
        mult = c(0.03, 0.03)
      )
    ) +
    labs(
      x = "Bias",
      y = NULL
    ) +
    theme_classic(
      base_family = "Arial",
      base_size = 11
    ) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
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
      strip.text = element_text(
        size = 11.5,
        family = "Arial"
      ),
      strip.background = element_blank(),
      panel.spacing.x = grid::unit(
        0.55,
        "cm"
      ),
      panel.spacing.y = grid::unit(
        0.45,
        "cm"
      ),
      plot.margin = margin(4)
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
      plot.margin = margin(
        0,
        20,
        6,
        20
      )
    )
}

make_row_label <- function(
    label,
    aggregation
) {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.56,
      label = label,
      size = 5.3,
      family = "Arial",
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.43,
      label = aggregation,
      size = 4.3,
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
      plot.margin = margin(
        25,
        16,
        25,
        8
      )
    )
}

plot_knows_feedback <- make_plot(
  knows_feedback,
  variance_limit = c(0, 160),
  bias_limit = c(0, 160),
  reference_y = 1
)

plot_knows_niche <- make_plot(
  knows_niche,
  variance_limit = c(0, 160),
  bias_limit = c(0, 160),
  reference_y = 0
)

plot_answers_feedback <- make_plot(
  answers_feedback,
  variance_limit = c(0, 1000),
  bias_limit = c(0, 800),
  reference_y = 1
)

plot_answers_niche <- make_plot(
  answers_niche,
  variance_limit = c(0, 1000),
  bias_limit = c(0, 800),
  reference_y = 1
)

feedback_header <- make_header(
  "Feedback"
)

niche_header <- make_header(
  "Niche-expert"
)

knows_label <- make_row_label(
  "Omniscient AI",
  "(Averaging)"
)

answers_label <- make_row_label(
  "Chatbot AI",
  "(Clustering)"
)

base_plot <- (
  plot_spacer() +
    feedback_header +
    plot_spacer() +
    niche_header +
    
    knows_label +
    plot_knows_feedback +
    plot_spacer() +
    plot_knows_niche +
    
    plot_spacer() +
    answers_label +
    plot_answers_feedback +
    plot_spacer() +
    plot_answers_niche +
    
    plot_layout(
      design = "
      ABCD
      EFGH
      IIII
      JKLM
      ",
      widths = c(
        0.25,
        1,
        0.015,
        1
      ),
      heights = c(
        0.12,
        1,
        0.025,
        1
      )
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
  here("Figures", "Supplementary figure 2.pdf"),
  base_plot,
  width = 18,
  height = 10.5,
  device = grDevices::cairo_pdf,
  units = "in",
  bg = "white"
)
