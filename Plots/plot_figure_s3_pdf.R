if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("patchwork")) install.packages("patchwork")
if (!require("ggh4x")) install.packages("ggh4x")


library(ggh4x)
library(ggplot2)
library(dplyr)
library(patchwork)

#Set your project path
project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"
save_path <- file.path(project_path, "Figures/")s

legend_order <- c(
  "Without AI",
  "With AI"
)

metric_order <- c(
  "Interest diversity",
  "Median reliance on AI",
  "Collective bias",
  "Collective variance"
)

my_colors <- c(
  "Without AI" = "#0072B2",
  "With AI" = "#D55E00"
)

make_plot_data <- function(original_file, ai_file, by = 100) {
  original_env <- new.env()
  ai_env <- new.env()
  
  load(file.path(project_path, original_file), envir = original_env)
  load(file.path(project_path, ai_file), envir = ai_env)
  
  original_result <- original_env$Result
  ai_result <- ai_env$Result
  
  idx <- seq(1, 200000, by = by)
  
  make_metric_df <- function(metric_name, metric_label) {
    data.frame(
      Generation = rep(idx, 2),
      Value = c(
        original_result[[metric_name]][idx],
        ai_result[[metric_name]][idx]
      ),
      source = rep(
        c("Without AI", "With AI"),
        each = length(idx)
      ),
      Metric = metric_label
    )
  }
  
  make_reliance_df <- function() {
    data.frame(
      Generation = rep(idx, 2),
      Value = c(
        rep(0, length(idx)),
        ai_result$median_AI_belief[idx]
      ),
      source = rep(
        c("Without AI", "With AI"),
        each = length(idx)
      ),
      Metric = "Median reliance on AI"
    )
  }
  
  bind_rows(
    make_metric_df(
      "interest_diversity",
      "Interest diversity"
    ),
    make_metric_df(
      "bias_sq",
      "Collective bias"
    ),
    make_metric_df(
      "variance",
      "Collective variance"
    ),
    make_reliance_df()
  ) %>%
    mutate(
      source = factor(
        source,
        levels = legend_order
      ),
      Metric = factor(
        Metric,
        levels = metric_order
      )
    )
}

make_plot <- function(data) {
  ggplot(
    data,
    aes(
      x = Generation,
      y = Value,
      color = source
    )
  ) +
    geom_line(
      linewidth = 1.5,
      lineend = "round"
    ) +
    facet_wrap(
      ~ Metric,
      ncol = 2,
      scales = "free_y"
    ) +
    facetted_pos_scales(
      y = list(
        Metric == "Interest diversity" ~
          scale_y_continuous(limits = c(0, 51))
      )
    ) +
    scale_color_manual(
      values = my_colors
    ) +
    scale_x_continuous(
      breaks = c(0, 40000, 80000, 120000, 160000, 200000),
      labels = c(0, 4, 8, 12, 16, 20)
    ) +
    labs(
      x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
      y = NULL
    ) +
    theme_classic(
      base_family = "Arial"
    ) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.position = "none",
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 1
      ),
      axis.title = element_text(
        size = 12,
        family = "Arial"
      ),
      axis.text = element_text(
        size = 12,
        family = "Arial"
      ),
      strip.text = element_text(
        size = 11,
        family = "Arial"
      ),
      strip.background = element_blank()
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
      plot.margin = margin(0, 25, 6, 20)
    )
}

make_row_label <- function(label, aggregation) {
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
      plot.margin = margin(25, 16, 25, 8)
    )
}

knows_feedback_data <- make_plot_data(
  "Simulations/Figure2,S1,S3,S5_simulation/without_feedback_avg.RData",
  "Simulations/Figure2,S1,S3,S5_simulation/omni_feedback_0.4.RData"
)

knows_niche_data <- make_plot_data(
  "Simulations/Figure2,S1,S3,S5_simulation/without_niche_avg.RData",
  "Simulations/Figure2,S1,S3,S5_simulation/omni_niche_0.4.RData"
)

answers_feedback_data <- make_plot_data(
  "Simulations/Figure2,S1,S3,S5_simulation/without_feedback_clu.RData",
  "Simulations/Figure2,S1,S3,S5_simulation/chatbot_feedback_0.4.RData"
)

answers_niche_data <- make_plot_data(
  "Simulations/Figure2,S1,S3,S5_simulation/without_niche_clu.RData",
  "Simulations/Figure2,S1,S3,S5_simulation/chatbot_niche_0.4.RData"
)

plot_knows_feedback <- make_plot(knows_feedback_data)
plot_knows_niche <- make_plot(knows_niche_data)
plot_answers_feedback <- make_plot(answers_feedback_data)
plot_answers_niche <- make_plot(answers_niche_data)

feedback_header <- make_header("Feedback")
niche_header <- make_header("Niche-expert")

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
      widths = c(0.32, 1, 0.015, 1),
      heights = c(0.14, 1.2, 0.025, 1.2)
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
  file.path(save_path, "Supplementary figure 3.pdf"),
  base_plot,
  width = 14,
  height = 10.5,
  device = grDevices::cairo_pdf,
  units = "in",
  bg = "white"
)

