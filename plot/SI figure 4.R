library(ggplot2)
library(patchwork)
library(cowplot)
library(grid)

project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-Intelligence-with-AI"

output_path <- file.path(
  project_path,
  "trajectory_and_accuracy_heatmaps.pdf"
)

font_family <- "Arial"

trajectory_axis_title_size <- 17
trajectory_axis_text_size <- 14
trajectory_legend_text_size <- 12

heatmap_axis_title_size <- 17
heatmap_axis_text_size <- 12
heatmap_title_size <- 18
heatmap_legend_title_size <- 15
heatmap_legend_text_size <- 12

panel_tag_size <- 21

trajectory_line_width <- 2
trajectory_legend_rows <- 2

left_tag_width <- 0.15
human_heatmap_width <- 0.12
heatmap_gap_width <- 0.4
ai_heatmap_width <- 1.62

trajectory_row_height <- 1.05
heatmap_row_height <- 1
heatmap_legend_row_height <- 0.22

pdf_width <- 11
pdf_height <- 14

heatmap_legend_bar_width <- 7
heatmap_legend_bar_height <- 0.45

extract_final <- function(x) {
  mean(x[190000:200000], na.rm = TRUE)
}

load_result <- function(file) {
  env <- new.env()
  load(file, envir = env)
  env$Result
}

read_mu_sweep <- function(save_path, mu_list, bias_list) {
  grid_df <- expand.grid(
    mu = mu_list,
    bias = bias_list
  )
  
  do.call(rbind, lapply(seq_len(nrow(grid_df)), function(k) {
    mu <- grid_df$mu[k]
    bias <- grid_df$bias[k]
    
    file <- file.path(
      save_path,
      sprintf(
        "Feedback_k%02d_i%02f_j%02f.RData",
        1,
        mu,
        bias
      )
    )
    
    result <- load_result(file)
    
    data.frame(
      bias = factor(bias, levels = bias_list),
      parameter = factor(mu, levels = mu_list),
      accuracy = extract_final(result$accuracy)
    )
  }))
}

read_belief_sweep <- function(save_path, belief_list, bias_list) {
  grid_df <- expand.grid(
    belief = belief_list,
    bias = bias_list
  )
  
  do.call(rbind, lapply(seq_len(nrow(grid_df)), function(k) {
    belief <- grid_df$belief[k]
    bias <- grid_df$bias[k]
    
    file <- file.path(
      save_path,
      sprintf(
        "Feedback_k%02d_i%02d_j%02f.RData",
        1,
        belief,
        bias
      )
    )
    
    result <- load_result(file)
    
    data.frame(
      bias = factor(bias, levels = bias_list),
      parameter = factor(belief, levels = belief_list),
      accuracy = extract_final(result$accuracy)
    )
  }))
}

heatmap_theme <- theme_classic() +
  theme(
    text = element_text(family = font_family),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.9
    ),
    axis.title = element_text(
      size = heatmap_axis_title_size
    ),
    axis.text = element_text(
      size = heatmap_axis_text_size
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(
      size = heatmap_title_size,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    legend.title = element_text(
      size = heatmap_legend_title_size,
      hjust = 0.5,
      lineheight = 0.9
    ),
    legend.text = element_text(
      size = heatmap_legend_text_size
    ),
    plot.margin = margin(12, 10, 10, 10)
  )

make_heatmap <- function(data, y_title, title_text) {
  ggplot(
    data,
    aes(
      x = bias,
      y = parameter,
      fill = accuracy
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 0.3
    ) +
    scale_x_discrete(
      expand = c(0, 0)
    ) +
    scale_y_discrete(
      expand = c(0, 0)
    ) +
    scale_fill_viridis_c(
      name = "Collective accuracy",
      limits = c(0, 1),
      labels = function(x) sprintf("%.2f", x)
    ) +
    labs(
      x = "Bias",
      y = y_title,
      title = title_text
    ) +
    heatmap_theme
}

make_tag_plot <- function(label) {
  ggdraw() +
    draw_label(
      label,
      x = 0.95,
      y = 0.98,
      hjust = 1,
      vjust = 1,
      fontfamily = font_family,
      fontface = "bold",
      size = panel_tag_size
    )
}

mu_list <- c(
  0, 0.01, 0.02, 0.03,
  0.04, 0.05, 0.06, 0.07
)

belief_list <- c(
  3, 4, 5, 6, 7, 8, 9, 10
)

ai_bias_list <- c(
  -0.6, -0.5, -0.4, -0.3,
  -0.2, -0.1, 0,
  0.1, 0.2, 0.3, 0.4, 0.5, 0.6
)

human_bias_list <- 0

df_acc_mu <- read_mu_sweep(
  file.path(
    project_path,
    "AI_answers_question/mu_sweep"
  ),
  mu_list,
  ai_bias_list
)

df_acc_b <- read_belief_sweep(
  file.path(
    project_path,
    "AI_answers_question/belief_sweep"
  ),
  belief_list,
  ai_bias_list
)

df_acc_mu_ori <- read_mu_sweep(
  file.path(
    project_path,
    "Original/mu_sweep"
  ),
  mu_list,
  human_bias_list
)

df_acc_b_ori <- read_belief_sweep(
  file.path(
    project_path,
    "Original/belief_sweep"
  ),
  belief_list,
  human_bias_list
)

p_acc_mu_ori <- make_heatmap(
  df_acc_mu_ori,
  y_title = "Mutation rate",
  title_text = "Human-only CI"
)

p_acc_mu <- make_heatmap(
  df_acc_mu,
  y_title = "Mutation rate",
  title_text = "AI-assisted CI"
)

p_acc_b_ori <- make_heatmap(
  df_acc_b_ori,
  y_title = "Belief SD",
  title_text = NULL
)

p_acc_b <- make_heatmap(
  df_acc_b,
  y_title = "Belief SD",
  title_text = NULL
)

tag_B <- make_tag_plot("B")
tag_C <- make_tag_plot("C")
tag_D <- make_tag_plot("D")
tag_E <- make_tag_plot("E")

trajectory_files <- c(
  file.path(
    project_path,
    "Original/mu_0.00.RData"
  ),
  file.path(
    project_path,
    "Original/mu_0.01.RData"
  ),
  file.path(
    project_path,
    "AI_answers_question/AI_mu_0.00_bias0.4_error0.3.RData"
  ),
  file.path(
    project_path,
    "AI_answers_question/AI_mu_0.01_bias0.4_error0.3.RData"
  )
)

trajectory_labels <- c(
  "Human-only CI, mutation rate 0",
  "Human-only CI, mutation rate 0.01",
  "AI-assisted CI, mutation rate 0",
  "AI-assisted CI, mutation rate 0.01"
)

trajectory_colors <- c(
  "Human-only CI, mutation rate 0" = "#333333",
  "Human-only CI, mutation rate 0.01" = "#3381A3",
  "AI-assisted CI, mutation rate 0" = "#E76F51",
  "AI-assisted CI, mutation rate 0.01" = "#7A5195"
)

trajectory_results <- lapply(
  trajectory_files,
  load_result
)

idx <- unique(c(
  seq(
    1,
    length(trajectory_results[[1]]$accuracy),
    by = 100
  ),
  length(trajectory_results[[1]]$accuracy)
))

trajectory_data <- do.call(
  rbind,
  lapply(seq_along(trajectory_results), function(i) {
    data.frame(
      Generation = idx,
      Accuracy = trajectory_results[[i]]$accuracy[idx],
      Trajectory = trajectory_labels[i]
    )
  })
)

trajectory_data$Trajectory <- factor(
  trajectory_data$Trajectory,
  levels = trajectory_labels
)

p_trj <- ggplot(
  trajectory_data,
  aes(
    x = Generation,
    y = Accuracy,
    color = Trajectory
  )
) +
  geom_line(
    linewidth = trajectory_line_width
  ) +
  scale_color_manual(
    values = trajectory_colors
  ) +
  scale_x_continuous(
    limits = c(0, 1000000),
    breaks = seq(
      0,
      1000000,
      by = 250000
    ),
    labels = c(
      "0", "25", "50", "75", "100"
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(
    color = guide_legend(
      nrow = trajectory_legend_rows,
      byrow = TRUE
    )
  ) +
  labs(
    x = expression("Generation (" * 10^4 * ")"),
    y = "Collective accuracy",
    color = NULL,
    tag = "A"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = font_family),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.9
    ),
    axis.title = element_text(
      size = trajectory_axis_title_size
    ),
    axis.text = element_text(
      size = trajectory_axis_text_size
    ),
    legend.position = "bottom",
    legend.justification = "center",
    legend.text = element_text(
      size = trajectory_legend_text_size
    ),
    legend.key.width = unit(1.5, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    plot.tag = element_text(
      size = panel_tag_size,
      face = "bold"
    ),
    plot.tag.position = c(0.01, 0.99),
    plot.margin = margin(12, 10, 5, 10)
  )

heatmap_legend <- cowplot::get_legend(
  p_acc_mu +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(
          heatmap_legend_bar_width,
          "cm"
        ),
        barheight = unit(
          heatmap_legend_bar_height,
          "cm"
        ),
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    theme(
      legend.position = "bottom",
      legend.justification = "center"
    )
)

p_acc_mu_ori <- p_acc_mu_ori +
  theme(legend.position = "none")

p_acc_mu <- p_acc_mu +
  theme(legend.position = "none")

p_acc_b_ori <- p_acc_b_ori +
  theme(legend.position = "none")

p_acc_b <- p_acc_b +
  theme(legend.position = "none")

legend_panel <- wrap_elements(
  full = heatmap_legend
)

layout_design <- "
AAAA
BHCI
DJEK
LLLL
"

p_combined <- wrap_plots(
  A = p_trj,
  B = tag_B,
  H = p_acc_mu_ori,
  C = tag_C,
  I = p_acc_mu,
  D = tag_D,
  J = p_acc_b_ori,
  E = tag_E,
  K = p_acc_b,
  L = legend_panel,
  design = layout_design,
  widths = c(
    left_tag_width,
    human_heatmap_width,
    heatmap_gap_width,
    ai_heatmap_width
  ),
  heights = c(
    trajectory_row_height,
    heatmap_row_height,
    heatmap_row_height,
    heatmap_legend_row_height
  )
)

ggsave(
  output_path,
  p_combined,
  width = pdf_width,
  height = pdf_height,
  device = grDevices::cairo_pdf,
  units = "in",
  bg = "white"
)
