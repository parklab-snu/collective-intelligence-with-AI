if (!require("ggplot2")) install.packages("ggplot2")
if (!require("cowplot")) install.packages("cowplot")
if (!require("patchwork")) install.packages("patchwork")
if (!require("grid")) install.packages("grid")

library(ggplot2)
library(patchwork)
library(cowplot)
library(grid)
#Set your project path
project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"
save_path <- file.path(project_path, "Figures/")

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

extract_final <- function(x) {
  mean(x[190000:200000], na.rm = TRUE)
}

load_result <- function(file) {
  env <- new.env()
  load(file, envir = env)
  env$Result
}

read_mu_sweep <- function(simulation_path, mu_list, bias_list) {
  grid_df <- expand.grid(
    mu = mu_list,
    bias = bias_list
  )
  
  do.call(rbind, lapply(seq_len(nrow(grid_df)), function(k) {
    mu <- grid_df$mu[k]
    bias <- grid_df$bias[k]
    
    result <- load_result(
      file.path(
        simulation_path,
        sprintf(
          "chatbot_feedback_i%02f_j%02f.RData",
          bias,
          mu
        )
      )
    )
    
    data.frame(
      bias = factor(bias, levels = bias_list),
      parameter = factor(mu, levels = mu_list),
      interest_diversity = extract_final(result$interest_diversity)
    )
  }))
}

read_belief_sweep <- function(simulation_path, belief_list, bias_list) {
  grid_df <- expand.grid(
    belief = belief_list,
    bias = bias_list
  )
  
  do.call(rbind, lapply(seq_len(nrow(grid_df)), function(k) {
    belief <- grid_df$belief[k]
    bias <- grid_df$bias[k]
    
    result <- load_result(
      file.path(
        simulation_path,
        sprintf(
          "chatbot_feedback_i%02f_j%02f.RData",
          bias,
          belief
        )
      )
    )
    
    data.frame(
      bias = factor(bias, levels = bias_list),
      parameter = factor(belief, levels = belief_list),
      interest_diversity = extract_final(result$interest_diversity)
    )
  }))
}

read_mu_sweep_ori <- function(simulation_path, mu_list) {
  do.call(rbind, lapply(mu_list, function(mu) {
    result <- load_result(
      file.path(
        simulation_path,
        sprintf(
          "without_feedback_j%02f.RData",
          mu
        )
      )
    )
    
    data.frame(
      bias = factor(0),
      parameter = factor(mu, levels = mu_list),
      interest_diversity = extract_final(result$interest_diversity)
    )
  }))
}

read_belief_sweep_ori <- function(simulation_path, belief_list) {
  do.call(rbind, lapply(belief_list, function(belief) {
    result <- load_result(
      file.path(
        simulation_path,
        sprintf(
          "without_feedback_j%02f.RData",
          belief
        )
      )
    )
    
    data.frame(
      bias = factor(0),
      parameter = factor(belief, levels = belief_list),
      interest_diversity = extract_final(result$interest_diversity)
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
      fill = interest_diversity
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
      name = "Interest diversity",
      limits = c(0, 51)
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

df_div_mu <- read_mu_sweep(
  file.path(
    project_path,
    "Simulations/FigureS4_simulation"
  ),
  mu_list,
  ai_bias_list
)

df_div_b <- read_belief_sweep(
  file.path(
    project_path,
    "Simulations/FigureS4_simulation"
  ),
  belief_list,
  ai_bias_list
)

df_div_mu_ori <- read_mu_sweep_ori(
  file.path(
    project_path,
    "Simulations/FigureS4_simulation"
  ),
  mu_list
)

df_div_b_ori <- read_belief_sweep_ori(
  file.path(
    project_path,
    "Simulations/FigureS4_simulation"
  ),
  belief_list
)

p_div_mu_ori <- make_heatmap(
  df_div_mu_ori,
  "Mutation rate",
  "Without AI"
)

p_div_mu <- make_heatmap(
  df_div_mu,
  "Mutation rate",
  "With AI"
)

p_div_b_ori <- make_heatmap(
  df_div_b_ori,
  "Belief SD",
  NULL
)

p_div_b <- make_heatmap(
  df_div_b,
  "Belief SD",
  NULL
)

trajectory_files <- c(
  file.path(
    project_path,
    "Simulations/FigureS4_simulation/without_feedback_clu_0.RData"
  ),
  file.path(
    project_path,
    "Simulations/FigureS4_simulation/without_feedback_clu_0.01.RData"
  ),
  file.path(
    project_path,
    "Simulations/FigureS4_simulation/chatbot_feedback_0.4_0.RData"
  ),
  file.path(
    project_path,
    "Simulations/FigureS4_simulation/chatbot_feedback_0.4_0.01.RData"
  )
)

trajectory_colors <- c(
  "Without AI" = "#3381A3",
  "With AI" = "#E76F51"
)

trajectory_linewidths <- c(
  "Mutation rate 0" = 2.2,
  "Mutation rate 0.01" = 1.1
)

trajectory_results <- lapply(
  trajectory_files,
  load_result
)

idx <- unique(c(
  seq(
    1,
    length(trajectory_results[[1]]$interest_diversity),
    by = 100
  ),
  length(trajectory_results[[1]]$interest_diversity)
))

trajectory_ai <- c(
  "Without AI",
  "Without AI",
  "With AI",
  "With AI"
)

trajectory_mutation <- c(
  "Mutation rate 0",
  "Mutation rate 0.01",
  "Mutation rate 0",
  "Mutation rate 0.01"
)

trajectory_data <- do.call(
  rbind,
  lapply(seq_along(trajectory_results), function(i) {
    data.frame(
      Generation = idx,
      Interest_diversity = trajectory_results[[i]]$interest_diversity[idx],
      AI = trajectory_ai[i],
      Mutation_rate = trajectory_mutation[i]
    )
  })
)

trajectory_data$AI <- factor(
  trajectory_data$AI,
  levels = c("Without AI", "With AI")
)

trajectory_data$Mutation_rate <- factor(
  trajectory_data$Mutation_rate,
  levels = c("Mutation rate 0", "Mutation rate 0.01")
)

p_trj <- ggplot(
  trajectory_data,
  aes(
    x = Generation,
    y = Interest_diversity,
    color = AI,
    linewidth = Mutation_rate
  )
) +
  geom_line() +
  scale_color_manual(
    values = trajectory_colors
  ) +
  scale_linewidth_manual(
    values = trajectory_linewidths
  ) +
  scale_x_continuous(
    limits = c(0, 1000000),
    breaks = seq(0, 1000000, by = 250000),
    labels = c("0", "25", "50", "75", "100"),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(
    ylim = c(0, 51)
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 2.2)
    ),
    linewidth = guide_legend(
      order = 2
    )
  ) +
  labs(
    x = expression("Generation (" * 10^4 * ")"),
    y = "Interest diversity",
    color = NULL,
    linewidth = NULL
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
    plot.margin = margin(12, 10, 5, 10)
  )

heatmap_legend <- cowplot::get_legend(
  p_div_mu +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(7, "cm"),
        barheight = unit(0.45, "cm"),
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    theme(
      legend.position = "bottom",
      legend.justification = "center"
    )
)

p_div_mu_ori <- p_div_mu_ori +
  theme(legend.position = "none")

p_div_mu <- p_div_mu +
  theme(legend.position = "none")

p_div_b_ori <- p_div_b_ori +
  theme(legend.position = "none")

p_div_b <- p_div_b +
  theme(legend.position = "none")

p_combined <- wrap_plots(
  A = make_tag_plot("A"),
  T = p_trj,
  B = make_tag_plot("B"),
  H = p_div_mu_ori,
  C = make_tag_plot("C"),
  I = p_div_mu,
  D = make_tag_plot("D"),
  J = p_div_b_ori,
  E = make_tag_plot("E"),
  K = p_div_b,
  L = wrap_elements(full = heatmap_legend),
  design = "
  ATTT
  ####
  BHCI
  DJEK
  LLLL
  ",
  widths = c(0.15, 0.12, 0.4, 1.62),
  heights = c(1.05, 0.15, 1, 1, 0.22)
)

ggsave(
  file.path(
    save_path,
    "Supplementary figure 4.pdf"
  ),
  p_combined,
  width = 11,
  height = 14,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)

