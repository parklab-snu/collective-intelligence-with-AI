if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("patchwork")) install.packages("patchwork")

library(ggplot2)
library(dplyr)
library(patchwork)

#Set your project path
project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"
save_path <- file.path(project_path, "Figures/")

sweep_path <- file.path(
  project_path,
  "Simulations/FigureS2,S5_simulation"
)

bias_list <- c(-0.2, -0.4, -0.6)
generation <- seq(1, 200000, by = 100)

load_result <- function(file) {
  env <- new.env()
  load(file, envir = env)
  env$Result
}

original <- load_result(
  file.path(
    project_path,
    "Simulations/Figure2,S1,S3,S5_simulation/without_niche_clu.RData"
  )
)

ai_data <- bind_rows(lapply(bias_list, function(bias) {
  result <- load_result(
    file.path(
      sweep_path,
      sprintf(
        "chatbot_niche_i%02f.RData",
        bias
      )
    )
  )
  
  data.frame(
    Generation = generation,
    accuracy = result$accuracy[generation],
    median_AI_belief = result$median_AI_belief[generation],
    Trajectory = sprintf(
      "With AI (Bias = %.1f)",
      bias
    )
  )
}))

original_data <- data.frame(
  Generation = generation,
  accuracy = original$accuracy[generation],
  median_AI_belief = 0,
  Trajectory = "Without AI"
)

df <- bind_rows(
  original_data,
  ai_data
)

trajectory_order <- c(
  "Without AI",
  "With AI (Bias = -0.2)",
  "With AI (Bias = -0.4)",
  "With AI (Bias = -0.6)"
)

df$Trajectory <- factor(
  df$Trajectory,
  levels = trajectory_order
)

trajectory_colors <- c(
  "Without AI" = "#0072B2",
  "With AI (Bias = -0.2)" = "#F0A06A",
  "With AI (Bias = -0.4)" = "#D55E00",
  "With AI (Bias = -0.6)" = "#B84E00"

)

common_theme <- theme_classic(
  base_family = "Arial"
) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.9
    ),
    axis.title = element_text(size = 17),
    axis.text = element_text(
      size = 14,
      color = "black"
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.7
    ),
    legend.text = element_text(size = 13),
    legend.key.width = grid::unit(1.5, "cm"),
    plot.title = element_text(
      size = 17,
      face = "bold",
      hjust = 0.5
    )
  )

make_trajectory <- function(variable, title) {
  ggplot(
    df,
    aes(
      x = Generation,
      y = .data[[variable]],
      color = Trajectory
    )
  ) +
    geom_line(
      linewidth = 1.5,
      lineend = "round"
    ) +
    scale_color_manual(
      values = trajectory_colors,
      breaks = trajectory_order
    ) +
    scale_x_continuous(
      limits = c(0, 200000),
      breaks = c(0, 50000, 100000, 150000, 200000),
      labels = c(0, 5, 10, 15, 20),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      breaks = c(0, 0.5, 1),
      labels = c("0.0", "0.5", "1.0"),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(
      ylim = c(-0.05, 1.05)
    ) +
    labs(
      x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
      y = NULL,
      title = title,
      color = NULL
    ) +
    common_theme
}

p_accuracy <- make_trajectory(
  "accuracy",
  "Collective accuracy"
)

p_reliance <- make_trajectory(
  "median_AI_belief",
  "Median reliance on AI"
)

combined <- (
  p_accuracy |
    p_reliance
) +
  plot_layout(
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    save_path,
    "Supplementary figure 5.pdf"
  ),
  combined,
  width = 11,
  height = 5.5,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)