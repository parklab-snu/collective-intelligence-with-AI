#---------------------------------
library(ggplot2)
library(dplyr)
library(patchwork)

save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/Feedback_biassweep"

bias_list <- c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)

df <- do.call(rbind, lapply(bias_list, function(i) {
  fp <- file.path(save_path, sprintf("adv_feedback_k%02d_i%03d_j%02f.RData", 1, 0, i))
  if (!file.exists(fp)) { warning(paste("Missing:", fp)); return(NULL) }
  env <- new.env(); load(fp, envir = env)
  R <- env$Result
  
  data.frame(
    bias_i = i,
    AI_accuracy = env$AI_accuracy,
    Generation = seq_along(R$accuracy),
    accuracy = R$accuracy,
    human_accuracy = R$human_accuracy,
    median_AI_belief = R$median_AI_belief,
    bias_sq = R$bias_sq,
    variance = R$variance,
    interest_diversity = R$interest_diversity
  )
}))

stationary <- df %>%
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

# ---- Theme / scale ----
line_width  <- 1.2
box_width   <- 1.0

# ---- Factor for line groups ----
# stationary_plot <- stationary %>%
#   mutate(
#     bias_i = factor(
#       bias_i,
#       levels = c(-0.4, -0.2, 0.2, 0.4),
#       labels = c("-0.4", "-0.2", "0.2", "0.4")
#     )
#   )

# ---- Colors for bias_i ----
# bias_colors <- c(
#   "-0.4" = "#00658d",
#   "-0.2" = "#008f7b",
#   "0.2"  = "#5fab29",
#   "0.4"  = "#ffa600"
# )

single_theme <- theme_classic() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = box_width),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.title.x.bottom = element_text(margin = margin(t = 8)),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.key.height = unit(0.7, "cm"),
    plot.title = element_text(size = 18, hjust = 0.5)
  )

# ---- Optional x scale ----
x_scale <- scale_x_continuous(
  breaks = c(-0.6, -0.3, 0, 0.3, 0.6)
)

color1 <- "#00658d"

# ---- Accuracy plot ----
p_accuracy <- ggplot(
  stationary,
  aes(
    x = bias_i,
    y = accuracy
  )
) +
  geom_line(linewidth = line_width, color = color1) +
  geom_point(size = 5, color = color1)+
  x_scale +
  geom_hline(
    yintercept = 1.0,
    linetype = "dashed",
    linewidth = 1.5,
    color = "#7E7E7E"
  ) +
  coord_cartesian(ylim = c(0.0, 1)) +
  labs(
    x = "Bias",
    y = NULL,
    title = "Collective accuracy"
  ) +
  single_theme +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p_hacc <- ggplot(
  stationary,
  aes(
    x = bias_i,
    y = human_accuracy
  )
) +
  geom_line(linewidth = line_width, color = color1) +
  geom_point(size = 5, color = color1)+
  x_scale +
  geom_hline(
    yintercept = 1.0,
    linetype = "dashed",
    linewidth = 1.5,
    color = "#7E7E7E"
  ) +
  labs(
    x = "Bias",
    y = NULL,
    title = "Couterfactual human CI"
  ) +
  single_theme +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


p_belief <- ggplot(
  stationary,
  aes(
    x = bias_i,
    y = median_AI_belief
  )
) +
  geom_line(linewidth = line_width, color = color1) +
  geom_point(size = 5, color = color1)+
  x_scale +
  coord_cartesian(ylim = c(0.0, 1)) +
  labs(
    x = "Bias",
    y = NULL,
    title = "Median reliance on AI"
  ) +
  single_theme +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p_div <- ggplot(
  stationary,
  aes(
    x = bias_i,
    y = interest_diversity
  )
) +
  geom_line(linewidth = line_width, color = color1) +
  geom_point(size = 5, color = color1)+
  x_scale +
  coord_cartesian(ylim = c(0, 51)) +
  labs(
    x = "Bias",
    y = NULL,
    title = "Interest Diversity"
  ) +
  single_theme

p_var <- ggplot(
  stationary,
  aes(
    x = bias_i,
    y = variance
  )
) +
  geom_line(linewidth = line_width, color = color1) +
  geom_point(size = 5, color = color1)+
  x_scale +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(
    x = "Bias",
    y = NULL,
    title = "Collective variance"
  ) +
  single_theme

p_bias <- ggplot(
  stationary,
  aes(
    x = bias_i,
    y = bias_sq
  )
) +
  geom_line(linewidth = line_width, color = color1) +
  geom_point(size = 5, color = color1)+
  x_scale +
  coord_cartesian(ylim = c(0, 800)) +
  labs(
    x = "Bias",
    y = NULL,
    title = "Collective bias"
  ) +
  single_theme

p_combined <- p_accuracy + p_hacc + p_belief + p_var + p_bias + p_div +
  plot_layout(
    ncol = 3,
    guides = "collect",
    axis_titles = "collect_x"
  ) &
  theme(
    legend.position = "right"
  )

p_combined

ggsave(
  file.path(save_path, "SI1_3.png"),
  p_combined,
  width = 13,
  height = 8,
  bg = "white"
)
