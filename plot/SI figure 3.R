#---------------------------------
library(ggplot2)
library(dplyr)
library(patchwork)

feedback_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/Feedback_biassweep"
niche_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/Nicheexpert_biassweep"
balanced_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/Balanced_biassweep"
save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/Balanced_biassweep"

bias_list <- c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)

df_feedback <- do.call(rbind, lapply(bias_list, function(i) {
  fp <- file.path(feedback_path, sprintf("adv_feedback_k%02d_i%03d_j%02f.RData", 1, 0, i))
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

stationary_feedback <- df_feedback %>%
  filter(Generation >= 190000, Generation <= 200000) %>%
  group_by(bias_i) %>%
  summarise(
    source = "Feedback",
    accuracy = mean(accuracy),
    human_accuracy = mean(human_accuracy),
    median_AI_belief = mean(median_AI_belief),
    bias_sq = mean(bias_sq),
    variance = mean(variance),
    interest_diversity = mean(interest_diversity),
    .groups = "drop"
  ) %>%
  arrange(bias_i)

df_niche <- do.call(rbind, lapply(bias_list, function(i) {
  fp <- file.path(niche_path, sprintf("adv_niche_k%02d_i%03d_j%02f.RData", 1, 0, i))
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

stationary_niche <- df_niche %>%
  filter(Generation >= 190000, Generation <= 200000) %>%
  group_by(bias_i) %>%
  summarise(
    source = "Niche expert",
    accuracy = mean(accuracy),
    human_accuracy = mean(human_accuracy),
    median_AI_belief = mean(median_AI_belief),
    bias_sq = mean(bias_sq),
    variance = mean(variance),
    interest_diversity = mean(interest_diversity),
    .groups = "drop"
  ) %>%
  arrange(bias_i)

df_balanced <- do.call(rbind, lapply(bias_list, function(i) {
  fp <- file.path(balanced_path, sprintf("Balanced_k%02d_i%02f_j%02f.RData", 1, 0, i))
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

stationary_balanced <- df_balanced %>%
  filter(Generation >= 190000, Generation <= 200000) %>%
  group_by(bias_i) %>%
  summarise(
    source = "Balanced",
    accuracy = mean(accuracy),
    human_accuracy = mean(human_accuracy),
    median_AI_belief = mean(median_AI_belief),
    bias_sq = mean(bias_sq),
    variance = mean(variance),
    interest_diversity = mean(interest_diversity),
    .groups = "drop"
  ) %>%
  arrange(bias_i)

df <- bind_rows(
  stationary_feedback,
  stationary_niche,
  stationary_balanced
)

# ---- Theme / scale ----
line_width  <- 1.2
box_width   <- 1.0

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

my_colors <- c(
  "Feedback" = "#3A85A6",
  "Niche expert" = "#FC8644",
  "Balanced" = "#C173C3"
)

my_shapes <- c(
  "Feedback" = 17,
  "Niche expert" = 15,
  "Balanced" = 16
)

# ---- Accuracy plot ----
p_accuracy <- ggplot(
  df,
  aes(
    x = bias_i,
    y = accuracy,
    color = source,
    shape = source
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(size = 5)+
  x_scale +
  scale_color_manual(values = my_colors, name = "Incentive") +
  scale_shape_manual(values = my_shapes, name = "Incentive")+
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

p_accuracy

p_hacc <- ggplot(
  df,
  aes(
    x = bias_i,
    y = human_accuracy,
    color = source,
    shape = source
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(size = 5)+
  x_scale +
  scale_color_manual(values = my_colors, name = "Incentive") +
  scale_shape_manual(values = my_shapes, name = "Incentive") +
  labs(
    x = "Bias",
    y = NULL,
    title = "Counterfactual human CI"
  ) +
  single_theme +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


p_belief <- ggplot(
  df,
  aes(
    x = bias_i,
    y = median_AI_belief,
    color = source,
    shape = source
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(size = 5) +
  scale_color_manual(values = my_colors, name = "Incentive") +
  scale_shape_manual(values = my_shapes, name = "Incentive") +
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
  df,
  aes(
    x = bias_i,
    y = interest_diversity,
    color = source,
    shape = source
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(size = 5) +
  scale_color_manual(values = my_colors, name = "Incentive") +
  scale_shape_manual(values = my_shapes, name = "Incentive") +
  x_scale +
  coord_cartesian(ylim = c(0, 51)) +
  labs(
    x = "Bias",
    y = NULL,
    title = "Interest Diversity"
  ) +
  single_theme

p_var <- ggplot(
  df,
  aes(
    x = bias_i,
    y = variance,
    color = source,
    shape = source
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(size = 5) +
  scale_color_manual(values = my_colors, name = "Incentive") +
  scale_shape_manual(values = my_shapes, name = "Incentive") +
  x_scale +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(
    x = "Bias",
    y = NULL,
    title = "Collective variance"
  ) +
  single_theme

p_bias <- ggplot(
  df,
  aes(
    x = bias_i,
    y = bias_sq,
    color = source,
    shape = source
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(size = 5) +
  scale_color_manual(values = my_colors, name = "Incentive") +
  scale_shape_manual(values = my_shapes, name = "Incentive") +
  x_scale +
  coord_cartesian(ylim = c(0, 800)) +
  labs(
    x = "Bias",
    y = NULL,
    title = "Collective bias"
  ) +
  single_theme

top_row_margin <- theme(
  plot.margin = margin(t = 5.5, r = 5.5, b = 30, l = 5.5)
)

p_accuracy <- p_accuracy + labs(x = NULL) + top_row_margin
p_hacc <- p_hacc + labs(x = NULL) + top_row_margin
p_belief <- p_belief + labs(x = NULL) + top_row_margin

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
  filename = file.path(
    save_path,
    "SI3.pdf"
  ),
  plot = p_combined,
  device = grDevices::cairo_pdf,
  width = 13,
  height = 8,
  units = "in",
  bg = "white"
)
