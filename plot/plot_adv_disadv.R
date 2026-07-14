#----------------------------------
library(ggplot2)
library(dplyr)
library(patchwork)

save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/Adv_niche_50"

lambda_list <- c(-40, -30, -20, -10, 0, 10, 20, 30, 40)
bias_list <- c(-0.4, -0.2, 0.2, 0.4)

grid <- expand.grid(i = lambda_list, j = bias_list)

df <- do.call(rbind, lapply(seq_len(nrow(grid)), function(k) {
  i <- grid$i[k]; j <- grid$j[k]
  fp <- file.path(save_path, sprintf("adv_niche_k%02d_i%03d_j%02f.RData", 1, i, j))
  if (!file.exists(fp)) { warning(paste("Missing:", fp)); return(NULL) }
  env <- new.env(); load(fp, envir = env)
  R <- env$Result
  
  data.frame(
    lambda = i, bias_i = j,
    AI_accuracy      = env$AI_accuracy,
    Generation       = seq_along(R$accuracy),
    accuracy         = R$accuracy,
    human_accuracy   = R$human_accuracy,
    median_AI_belief = R$median_AI_belief
  )
}))

stationary <- df %>%
  filter(Generation >= 190000, Generation <= 200000) %>%
  group_by(lambda, bias_i) %>%
  summarise(
    accuracy         = mean(accuracy),
    human_accuracy   = mean(human_accuracy),
    median_AI_belief = mean(median_AI_belief),
    .groups = "drop"
  ) %>%
  arrange(lambda, bias_i)

# ---- Theme / scale ----
line_width  <- 1.2
box_width   <- 1.0
font_family <- "Arial"

library(ggplot2)
library(dplyr)
library(patchwork)

# ---- Factor for line groups ----
stationary_plot <- stationary %>%
  mutate(
    bias_i = factor(
      bias_i,
      levels = c(-0.4, -0.2, 0.2, 0.4),
      labels = c("-0.4", "-0.2", "0.2", "0.4")
    )
  )

# ---- Colors for bias_i ----
bias_colors <- c(
  "-0.4" = "#00658d",
  "-0.2" = "#008f7b",
  "0.2"  = "#5fab29",
  "0.4"  = "#ffa600"
)

single_theme <- theme_classic(base_family = font_family) +
  theme(
    panel.grid          = element_blank(),
    panel.background    = element_blank(),
    plot.background     = element_blank(),
    legend.background   = element_blank(),
    legend.key          = element_blank(),
    panel.border        = element_rect(color = "black", fill = NA, linewidth = box_width),
    axis.title          = element_text(size = 20, family = font_family),
    axis.text           = element_text(size = 16, family = font_family),
    axis.title.x.top    = element_text(margin = margin(b = 8)),
    axis.title.x.bottom = element_text(margin = margin(t = 8)),
    legend.title        = element_text(size = 18, family = font_family),
    legend.text         = element_text(size = 15, family = font_family),
    legend.key.height   = unit(0.7, "cm"),
    plot.title          = element_text(size = 18, family = font_family, hjust = 0.5)
  )

# ---- Optional x scale ----
x_scale <- scale_x_continuous(
  breaks = c(-40, -20, 0, 20, 40)
)

# ---- Accuracy plot ----
p_accuracy <- ggplot(
  stationary_plot,
  aes(
    x = lambda,
    y = accuracy,
    color = bias_i,
    group = bias_i
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(
    data = stationary_plot %>% filter(bias_i == "-0.4"),
    size = 5,
    shape = 17
  ) +
  geom_point(
    data = stationary_plot %>% filter(bias_i == "-0.2"),
    size = 5,
    shape = 15
  ) +
  geom_point(
    data = stationary_plot %>% filter(bias_i == "0.2"),
    size = 5,
    shape = 16
  ) +
  geom_point(
    data = stationary_plot %>% filter(bias_i == "0.4"),
    size = 5,
    shape = 18
  )+
  scale_color_manual(values = bias_colors, name = "Bias") +
  x_scale +
  coord_cartesian(ylim = c(0.35, 1)) +
  labs(
    x = expression(lambda),
    y = "Accuracy"
  ) +
  single_theme

# ---- Median AI belief plot ----
p_belief <- ggplot(
  stationary_plot,
  aes(
    x = lambda,
    y = median_AI_belief,
    color = bias_i,
    group = bias_i
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(
    data = stationary_plot %>% filter(bias_i == "-0.4"),
    size = 5,
    shape = 17
  ) +
  geom_point(
    data = stationary_plot %>% filter(bias_i == "-0.2"),
    size = 5,
    shape = 15
  ) +
  geom_point(
    data = stationary_plot %>% filter(bias_i == "0.2"),
    size = 5,
    shape = 16
  ) +
  geom_point(
    data = stationary_plot %>% filter(bias_i == "0.4"),
    size = 5,
    shape = 18
  )+
  scale_color_manual(values = bias_colors, name = "Bias") +
  x_scale +
  coord_cartesian(ylim = c(0.0, 1)) +
  labs(
    x = expression(lambda),
    y = "Median AI belief"
  ) +
  single_theme

p_combined <- (p_accuracy | p_belief) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right"
  )

ggsave(
  file.path(save_path, "Adv_niche_Stationary_vs_Lambda_accuracy_belief_by_bias_30.png"),
  p_combined,
  width = 10,
  height = 4,
  bg = "white"
)
