#----------------------------------
library(ggplot2)
library(dplyr)
library(patchwork)

save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/Figure4"

lambda_list <- c(0)
bias_list <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)

grid <- expand.grid(i = lambda_list, j = bias_list)

df <- do.call(rbind, lapply(seq_len(nrow(grid)), function(k) {
  i <- grid$i[k]; j <- grid$j[k]
  fp <- file.path(save_path, sprintf("Feedback/balanced_k%02d_i%03d_j%02f.RData", 1, i, j))
  if (!file.exists(fp)) { warning(paste("Missing:", fp)); return(NULL) }
  env <- new.env(); load(fp, envir = env)
  R <- env$Result
  
  data.frame(
    lambda = i, bias_i = j,
    Generation = seq_along(R$accuracy),
    accuracy = R$accuracy,
    human_accuracy = R$human_accuracy[1:10000],
    interest_diversity = R$interest_diversity,
    belief_diversity = R$belief_diversity,
    reliance_diversity = R$reliance_diversity,
    median_AI_belief = R$median_AI_belief
  )
}))

stationary_feedback <- df %>%
  group_by(bias_i) %>%
  summarise(
    accuracy = mean(accuracy),
    human_accuracy = mean(human_accuracy),
    interest_diversity = mean(interest_diversity),
    belief_diversity = mean(belief_diversity),
    reliance_diversity = mean(reliance_diversity),
    median_AI_belief = mean(median_AI_belief),
    .groups = "drop"
  ) %>%
  arrange(bias_i)

df <- do.call(rbind, lapply(seq_len(nrow(grid)), function(k) {
  i <- grid$i[k]; j <- grid$j[k]
  fp <- file.path(save_path, sprintf("Niche_expert/balanced_k%02d_i%03d_j%02f.RData", 1, i, j))
  if (!file.exists(fp)) { warning(paste("Missing:", fp)); return(NULL) }
  env <- new.env(); load(fp, envir = env)
  R <- env$Result
  
  data.frame(
    lambda = i, bias_i = j,
    Generation = seq_along(R$accuracy),
    accuracy = R$accuracy,
    human_accuracy = R$human_accuracy[1:10000],
    interest_diversity = R$interest_diversity,
    belief_diversity = R$belief_diversity,
    reliance_diversity = R$reliance_diversity,
    median_AI_belief = R$median_AI_belief
  )
}))

stationary_niche <- df %>%
  group_by(bias_i) %>%
  summarise(
    accuracy = mean(accuracy),
    human_accuracy = mean(human_accuracy),
    interest_diversity = mean(interest_diversity),
    belief_diversity = mean(belief_diversity),
    reliance_diversity = mean(reliance_diversity),
    median_AI_belief = mean(median_AI_belief),
    .groups = "drop"
  ) %>%
  arrange(bias_i)

df <- do.call(rbind, lapply(seq_len(nrow(grid)), function(k) {
  i <- grid$i[k]; j <- grid$j[k]
  fp <- file.path(save_path, sprintf("Balanced/balanced_k%02d_i%03d_j%02f.RData", 1, i, j))
  if (!file.exists(fp)) { warning(paste("Missing:", fp)); return(NULL) }
  env <- new.env(); load(fp, envir = env)
  R <- env$Result
  
  data.frame(
    lambda = i, bias_i = j,
    Generation = seq_along(R$accuracy),
    accuracy = R$accuracy,
    human_accuracy = R$human_accuracy[1:10000],
    interest_diversity = R$interest_diversity,
    belief_diversity = R$belief_diversity,
    reliance_diversity = R$reliance_diversity,
    median_AI_belief = R$median_AI_belief
  )
}))

stationary_balanced <- df %>%
  group_by(bias_i) %>%
  summarise(
    accuracy = mean(accuracy),
    human_accuracy = mean(human_accuracy),
    interest_diversity = mean(interest_diversity),
    belief_diversity = mean(belief_diversity),
    reliance_diversity = mean(reliance_diversity),
    median_AI_belief = mean(median_AI_belief),
    .groups = "drop"
  ) %>%
  arrange(bias_i)


# ---- Theme / scale ----
line_width  <- 1.2
box_width   <- 1.0

stationary <- bind_rows(
  stationary_feedback %>% mutate(payoff = "Feedback"),
  stationary_niche %>% mutate(payoff = "Niche expert"),
  stationary_balanced %>% mutate(payoff = "Balanced")
)

stationary_plot <- stationary %>%
  mutate(
    payoff = factor(
      payoff,
      levels = c("Feedback", "Niche expert", "Balanced")
    )
  )

# ---- Colors for bias_i ----
bias_colors <- c(
  "Feedback" = "#00658d",
  "Niche expert" = "#008f7b",
  "Balanced"= "#ffa600"
)

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
  breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)
)

p_accuracy <- ggplot(
  stationary_plot,
  aes(
    x = bias_i,
    y = accuracy,
    color = payoff,
    group = payoff
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Feedback"),
    size = 5,
    shape = 17
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Niche expert"),
    size = 5,
    shape = 15
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Balanced"),
    size = 5,
    shape = 16
  ) +
  scale_color_manual(values = bias_colors, name = "Incentive") +
  x_scale +
  labs(
    x = "AI bias",
    y = "Collective accuracy"
  ) +
  single_theme

p_accuracy

p_intdiv <- ggplot(
  stationary_plot,
  aes(
    x = bias_i,
    y = interest_diversity,
    color = payoff,
    group = payoff
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Feedback"),
    size = 5,
    shape = 17
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Niche expert"),
    size = 5,
    shape = 15
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Balanced"),
    size = 5,
    shape = 16
  ) +
  scale_color_manual(values = bias_colors, name = "Incentive") +
  x_scale +
  labs(
    x = "AI bias",
    y = "Interest diversity"
  ) +
  single_theme

p_intdiv

p_beldiv <- ggplot(
  stationary_plot,
  aes(
    x = bias_i,
    y = belief_diversity,
    color = payoff,
    group = payoff
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Feedback"),
    size = 5,
    shape = 17
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Niche expert"),
    size = 5,
    shape = 15
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Balanced"),
    size = 5,
    shape = 16
  ) +
  scale_color_manual(values = bias_colors, name = "Incentive") +
  x_scale +
  labs(
    x = "AI bias",
    y = "Belief diversity"
  ) +
  single_theme

p_beldiv

p_reldiv <- ggplot(
  stationary_plot,
  aes(
    x = bias_i,
    y = reliance_diversity,
    color = payoff,
    group = payoff
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Feedback"),
    size = 5,
    shape = 17
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Niche expert"),
    size = 5,
    shape = 15
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Balanced"),
    size = 5,
    shape = 16
  ) +
  scale_color_manual(values = bias_colors, name = "Incentive") +
  x_scale +
  labs(
    x = "AI bias",
    y = "Reliance diversity"
  ) +
  single_theme

p_reldiv

p_hacc <- ggplot(
  stationary_plot,
  aes(
    x = bias_i,
    y = human_accuracy,
    color = payoff,
    group = payoff
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Feedback"),
    size = 5,
    shape = 17
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Niche expert"),
    size = 5,
    shape = 15
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Balanced"),
    size = 5,
    shape = 16
  ) +
  scale_color_manual(values = bias_colors, name = "Incentive") +
  x_scale +
  labs(
    x = "AI bias",
    y = "Counterfactual human CI"
  ) +
  single_theme

p_hacc

p_AIrel <- ggplot(
  stationary_plot,
  aes(
    x = bias_i,
    y = median_AI_belief,
    color = payoff,
    group = payoff
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Feedback"),
    size = 5,
    shape = 17
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Niche expert"),
    size = 5,
    shape = 15
  ) +
  geom_point(
    data = stationary_plot %>% filter(payoff == "Balanced"),
    size = 5,
    shape = 16
  ) +
  scale_color_manual(values = bias_colors, name = "Incentive") +
  x_scale +
  labs(
    x = "AI bias",
    y = "Median reliance on AI"
  ) +
  single_theme

p_AIrel
