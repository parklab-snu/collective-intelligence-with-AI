library(ggh4x)
library(ggplot2)
library(tidyr)
library(dplyr)

#------------------------------------------------------------
save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/"

load(file.path(save_path, "Original/avg_feedback.RData"))
ori_avg_feedback <- Result

load(file.path(save_path, "Original/avg_niche.RData"))
ori_avg_niche <- Result

load(file.path(save_path, "AI_knows_all/avg_feedback_Acc70_bias0.36_error0.3.RData"))
AI_avg_feedback <- Result

load(file.path(save_path, "AI_knows_all/avg_niche_Acc70_bias0.36_error0.3.RData"))
AI_avg_niche <- Result

#------------------------------------------------------------
legend_order <- c(
  "Feedback",
  "Niche expert",
  "Feedback with AI",
  "Niche expert with AI"
)

metric_order <- c(
  "Collective accuracy",
  "Human accuracy",
  "Interest diversity",
  "Median reliance on AI",
  "Collective bias",
  "Collective variance"
)

make_metric_df <- function(metric_name, y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- ori_avg_feedback[[metric_name]][idx]
  cf_02 <- ori_avg_niche[[metric_name]][idx]
  cf_03 <- AI_avg_feedback[[metric_name]][idx]
  cf_04 <- AI_avg_niche[[metric_name]][idx]
  
  data.frame(
    Generation = rep(idx, 4),
    Value = c(cf_01, cf_02, cf_03, cf_04),
    source = rep(c("Feedback", "Niche expert", "Feedback with AI", "Niche expert with AI"), each = length(idx)),
    Metric = y_label
  )
}

make_metric_df_acc <- function(y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- ori_avg_feedback[["accuracy"]][idx]
  cf_02 <- ori_avg_niche[["accuracy"]][idx]
  cf_03 <- AI_avg_feedback[["accuracy"]][idx]
  cf_04 <- AI_avg_niche[["accuracy"]][idx]
  
  
  
  data.frame(
    Generation = rep(idx, 4),
    Value = c(cf_01, cf_02, cf_03, cf_04),
    source = rep(c("Feedback", "Niche expert", "Feedback with AI", "Niche expert with AI"), each = length(idx)),
    Metric = y_label
  )
}

make_metric_df_AI_reliance <- function(y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- rep(0, 2000)
  cf_02 <- rep(0, 2000)
  cf_03 <- AI_avg_feedback[["median_AI_belief"]][idx]
  cf_04 <- AI_avg_niche[["median_AI_belief"]][idx]
  
  data.frame(
    Generation = rep(idx, 4),
    Value = c(cf_01, cf_02, cf_03, cf_04),
    source = rep(c("Feedback", "Niche expert", "Feedback with AI", "Niche expert with AI"), each = length(idx)),
    Metric = y_label
  )
}

make_metric_df_human_acc <- function(y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- ori_avg_feedback[["accuracy"]][idx]
  cf_02 <- ori_avg_niche[["accuracy"]][idx]
  cf_03 <- AI_avg_feedback[["human_accuracy"]][idx]
  cf_04 <- AI_avg_niche[["human_accuracy"]][idx]
  
  data.frame(
    Generation = rep(idx, 4),
    Value = c(cf_01, cf_02, cf_03, cf_04),
    source = rep(c("Feedback", "Niche expert", "Feedback with AI", "Niche expert with AI"), each = length(idx)),
    Metric = y_label
  )
}

df_all <- bind_rows(
  make_metric_df("accuracy", "Collective accuracy"),
  make_metric_df("interest_diversity", "Interest diversity"),
  make_metric_df("bias_sq", "Collective bias"),
  make_metric_df("variance", "Collective variance"),
  make_metric_df_AI_reliance("Median reliance on AI"),
  make_metric_df_human_acc("Human accuracy")
)

df_all$source <- factor(df_all$source, levels = legend_order)
df_all$Metric <- factor(df_all$Metric, levels = metric_order)

#------------------------------------------------------------
line_width <- 1.0
box_width <- 1.0

axis_title_size <- 12
axis_text_size <- 12
legend_title_size <- 13
legend_text_size <- 11
strip_text_size <- 11

font_family <- "Arial"

my_colors <- c(
  "Feedback" = "#2166AC",
  "Niche expert" = "#2166AC",
  "Feedback with AI" = "#B2182B",
  "Niche expert with AI" = "#B2182B",
  "AI accuracy" = "gray45"
)

my_linewidth <- c(
  "Feedback" = 1.2,
  "Niche expert" = 1.2,
  "Feedback with AI" = 1.1,
  "Niche expert with AI" = 1.1,
  "AI accuracy" = 1.1
)

#------------------------------------------------------------
ai_acc_line <- data.frame(
  Metric = factor("Collective accuracy", levels = metric_order),
  yintercept = 0.75,
  source = "AI accuracy"
)

plot <- ggplot(df_all, aes(x = Generation, y = Value, color = source)) +
  geom_line(aes(linetype = source, linewidth = source), lineend = "round") +
  geom_hline(
    data = ai_acc_line,
    aes(yintercept = yintercept, color = source, linetype = source, linewidth = source),
    lineend = "round"
  ) +
  facet_wrap(
    ~ Metric,
    ncol = 2,
    scales = "free_y"
  ) +
  facetted_pos_scales(
    y = list(
      Metric == "Human accuracy" ~ scale_y_continuous( limits = c(-0.5, 1.0) ),
      Metric == "Interest diversity" ~ scale_y_continuous( limits = c(0, 51) ),
      Metric == "Collective accuracy" ~ scale_y_continuous(
        limits = c(0.0, 1),
        breaks = seq(1, 0, by = -0.2),
        labels = sprintf("%.1f", seq(1, 0, by = -0.2))
      )
    )
  )+
  scale_linewidth_manual(values = my_linewidth) +
  scale_color_manual(values = my_colors) +
  scale_linetype_manual(
    values = c(
      "Feedback" = "solid",
      "Niche expert" = "dotted",
      "Feedback with AI" = "solid",
      "Niche expert with AI" = "dotted",
      "AI accuracy" = "dashed"
    )
  ) +
  
  scale_x_continuous(
    breaks = c(0, 40000, 80000, 120000, 160000, 200000),
    labels = c(0, 4, 8, 12, 16, 20)
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = NULL,
    color = "Type",
    linetype = "Type",
    linewidth = "Type"
  ) +
  theme_classic(base_family = font_family) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "right",
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = box_width
    ),
    axis.title = element_text(size = axis_title_size, family = font_family),
    axis.text = element_text(size = axis_text_size, family = font_family),
    legend.title = element_text(size = legend_title_size, family = font_family),
    legend.text = element_text(size = legend_text_size, family = font_family),
    strip.text = element_text(size = strip_text_size, family = font_family),
    strip.background = element_blank()
  )

ggsave(
  file.path(save_path, "six_metrics.png"),
  plot,
  width = 6,
  height = 5,
  bg = "white"
)


library(ggh4x)
library(ggplot2)
library(tidyr)
library(dplyr)

#------------------------------------------------------------
save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/"

load(file.path(save_path, "Original/clu_feedback.RData"))
ori_clu_feedback <- Result

load(file.path(save_path, "Original/clu_niche.RData"))
ori_clu_niche <- Result

load(file.path(save_path, "AI_answers_question/clu_feedback_Acc70_bias0.36_error0.3.RData"))
AI_clu_feedback <- Result

load(file.path(save_path, "AI_answers_question/clu_niche_Acc70_bias0.36_error0.3.RData"))
AI_clu_niche <- Result

#------------------------------------------------------------
legend_order <- c(
  "Feedback",
  "Niche expert",
  "Feedback with AI",
  "Niche expert with AI"
)

metric_order <- c(
  "Collective accuracy",
  "Human accuracy",
  "Interest diversity",
  "Median reliance on AI",
  "Collective bias",
  "Collective variance"
)

make_metric_df <- function(metric_name, y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- ori_clu_feedback[[metric_name]][idx]
  cf_02 <- ori_clu_niche[[metric_name]][idx]
  cf_03 <- AI_clu_feedback[[metric_name]][idx]
  cf_04 <- AI_clu_niche[[metric_name]][idx]
  
  data.frame(
    Generation = rep(idx, 4),
    Value = c(cf_01, cf_02, cf_03, cf_04),
    source = rep(c("Feedback", "Niche expert", "Feedback with AI", "Niche expert with AI"), each = length(idx)),
    Metric = y_label
  )
}

make_metric_df_AI_reliance <- function(y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- rep(0, 2000)
  cf_02 <- rep(0, 2000)
  cf_03 <- AI_clu_feedback[["median_AI_belief"]][idx]
  cf_04 <- AI_clu_niche[["median_AI_belief"]][idx]
  
  data.frame(
    Generation = rep(idx, 4),
    Value = c(cf_01, cf_02, cf_03, cf_04),
    source = rep(c("Feedback", "Niche expert", "Feedback with AI", "Niche expert with AI"), each = length(idx)),
    Metric = y_label
  )
}

make_metric_df_human_acc <- function(y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- ori_clu_feedback[["accuracy"]][idx]
  cf_02 <- ori_clu_niche[["accuracy"]][idx]
  cf_03 <- AI_clu_feedback[["human_accuracy"]][idx]
  cf_04 <- AI_clu_niche[["human_accuracy"]][idx]
  
  data.frame(
    Generation = rep(idx, 4),
    Value = c(cf_01, cf_02, cf_03, cf_04),
    source = rep(c("Feedback", "Niche expert", "Feedback with AI", "Niche expert with AI"), each = length(idx)),
    Metric = y_label
  )
}

df_all <- bind_rows(
  make_metric_df("accuracy", "Collective accuracy"),
  make_metric_df("interest_diversity", "Interest diversity"),
  make_metric_df("bias_sq", "Collective bias"),
  make_metric_df("variance", "Collective variance"),
  make_metric_df_AI_reliance("Median reliance on AI"),
  make_metric_df_human_acc("Human accuracy")
)

df_all$source <- factor(df_all$source, levels = legend_order)
df_all$Metric <- factor(df_all$Metric, levels = metric_order)

#------------------------------------------------------------
line_width <- 1.0
box_width <- 1.0

axis_title_size <- 12
axis_text_size <- 12
legend_title_size <- 13
legend_text_size <- 11
strip_text_size <- 11

font_family <- "Arial"

my_colors <- c(
  "Feedback" = "#2166AC",
  "Niche expert" = "#2166AC",
  "Feedback with AI" = "#B2182B",
  "Niche expert with AI" = "#B2182B",
  "AI accuracy" = "gray45"
)

my_linewidth <- c(
  "Feedback" = 1.2,
  "Niche expert" = 1.2,
  "Feedback with AI" = 1.1,
  "Niche expert with AI" = 1.1,
  "AI accuracy" = 1.1
)

#------------------------------------------------------------
ai_acc_line <- data.frame(
  Metric = factor("Collective accuracy", levels = metric_order),
  yintercept = 0.75,
  source = "AI accuracy"
)

plot <- ggplot(df_all, aes(x = Generation, y = Value, color = source)) +
  geom_line(aes(linetype = source, linewidth = source), lineend = "round") +
  geom_hline(
    data = ai_acc_line,
    aes(yintercept = yintercept, color = source, linetype = source, linewidth = source),
    lineend = "round"
  ) +
  facet_wrap(
    ~ Metric,
    ncol = 2,
    scales = "free_y"
  ) +
  facetted_pos_scales(
    y = list(
      Metric == "Human accuracy" ~ scale_y_continuous( limits = c(0.0, 1.0) ),
      Metric == "Median reliance on AI" ~ scale_y_continuous( limits = c(0.0, 1.0) ),
      Metric == "Interest diversity" ~ scale_y_continuous( limits = c(0, 51) ),
      Metric == "Collective accuracy" ~ scale_y_continuous(
        limits = c(0.5, 1),
        breaks = seq(1, 0, by = -0.2),
        labels = sprintf("%.1f", seq(1, 0, by = -0.2))
      )
    )
  )+
  scale_linewidth_manual(values = my_linewidth) +
  scale_color_manual(values = my_colors) +
  scale_linetype_manual(
    values = c(
      "Feedback" = "solid",
      "Niche expert" = "dotted",
      "Feedback with AI" = "solid",
      "Niche expert with AI" = "dotted",
      "AI accuracy" = "dashed"
    )
  ) +
  
  scale_x_continuous(
    breaks = c(0, 40000, 80000, 120000, 160000, 200000),
    labels = c(0, 4, 8, 12, 16, 20)
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = NULL,
    color = "Type",
    linetype = "Type",
    linewidth = "Type"
  ) +
  theme_classic(base_family = font_family) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "right",
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = box_width
    ),
    axis.title = element_text(size = axis_title_size, family = font_family),
    axis.text = element_text(size = axis_text_size, family = font_family),
    legend.title = element_text(size = legend_title_size, family = font_family),
    legend.text = element_text(size = legend_text_size, family = font_family),
    strip.text = element_text(size = strip_text_size, family = font_family),
    strip.background = element_blank()
  )

ggsave(
  file.path(save_path, "six_metrics_clu.png"),
  plot,
  width = 6,
  height = 5,
  bg = "white"
)
