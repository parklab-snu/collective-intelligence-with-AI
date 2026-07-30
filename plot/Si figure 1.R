library(ggh4x)
library(ggplot2)
library(tidyr)
library(dplyr)

#------------------------------------------------------------
save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/"

load(file.path(save_path, "Original/avg_feedback.RData"))
ori_avg_feedback <- Result

# load(file.path(save_path, "Original/avg_niche.RData"))
# ori_avg_niche <- Result

load(file.path(save_path, "AI_knows_all/avg_feedback_Acc70_bias0.36_error0.3.RData"))
AI_avg_feedback <- Result

# load(file.path(save_path, "AI_knows_all/avg_niche_Acc70_bias0.36_error0.3.RData"))
# AI_avg_niche <- Result

#------------------------------------------------------------
legend_order <- c(
  "Human-only CI",
  "AI-assisted CI"
)

metric_order <- c(
  "Interest diversity",
  "Median reliance on AI",
  "Collective bias",
  "Collective variance"
)

make_metric_df <- function(metric_name, y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- ori_avg_feedback[[metric_name]][idx]
  cf_02 <- AI_avg_feedback[[metric_name]][idx]
  
  data.frame(
    Generation = rep(idx, 2),
    Value = c(cf_01, cf_02),
    source = rep(c("Human-only CI", "AI-assisted CI"), each = length(idx)),
    Metric = y_label
  )
}

make_metric_df_AI_reliance <- function(y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- rep(0, 2000)
  cf_02 <- AI_avg_feedback[["median_AI_belief"]][idx]
  
  data.frame(
    Generation = rep(idx, 2),
    Value = c(cf_01, cf_02),
    source = rep(c("Human-only CI", "AI-assisted CI"), each = length(idx)),
    Metric = y_label
  )
}

df_all <- bind_rows(
  make_metric_df("interest_diversity", "Interest diversity"),
  make_metric_df("bias_sq", "Collective bias"),
  make_metric_df("variance", "Collective variance"),
  make_metric_df_AI_reliance("Median reliance on AI")
)

df_all$source <- factor(df_all$source, levels = legend_order)
df_all$Metric <- factor(df_all$Metric, levels = metric_order)

#------------------------------------------------------------
box_width <- 1.0

axis_title_size <- 12
axis_text_size <- 12
legend_title_size <- 13
legend_text_size <- 11
strip_text_size <- 11

font_family <- "Arial"

my_colors <- c(
  "Human-only CI" = "#298C8C",
  "AI-assisted CI" = "#A00000"
)

plot <- ggplot(df_all, aes(x = Generation, y = Value, color = source)) +
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
      Metric == "Interest diversity" ~ scale_y_continuous( limits = c(0, 51) )
    )
  )+
  scale_color_manual(values = my_colors) +
  scale_x_continuous(
    breaks = c(0, 40000, 80000, 120000, 160000, 200000),
    labels = c(0, 4, 8, 12, 16, 20)
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = NULL,
    color = "Type"
  ) +
  theme_classic(base_family = font_family) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "none",
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
  file.path(save_path, "Avg_feedback.png"),
  plot,
  width = 6,
  height = 5,
  bg = "white"
)


#------------------------------------------------
library(ggh4x)
library(ggplot2)
library(tidyr)
library(dplyr)

#------------------------------------------------------------
save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/"

load(file.path(save_path, "Original/avg_niche.RData"))
ori_avg_feedback <- Result

load(file.path(save_path, "AI_knows_all/avg_niche_Acc70_bias0.36_error0.3.RData"))
AI_avg_feedback <- Result

#------------------------------------------------------------
legend_order <- c(
  "Human-only CI",
  "AI-assisted CI"
)

metric_order <- c(
  "Interest diversity",
  "Median reliance on AI",
  "Collective bias",
  "Collective variance"
)

make_metric_df <- function(metric_name, y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- ori_avg_feedback[[metric_name]][idx]
  cf_02 <- AI_avg_feedback[[metric_name]][idx]
  
  data.frame(
    Generation = rep(idx, 2),
    Value = c(cf_01, cf_02),
    source = rep(c("Human-only CI", "AI-assisted CI"), each = length(idx)),
    Metric = y_label
  )
}

make_metric_df_AI_reliance <- function(y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- rep(0, 2000)
  cf_02 <- AI_avg_feedback[["median_AI_belief"]][idx]
  
  data.frame(
    Generation = rep(idx, 2),
    Value = c(cf_01, cf_02),
    source = rep(c("Human-only CI", "AI-assisted CI"), each = length(idx)),
    Metric = y_label
  )
}

df_all <- bind_rows(
  make_metric_df("interest_diversity", "Interest diversity"),
  make_metric_df("bias_sq", "Collective bias"),
  make_metric_df("variance", "Collective variance"),
  make_metric_df_AI_reliance("Median reliance on AI")
)

df_all$source <- factor(df_all$source, levels = legend_order)
df_all$Metric <- factor(df_all$Metric, levels = metric_order)

#------------------------------------------------------------
box_width <- 1.0

axis_title_size <- 12
axis_text_size <- 12
legend_title_size <- 13
legend_text_size <- 11
strip_text_size <- 11

font_family <- "Arial"

my_colors <- c(
  "Human-only CI" = "#298C8C",
  "AI-assisted CI" = "#A00000"
)

plot <- ggplot(df_all, aes(x = Generation, y = Value, color = source)) +
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
      Metric == "Interest diversity" ~ scale_y_continuous( limits = c(0, 51) )
    )
  )+
  scale_color_manual(values = my_colors) +
  scale_x_continuous(
    breaks = c(0, 40000, 80000, 120000, 160000, 200000),
    labels = c(0, 4, 8, 12, 16, 20)
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = NULL,
    color = "Type"
  ) +
  theme_classic(base_family = font_family) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "none",
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
  file.path(save_path, "Avg_niche.png"),
  plot,
  width = 6,
  height = 5,
  bg = "white"
)


#------------------------------------------------
library(ggh4x)
library(ggplot2)
library(tidyr)
library(dplyr)

#------------------------------------------------------------
save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/"

load(file.path(save_path, "Original/clu_feedback.RData"))
ori_avg_feedback <- Result

load(file.path(save_path, "AI_answers_question/clu_feedback_Acc70_bias0.36_error0.3.RData"))
AI_avg_feedback <- Result

#------------------------------------------------------------
legend_order <- c(
  "Human-only CI",
  "AI-assisted CI"
)

metric_order <- c(
  "Interest diversity",
  "Median reliance on AI",
  "Collective bias",
  "Collective variance"
)

make_metric_df <- function(metric_name, y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_02 <- AI_avg_feedback[[metric_name]][idx]
  cf_01 <- ori_avg_feedback[[metric_name]][idx]
  
  data.frame(
    Generation = rep(idx, 2),
    Value = c(cf_01, cf_02),
    source = rep(c("Human-only CI", "AI-assisted CI"), each = length(idx)),
    Metric = y_label
  )
}

make_metric_df_AI_reliance <- function(y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- rep(0, 2000)
  cf_02 <- AI_avg_feedback[["median_AI_belief"]][idx]
  
  data.frame(
    Generation = rep(idx, 2),
    Value = c(cf_01, cf_02),
    source = rep(c("Human-only CI", "AI-assisted CI"), each = length(idx)),
    Metric = y_label
  )
}

df_all <- bind_rows(
  make_metric_df("interest_diversity", "Interest diversity"),
  make_metric_df("bias_sq", "Collective bias"),
  make_metric_df("variance", "Collective variance"),
  make_metric_df_AI_reliance("Median reliance on AI")
)

df_all$source <- factor(df_all$source, levels = legend_order)
df_all$Metric <- factor(df_all$Metric, levels = metric_order)

#------------------------------------------------------------
box_width <- 1.0

axis_title_size <- 12
axis_text_size <- 12
legend_title_size <- 13
legend_text_size <- 11
strip_text_size <- 11

font_family <- "Arial"

my_colors <- c(
  "Human-only CI" = "#298C8C",
  "AI-assisted CI" = "#A00000"
)

plot <- ggplot(df_all, aes(x = Generation, y = Value, color = source)) +
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
      Metric == "Interest diversity" ~ scale_y_continuous( limits = c(0, 51) )
    )
  )+
  scale_color_manual(values = my_colors) +
  scale_x_continuous(
    breaks = c(0, 40000, 80000, 120000, 160000, 200000),
    labels = c(0, 4, 8, 12, 16, 20)
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = NULL,
    color = "Type"
  ) +
  theme_classic(base_family = font_family) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "none",
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
  file.path(save_path, "Clu_feedback.png"),
  plot,
  width = 6,
  height = 5,
  bg = "white"
)


#------------------------------------------------
library(ggh4x)
library(ggplot2)
library(tidyr)
library(dplyr)

#------------------------------------------------------------
save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/"

load(file.path(save_path, "Original/clu_niche.RData"))
ori_avg_feedback <- Result

load(file.path(save_path, "AI_answers_question/clu_niche_Acc70_bias0.36_error0.3.RData"))
AI_avg_feedback <- Result

#------------------------------------------------------------
legend_order <- c(
  "Human-only CI",
  "AI-assisted CI"
)

metric_order <- c(
  "Interest diversity",
  "Median reliance on AI",
  "Collective bias",
  "Collective variance"
)

make_metric_df <- function(metric_name, y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_02 <- AI_avg_feedback[[metric_name]][idx]
  cf_01 <- ori_avg_feedback[[metric_name]][idx]
  
  data.frame(
    Generation = rep(idx, 2),
    Value = c(cf_01, cf_02),
    source = rep(c("Human-only CI", "AI-assisted CI"), each = length(idx)),
    Metric = y_label
  )
}

make_metric_df_AI_reliance <- function(y_label, by = 100) {
  idx <- seq(1, 200000, by = by)
  
  cf_01 <- rep(0, 2000)
  cf_02 <- AI_avg_feedback[["median_AI_belief"]][idx]
  
  data.frame(
    Generation = rep(idx, 2),
    Value = c(cf_01, cf_02),
    source = rep(c("Human-only CI", "AI-assisted CI"), each = length(idx)),
    Metric = y_label
  )
}

df_all <- bind_rows(
  make_metric_df("interest_diversity", "Interest diversity"),
  make_metric_df("bias_sq", "Collective bias"),
  make_metric_df("variance", "Collective variance"),
  make_metric_df_AI_reliance("Median reliance on AI")
)

df_all$source <- factor(df_all$source, levels = legend_order)
df_all$Metric <- factor(df_all$Metric, levels = metric_order)

#------------------------------------------------------------
box_width <- 1.0

axis_title_size <- 12
axis_text_size <- 12
legend_title_size <- 13
legend_text_size <- 11
strip_text_size <- 11

font_family <- "Arial"

my_colors <- c(
  "Human-only CI" = "#298C8C",
  "AI-assisted CI" = "#A00000"
)

plot <- ggplot(df_all, aes(x = Generation, y = Value, color = source)) +
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
      Metric == "Interest diversity" ~ scale_y_continuous( limits = c(0, 51) )
    )
  )+
  scale_color_manual(values = my_colors) +
  scale_x_continuous(
    breaks = c(0, 40000, 80000, 120000, 160000, 200000),
    labels = c(0, 4, 8, 12, 16, 20)
  ) +
  labs(
    x = expression(Generation~"(" * "\u00D7" * 10^4 * ")"),
    y = NULL,
    color = "Type"
  ) +
  theme_classic(base_family = font_family) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "none",
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
  file.path(save_path, "Clu_Niche.png"),
  plot,
  width = 6,
  height = 5,
  bg = "white"
)
