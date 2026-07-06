#------------- median trajectory plot across 30 simulations per bias value

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(viridis)
library(patchwork)
library(scales)
library(colorspace)

# ----- 1. Path and settings -----
save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_knows_all/Nicheexpert_repeat"

#save_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/AI_answers_question/Nicheexpert_biassweep"

lambda_value <- 0

bias_list <- c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6 )

rep_list <- 1:1

G <- 200000

# 20,000 generation을 균등하게 stratify해서 시각화용 generation 추출
n_gen_plot <- 20000
gen_keep <- unique(round(seq(1, G, length.out = n_gen_plot)))

metrics <- c(
  "accuracy",
  "human_accuracy",
  "interest_diversity",
  "median_AI_belief",
  "bias_sq",
  "variance"
)

metric_labels <- c(
  accuracy           = "Accuracy",
  human_accuracy     = "Human accuracy",
  interest_diversity = "Diversity",
  median_AI_belief   = "Median AI belief",
  bias_sq            = "Collective bias",
  variance           = "Collective variance"
)

# ----- 2. Load RData and build long dataframe -----

load_one_run <- function(k, lambda_value, bias_value, gen_keep, metrics) {
  
  filename <- sprintf(
    "adv_feedback_k%02d_i%03d_j%02f.RData",
    k, lambda_value, bias_value
  )
  
  fp <- file.path(save_path, filename)
  
  if (!file.exists(fp)) {
    warning(paste("Missing:", fp))
    return(NULL)
  }
  
  env <- new.env()
  load(fp, envir = env)
  
  R <- env$Result
  
  out <- map_dfr(metrics, function(metric) {
    
    if (!metric %in% names(R)) {
      warning(paste("Metric not found:", metric, "in", fp))
      return(NULL)
    }
    
    x <- R[[metric]]
    
    tibble(
      generation = gen_keep,
      value      = x[gen_keep],
      metric     = metric
    )
  })
  
  out %>%
    mutate(
      rep_id = k,
      lambda = lambda_value,
      bias_i = bias_value
    )
}

df_raw <- map_dfr(bias_list, function(bias_value) {
  map_dfr(rep_list, function(k) {
    load_one_run(
      k = k,
      lambda_value = lambda_value,
      bias_value = bias_value,
      gen_keep = gen_keep,
      metrics = metrics
    )
  })
})

# ----- 3. Median trajectory by generation and bias value -----

df_median <- df_raw %>%
  group_by(metric, bias_i, generation) %>%
  summarise(
    median_value = median(value, na.rm = TRUE),
    n_rep = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(
    bias_i_f = factor(
      sprintf("%.1f", bias_i),
      levels = sprintf("%.1f", sort(unique(bias_i)))
    ),
    metric_label = factor(
      metric_labels[metric],
      levels = unname(metric_labels[metrics])
    )
  )

# Optional: check whether all bias-generation-metric combinations have 30 runs
df_check <- df_median %>%
  group_by(metric, bias_i) %>%
  summarise(
    min_n_rep = min(n_rep),
    max_n_rep = max(n_rep),
    .groups = "drop"
  )

print(df_check, n= 100)

# ----- 4. Style -----

font_family <- "Arial"

common_theme <- theme_classic(base_family = font_family) +
  theme(
    panel.grid       = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.title       = element_text(size = 20, family = font_family),
    axis.text        = element_text(size = 13, family = font_family),
    legend.title     = element_blank(),
    legend.text      = element_text(size = 13, family = font_family),
    plot.title       = element_text(
      hjust = 0.5,
      size = 20,
      family = font_family,
      margin = margin(b = 6)
    )
  )

# ----- 5. Trajectory plot factory -----

# make_trajectory <- function(metric, title_text) {
#   
#   ggplot(
#     df_median %>% filter(metric == !!metric),
#     aes(
#       x = generation,
#       y = median_value,
#       color = bias_i
#     )
#   ) +
#     geom_line(linewidth = 0.4, alpha = 0.95) +
#     scale_color_viridis_c(
#       option = "plasma",
#       direction = 1,
#       breaks = pretty_breaks(n = 5),
#       guide = guide_colorbar(
#         barheight    = unit(4, "cm"),
#         barwidth     = unit(0.5, "cm"),
#         frame.colour = "black",
#         ticks.colour = "black"
#       )
#     ) +
#     scale_x_continuous(
#       labels = comma,
#       expand = expansion(mult = c(0.01, 0.02))
#     ) +
#     labs(
#       x = "Generation",
#       y = title_text,
#       color = expression(bias[i]),
#       title = title_text
#     ) +
#     common_theme
# }

make_trajectory <- function(metric, title_text) {
  
  ggplot(
    df_median %>% filter(metric == !!metric),
    aes(
      x = generation,
      y = median_value,
      color = bias_i,
      group = bias_i_f
    )
  ) +
    geom_line(linewidth = 1.3, alpha = 0.9) +
    scale_color_viridis_c(
      option = "plasma",
      direction = 1,
      breaks = c(-0.6, -0.3, 0, 0.3, 0.6),
      labels = c("-0.6", "-0.3", "0", "0.3", "0.6"),
      guide = guide_colorbar(
        barheight    = unit(4, "cm"),
        barwidth     = unit(0.5, "cm"),
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    # scale_color_continuous_divergingx(
    #   palette = "Spectral",
    #   mid = 0,
    #   limits = c(-0.6, 0.6),
    #   breaks = c(-0.6, -0.3, 0, 0.3, 0.6),
    #   labels = c("-0.6", "-0.3", "0", "0.3", "0.6"),
    #   guide = "none"
    # )+
    scale_x_continuous(
      limits = c(0, 200000),
      breaks = c(0, 50000, 100000, 150000, 200000),
      labels = c(0, 5, 10, 15, 20),
      expand = c(0.02, 0)
    ) +
    labs(
      x = NULL,
      y = NULL,
      color = expression(bias[i]),
      title = title_text
    ) +
    common_theme+
    theme(legend.position = "none")
}

p_acc     <- make_trajectory("accuracy",           "Accuracy")
p_human   <- make_trajectory("human_accuracy",     "Human accuracy")
p_div <- make_trajectory("interest_diversity", "Diversity") + scale_y_continuous(limits = c(0, 51))
p_belief  <- make_trajectory("median_AI_belief",   "Median AI belief")
p_bias_sq <- make_trajectory("bias_sq",            "Collective bias")
p_var     <- make_trajectory("variance",           "Collective variance")

# ----- 6. Combine and save -----

combined <- (p_acc + p_human + p_div) / 
  (p_belief + p_bias_sq + p_var)

ggsave(
  file.path(save_path, "Diverging.png"),
  combined,
  width = 10,
  height = 7,
  bg = "white",
  dpi = 300
)
