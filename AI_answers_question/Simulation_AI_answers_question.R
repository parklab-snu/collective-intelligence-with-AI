# Collective intelligence simulation with AI_knows_all model
# All players share same AI assistent
# AI has two kinds of innate bias: coefficient bias (b^c) and intercept bias (b^i)
# AI has random error with zero mean, tau s.d.
# AI answers prediction based on the players' interest


compute_p_revised_vec <- function(interest, belief, AI_belief, alpha_AI) {
  AI_belief * alpha_AI[interest + 1] + (1 - AI_belief) * belief
}

compute_payoff_one <- function(i, players, p_revised,
                               cluster_sum, cluster_count, cluster_mean, B_bar,
                               alpha, sigma, bias, AI_error_sd,
                               payoff_type, agg_type, C_const, N, lambda) {
  k       <- players[i, 1] + 1
  alpha_e <- alpha[k]
  sigma_e <- sigma[k]
  bias_e <- bias[k]
  beta_i  <- players[i, 3]
  p_i     <- p_revised[i]
  if (payoff_type == "Expert" || payoff_type == "Niche expert" || payoff_type == "Advantage AI Niche" || payoff_type == "Disadvantage AI Niche") {
    expr <- (p_i^2 - 2 * p_i * alpha_e) * sigma_e^2 +
      C_const +
      beta_i^2 * AI_error_sd^2 +
      beta_i^2 * bias_e^2
    
    if (k == 1) {
      expr <- expr + 2 * (p_i - alpha_e) * beta_i * bias_e
    } else {
      expr <- expr - 2 * alpha[1] * beta_i * bias_e
    }
    
    if (payoff_type == "Expert") {
      -expr
    } else if(payoff_type == "Niche expert"){
      rho_i <- cluster_count[k] / N
      -rho_i * expr
    } else if(payoff_type == "Advantage AI Niche"){
      rho_i <- cluster_count[k] / N
      -rho_i * expr + lambda * beta_i
    } else if(payoff_type == "Disadvantage AI Niche"){
      rho_i <- cluster_count[k] / N
      -rho_i * expr - lambda * beta_i
    }
  } else {
    mu_e    <- cluster_mean[k]
    count_e <- cluster_count[k]
    delta_0 <- alpha[1] - cluster_mean[1] - B_bar
    if(k ==1 ){
      first_term <- p_i * delta_0 * sigma_e^2
    } else{
      first_term <- p_i * (alpha_e - mu_e) * sigma_e^2
    }
    if(payoff_type == "Feedback"){
      first_term + beta_i * bias_e * delta_0 - beta_i^2 * AI_error_sd^2 / count_e 
    } else if(payoff_type == "Advantage AI Feedback"){
      first_term + beta_i * bias_e * delta_0 - beta_i^2 * AI_error_sd^2 / count_e + lambda * beta_i
    } else if (payoff_type == "Disadvantage AI Feedback"){
      first_term + beta_i * bias_e * delta_0 - beta_i^2 * AI_error_sd^2 / count_e - lambda * beta_i
    } else if (payoff_type == "Balanced"){
      rho_i <- cluster_count[k] / N
      
      expr <- (p_i^2 - 2 * p_i * alpha_e) * sigma_e^2 +
        C_const +
        beta_i^2 * AI_error_sd^2 +
        beta_i^2 * bias_e^2
      
      if (k == 1) {
        expr <- expr + 2 * (p_i - alpha_e) * beta_i * bias_e
      } else {
        expr <- expr - 2 * alpha[1] * beta_i * bias_e
      }
      
      (1-lambda)*(-rho_i * expr) + lambda*(first_term + beta_i * bias_e * delta_0 - beta_i^2 * AI_error_sd^2 / count_e)
    }
  }
}

update_cluster_inplace <- function(state, k,
                                   delta_count, delta_sum, delta_beta2, delta_beta,
                                   alpha, sigma, bias, AI_error_sd,
                                   agg_type, N, eps, delta_human) {
  old_count <- state$cluster_count[k]
  old_sum   <- state$cluster_sum[k]
  old_beta_sum <- state$cluster_beta_sum[k]
  old_beta2 <- state$cluster_beta2_sum[k]
  old_ratio    <- old_count / N
  old_ent_term <- old_ratio * log(old_ratio + eps)
  
  
  old_mean     <- if (old_count > 0) old_sum / old_count else 0
  if(k == 1){
    old_err_term <- 0
  } else{
    old_err_term <- (alpha[k] - old_mean)^2 * sigma[k]^2
  }
  
  old_delta0_term <- (alpha[1] - state$cluster_mean[1] - state$B_bar)^2 * sigma[1]^2
  old_var_term <- if (old_count > 0) (AI_error_sd^2 / old_count^2) * old_beta2 else 0
  old_Btilde <- if (old_count > 0) (old_beta_sum / old_count) * bias[k] else 0
  
  new_beta_sum <- old_beta_sum + delta_beta
  new_count <- old_count + delta_count
  new_sum   <- old_sum   + delta_sum
  new_beta2 <- old_beta2 + delta_beta2
  new_Btilde <- if (new_count > 0) (new_beta_sum / new_count) * bias[k] else 0
  new_ratio    <- new_count / N
  new_ent_term <- new_ratio * log(new_ratio + eps)
  new_mean     <- if (new_count > 0) new_sum / new_count else 0
  
  
  state$B_bar <- state$B_bar - old_Btilde + new_Btilde
  state$cluster_count[k]     <- new_count
  state$cluster_sum[k]       <- new_sum
  state$cluster_beta2_sum[k] <- new_beta2
  state$cluster_beta_sum[k] <- new_beta_sum
  state$cluster_mean[k] <- new_mean
  
  if(k == 1){
    new_err_term <- 0
  } else{
    new_err_term <- (alpha[k] - new_mean)^2 * sigma[k]^2
  }
  
  new_delta0_term <- (alpha[1] - state$cluster_mean[1] - state$B_bar)^2 * sigma[1]^2
  new_var_term <- if (new_count > 0) (AI_error_sd^2 / new_count^2) * new_beta2 else 0
  state$error_part  <- state$error_part - old_err_term    + new_err_term - old_delta0_term + new_delta0_term
  state$entropy_sum  <- state$entropy_sum  - old_ent_term + new_ent_term
  state$var_nu_mean <- state$var_nu_mean - old_var_term + new_var_term
  
  #Human error
  old_human_mean <- state$cluster_human_mean[k]
  new_human_mean <- if (new_count > 0) (old_human_mean*old_count + delta_human)/new_count else 0
  state$cluster_human_mean[k] <- new_human_mean
  
  old_error <- (alpha[k] - old_human_mean)^2*sigma[k]^2
  
  new_error <- (alpha[k] - new_human_mean)^2*sigma[k]^2
  
  state$human_error <- state$human_error - old_error + new_error
  
}

resync_state <- function(state, players, p_revised, m, N, alpha, sigma, bias,
                         AI_error_sd, agg_type, eps) {
  state$cluster_count <- tabulate(players[, 1] + 1, nbins = m + 1)
  grp <- factor(players[, 1], levels = 0:m)
  sum_pr <- as.numeric(tapply(p_revised,      grp, sum))
  sum_b2 <- as.numeric(tapply(players[, 3]^2, grp, sum))
  sum_pr[is.na(sum_pr)] <- 0
  sum_b2[is.na(sum_b2)] <- 0
  state$cluster_sum       <- sum_pr
  state$cluster_beta2_sum <- sum_b2
  
  sum_b <- as.numeric(tapply(players[, 3], grp, sum))
  sum_b[is.na(sum_b)] <- 0
  state$cluster_beta_sum <- sum_b
  nz <- state$cluster_count > 0
  state$B_bar <- sum(
    (state$cluster_beta_sum[nz] / state$cluster_count[nz]) * bias[nz]
  )
  
  state$cluster_mean <- ifelse(state$cluster_count > 0, state$cluster_sum / pmax(state$cluster_count, 1), 0)
  state$error_part <- sum((alpha[-1] - state$cluster_mean[-1])^2 * sigma[-1]^2) + (alpha[1] - state$cluster_mean[1] - state$B_bar)^2*sigma[1]^2
  total_beta2_sum <- sum(players[, 3]^2)
  nz <- state$cluster_count > 0
  state$var_nu_mean <- sum((AI_error_sd^2 / state$cluster_count[nz]^2) * state$cluster_beta2_sum[nz])
  ratios <- state$cluster_count / N
  state$entropy_sum <- sum(ratios * log(ratios + eps))
  state$total_beta2 <- total_beta2_sum
  
  
  #human error
  sum_human <- as.numeric(tapply(players[ ,2],      grp, sum))
  sum_human[is.na(sum_human)] <- 0
  state$cluster_human_mean <- ifelse(state$cluster_count > 0, sum_human / pmax(state$cluster_count, 1), 0)
  state$human_error <- sum((alpha - state$cluster_human_mean)^2*sigma^2)
  
}

main_opt <- function(m, alpha, sigma, N, players, G, alpha_AI, bias, AI_error_sd,
                     agg_type    = "clustering",
                     payoff_type = "Feedback",
                     s = 50, mu = 0, eps = 1e-12,
                     resync_every = 1000, lambda) {
  denom   <- sum((alpha[-1] * sigma[-1])^2)
  C_const <- sum(alpha^2 * sigma^2)
  accuracy           <- numeric(G)
  median_AI_belief   <- numeric(G)
  interest_diversity <- numeric(G)
  players_intime     <- array(0, dim = c(G %/% 1000, N, 3))
  bias_sq               <- numeric(G)
  variance           <- numeric(G)
  state <- new.env()
  state$cluster_sum       <- numeric(m + 1)
  state$cluster_count     <- numeric(m + 1)
  state$cluster_beta2_sum <- numeric(m + 1)
  state$cluster_beta_sum  <- numeric(m + 1)
  state$cluster_mean      <- numeric(m + 1)
  state$B_bar             <- 0
  state$error_part        <- 0
  state$var_nu_mean       <- 0
  state$entropy_sum       <- 0
  state$total_beta2       <- 0
  
  #human error
  state$cluster_human_mean <- numeric(m + 1)
  state$human_error <- 0
  human_accuracy <- numeric(G)
  
  p_revised <- compute_p_revised_vec(players[, 1], players[, 2],
                                     players[, 3], alpha_AI)
  resync_state(state, players, p_revised, m, N, alpha, sigma, bias,
               AI_error_sd, agg_type, eps)
  total_beta2_sum <- state$total_beta2
  current_median <- median(players[, 3])
  for (g in 1:G) {
    error       <- state$error_part + state$var_nu_mean
    bias_sq[g] <- (alpha[1] - state$cluster_mean[1] - state$B_bar)^2
    variance[g] <- error - bias_sq[g]
    accuracy[g] <- 1 - error / denom
    
    #Human accuracy
    human_accuracy[g] <- 1- state$human_error / denom
    
    A <- sample.int(N, 1)
    B <- sample.int(N, 1)
    while (B == A) B <- sample.int(N, 1)
    r1 <- runif(1)
    state_changed <- FALSE
    if (r1 >= mu) {
      payoff_A <- compute_payoff_one(A, players, p_revised,
                                     state$cluster_sum, state$cluster_count,
                                     state$cluster_mean, state$B_bar,
                                     alpha, sigma, bias, AI_error_sd,
                                     payoff_type, agg_type, C_const, N, lambda)
      payoff_B <- compute_payoff_one(B, players, p_revised,
                                     state$cluster_sum, state$cluster_count,
                                     state$cluster_mean, state$B_bar,
                                     alpha, sigma, bias, AI_error_sd,
                                     payoff_type, agg_type, C_const, N, lambda)
      p_imitate <- 1 / (1 + exp(s * (payoff_A - payoff_B)))
      r2 <- runif(1)
      if (r2 < p_imitate) {
        k_old    <- players[A, 1] + 1
        k_new    <- players[B, 1] + 1
        p_old    <- p_revised[A]
        p_new    <- p_revised[B]
        human_old <- players[A, 2]
        human_new <- players[B, 2]
        beta_old <- players[A, 3]
        beta_new <- players[B, 3]
        players[A, ] <- players[B, ]
        p_revised[A] <- p_new
        if (k_old == k_new) {
          update_cluster_inplace(state, k_old, 0L,
                                 p_new - p_old, beta_new^2 - beta_old^2, beta_new - beta_old,
                                 alpha, sigma, bias, AI_error_sd, agg_type, N, eps, human_new - human_old)
        } else {
          update_cluster_inplace(state, k_old, -1L, -p_old, -beta_old^2, -beta_old,
                                 alpha, sigma, bias, AI_error_sd, agg_type, N, eps, -human_old)
          update_cluster_inplace(state, k_new, +1L,  p_new,  beta_new^2, beta_new,
                                 alpha, sigma, bias, AI_error_sd, agg_type, N, eps, +human_new)
        }
        state_changed <- TRUE
      }
    } else {
      new_interest  <- sample(0:m, 1)
      new_belief    <- rnorm(1, mean = 0, sd = 5)
      new_AI_belief <- runif(1, 0, 1)
      k_old    <- players[A, 1] + 1
      k_new    <- new_interest + 1
      human_old <- players[A, 2]
      human_new <- new_belief
      p_old    <- p_revised[A]
      p_new    <- new_AI_belief * alpha_AI[k_new] +
        (1 - new_AI_belief) * new_belief
      beta_old <- players[A, 3]
      beta_new <- new_AI_belief
      players[A, ] <- c(new_interest, new_belief, new_AI_belief)
      p_revised[A] <- p_new
      
      if (k_old == k_new) {
        update_cluster_inplace(state, k_old, 0L,
                               p_new - p_old, beta_new^2 - beta_old^2, beta_new - beta_old,
                               alpha, sigma, bias, AI_error_sd, agg_type, N, eps, human_new - human_old)
      } else {
        update_cluster_inplace(state, k_old, -1L, -p_old, -beta_old^2, -beta_old,
                               alpha, sigma, bias, AI_error_sd, agg_type, N, eps, -human_old)
        update_cluster_inplace(state, k_new, +1L,  p_new,  beta_new^2, beta_new,
                               alpha, sigma, bias, AI_error_sd, agg_type, N, eps, +human_new)
      }
      
      state_changed <- TRUE
    }
    if (state_changed) {
      current_median <- median(players[, 3])
    }
    median_AI_belief[g] <- current_median
    interest_diversity[g] <- exp(-state$entropy_sum)
    if (g %% 1000 == 0) {
      players_intime[g %/% 1000, , ] <- players
      # cat("Generation:", g, "\n")
      # cat("Interest diversity:", sprintf("%.2f", interest_diversity[g]), "\n")
      # cat("Median AI belief:",   sprintf("%.2f", median_AI_belief[g]),   "\n")
      # cat("Accuracy:",           sprintf("%.2f", accuracy[g]),           "\n")
      # cat("Bias:",               sprintf("%.2f", bias[g]),               "\n")
      # cat("Variance:",           sprintf("%.2f", variance[g]),           "\n")
      # cat("\n")
    }
    if (g %% resync_every == 0) {
      resync_state(state, players, p_revised, m, N, alpha, sigma, bias,
                   AI_error_sd, agg_type, eps)
      total_beta2_sum <- state$total_beta2
    }
  }
  list(accuracy           = accuracy,
       players_intime     = players_intime,
       median_AI_belief   = median_AI_belief,
       interest_diversity = interest_diversity,
       bias_sq               = bias_sq,
       variance           = variance,
       human_accuracy = human_accuracy)
}

out_dir <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective intelligence/0520/Balanced"

for(i in -8:8){
  for(j in -8:8){
    set.seed(42)  
    m <- 50
    alpha <- runif(m+1, min = -5, max = 5)
    
    #sigma <- rep(1, m+1)
    sigma <- runif(m, min = 0, max = 3)
    sigma <- c(1, sigma)
    
    N <- 10000
    G <- 160000
    belief <- rnorm(N, mean = 0, sd = 5)
    #Sample initial interest (SRS form 0 to 50)
    interest <- sample(0:m, size = N, replace = TRUE)
    #Sample initial AI belief
    AI_belief <- runif(N, min = 0, max = 1)
    #AI_belief <- rep(1, N)
    #Build player
    players <- cbind(interest, belief, AI_belief)
    
    bias_c <- rep(i/5, m+1)
    bias_i <- rep(j/20, m+1)
    alpha_AI <- alpha + bias_c
    AI_error_sd <- 0.3
    
    lambda <- 0.5
    
    #corr<- cor(alpha, bias, method = "pearson")
    denom <- sum((alpha[-1]*sigma[-1])^2)
    AI_accuracy <- 1- (sum(bias_c^2*sigma^2) + 2*bias_c[1]*sum(bias_i) + sum(bias_i)^2 + AI_error_sd^2)/denom
    cat("Accuracy:", AI_accuracy, "\n")
    
    Result<- main_opt(m, alpha, sigma, N, players, G, alpha_AI, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Balanced', lambda = lambda)
    
    filename <- sprintf("balanced_i%02d_j%02d.RData", i, j)
    filepath <- file.path(out_dir, filename)
    
    save(i, j, bias_c, bias_i, alpha_AI, AI_error_sd, AI_accuracy, Result, file = filepath)
  }
}

set.seed(42)  
m <- 50
alpha <- runif(m+1, min = -5, max = 5)
#sigma <- rep(1, m+1)
sigma <- runif(m, min = 0, max = 3)
sigma <- c(1, sigma)
N <- 50000
G <- 500000
belief <- rnorm(N, mean = 0, sd = 5)
#Sample initial interest (SRS form 0 to 50)
interest <- sample(0:m, size = N, replace = TRUE)
#Sample initial AI belief
AI_belief <- runif(N, min = 0, max = 1)
#AI_belief <- rep(1, N)
#Build player
players <- cbind(interest, belief, AI_belief)

bias_c <- rep(0.2, m+1)
bias_i <- runif(m+1, min = -1.0, max = 1.0)
#bias_i <- rep(-0.2, m+1)
alpha_AI <- alpha + bias_c
AI_error_sd <- 0.3
#corr<- cor(alpha, bias, method = "pearson")
denom <- sum((alpha[-1]*sigma[-1])^2)
AI_accuracy <- 1- (sum(bias_c^2*sigma^2) + 2*bias_c[1]*sum(bias_i) + sum(bias_i)^2 + AI_error_sd^2)/denom
cat("Accuracy:", AI_accuracy, "\n")

lambda <- 0.5

Result <- main_opt(m, alpha, sigma, N, players, G, alpha_AI, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Niche expert', lambda = lambda)

filename <- sprintf("clu_niche_Acc70_biasc0.8_biasi_0.3.RData")
filepath <- file.path(out_dir, filename)

save(bias_i, alpha_AI, AI_error_sd, AI_accuracy, Result, file = filepath)



# make_bias_perm <- function(alpha, mag, target_cor, n_perm = 1000000) {
#   
#   bias_values <- seq(-mag, mag, length.out = length(alpha))
#   
#   best_bias <- NULL
#   best_cor <- NA
#   best_diff <- Inf
#   
#   candidates <- list(
#     sort(bias_values),
#     rev(sort(bias_values))
#   )
#   
#   for (bias in candidates) {
#     current_cor <- cor(alpha, bias)
#     current_diff <- abs(current_cor - target_cor)
#     
#     if (current_diff < best_diff) {
#       best_bias <- bias
#       best_cor <- current_cor
#       best_diff <- current_diff
#     }
#   }
#   
#   for (i in 1:n_perm) {
#     
#     bias <- sample(bias_values)
#     
#     current_cor <- cor(alpha, bias)
#     current_diff <- abs(current_cor - target_cor)
#     
#     if (current_diff < best_diff) {
#       best_bias <- bias
#       best_cor <- current_cor
#       best_diff <- current_diff
#     }
#     
#     if(best_diff < 0.0001) break
#   }
#   return(best_bias)
# }

library(ggplot2)
library(tidyr)
library(dplyr)

#Main visualization
accuracy <- Result$accuracy
median_AI_belief <- Result$median_AI_belief
interest_diversity <- Result$interest_diversity

df <- data.frame(
  Generation = seq_along(accuracy),
  Accuracy = accuracy,
  Diversity = interest_diversity,
  AI_belief = median_AI_belief,
  source = "clustering_feedback_AI"
)

df_long <- pivot_longer(
  df,
  cols = c(Accuracy, Diversity, AI_belief),
  names_to = "metric",
  values_to = "value"
)

plot <- ggplot(df_long,
               aes(x = Generation,
                   y = value,
                   color = metric,
                   linetype = metric)) +
  geom_line(linewidth = 1) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c(
    "Accuracy" = "blue",
    "Diversity" = "red",
    "AI_belief" = "green"
  )) +
  scale_linetype_manual(values = c(
    "Accuracy" = "solid",
    "Diversity" = "dashed",
    "AI_belief" = "solid"
  ))+
  labs(title = "Clustering Feedback AI")
plot

players_intime <- Result$players_intime
belief <- players_intime[160,,]

df_cl_fe_sc <- as.data.frame(belief)
cl_fe_sc <- ggplot(df_cl_fe_sc, aes(x = V1, y = V3))+
  geom_point(
    size = 2,
    color = "red",
    alpha = 0.2 
  )+
  labs(x = "Interest", y = "Belief")
cl_fe_sc

players_intime <- Result$players_intime
belief <- players_intime[500,,]

df_cl_fe_sc <- as.data.frame(belief)

library(ggplot2)
library(scales)
library(grid)

font_family <- "Arial"

common_theme <- theme_classic(base_family = font_family) +
  theme(
    panel.grid       = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    #panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
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

cl_fe_sc <- ggplot(df_cl_fe_sc, aes(x = V1, y = V3)) +
  geom_bin2d(bins = 50) +
  scale_fill_gradient(
    low = "#3B0F70",
    high = "#FCA636"
  ) +
  labs(
    x = "Interest",
    y = "AI belief",
    fill = "Count"
  ) +
  common_theme

cl_fe_sc

df_cl_fe_sc$V1 <- factor(df_cl_fe_sc$V1)

cl_fe_sc <- ggplot(df_cl_fe_sc, aes(x = V1, y = V3)) +
  geom_violin(
    aes(fill = after_stat(density)),
    scale = "width",
    trim = TRUE
  ) +
  scale_fill_gradient(
    low = "#3B0F70",
    high = "#FCA636"
  ) +
  labs(
    x = "Interest",
    y = "AI belief",
    fill = "Density"
  ) +
  common_theme

cl_fe_sc

library(ggplot2)

df_cl_fe_sc$V1 <- factor(df_cl_fe_sc$V1)

cl_fe_sc <- ggplot(df_cl_fe_sc, aes(x = V1, y = V3)) +
  geom_violin(
    fill = "grey80",
    color = "black",
    linewidth = 0.5,
    trim = TRUE,
    scale = "width"
  ) +
  labs(
    x = "Interest",
    y = "AI belief",
    title = "AI belief distribution by interest"
  ) +
  common_theme

cl_fe_sc

ggsave(
  file.path(save_path, "AI_belief_density_snapshot_160.png"),
  cl_fe_sc,
  width = 4, height = 4, bg = "white"
)

library(ggplot2)
library(dplyr)

df_cl_fe_sc <- as.data.frame(belief)

n_y_bins <- 50

df_heat <- df_cl_fe_sc %>%
  mutate(
    interest = factor(V1),
    AI_bin = cut(V3, breaks = n_y_bins)
  ) %>%
  count(interest, AI_bin, name = "n") %>%
  group_by(interest) %>%
  mutate(density = n / sum(n)) %>%
  ungroup()

cl_fe_sc <- ggplot(df_heat, aes(x = interest, y = AI_bin, fill = density)) +
  geom_tile() +
  scale_fill_gradient(
    low = "#3B0F70",
    high = "#FCA636",
    name = "Density"
  ) +
  labs(
    x = "Interest",
    y = "AI belief",
    title = "AI belief density by interest"
  ) +
  common_theme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

cl_fe_sc

library(ggplot2)
library(dplyr)
library(tidyr)

df_cl_fe_sc <- as.data.frame(belief)

n_y_bins <- 50

breaks_y <- seq(
  min(df_cl_fe_sc$V3, na.rm = TRUE),
  max(df_cl_fe_sc$V3, na.rm = TRUE),
  length.out = n_y_bins + 1
)

mids_y <- (breaks_y[-1] + breaks_y[-length(breaks_y)]) / 2

df_heat <- df_cl_fe_sc %>%
  mutate(
    interest = factor(V1),
    AI_bin_id = cut(
      V3,
      breaks = breaks_y,
      include.lowest = TRUE,
      labels = FALSE
    )
  ) %>%
  count(interest, AI_bin_id, name = "n") %>%
  complete(interest, AI_bin_id = 1:n_y_bins, fill = list(n = 0)) %>%
  group_by(interest) %>%
  mutate(density = n / sum(n)) %>%
  ungroup() %>%
  mutate(AI_mid = mids_y[AI_bin_id])

cl_fe_sc <- ggplot(df_heat, aes(x = interest, y = AI_mid, fill = density)) +
  geom_tile() +
  scale_fill_gradient(
    low = "#3B0F70",
    high = "#FCA636",
    limits = c(0, max(df_heat$density)),
    name = "Density"
  ) +
  labs(
    x = "Interest",
    y = "AI belief",
    title = "AI belief density by interest"
  ) +
  common_theme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

cl_fe_sc
