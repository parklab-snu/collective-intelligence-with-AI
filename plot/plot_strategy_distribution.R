library(plotly)
library(dplyr)

set.seed(42)  
m <- 50
alpha <- runif(m+1, min = -5, max = 5)

#sigma <- rep(1, m+1)
sigma <- runif(m, min = 0, max = 3)
sigma <- c(1, sigma)

N <- 10000
G <- 200000
belief <- rnorm(N, mean = 0, sd = 5)
#Sample initial interest (SRS form 0 to 50)
interest <- sample(0:m, size = N, replace = TRUE)
#Sample initial AI belief
AI_belief <- runif(N, min = 0, max = 1)
#AI_belief <- rep(1, N)
#Build player
players <- cbind(interest, belief, AI_belief)

bias_c <- rep(0, m+1)
bias_i <- rep(-0.3, m+1)
bias <- bias_i
alpha_AI <- alpha + bias_c
AI_error_sd <- 0.3

lambda <- 0

#corr<- cor(alpha, bias, method = "pearson")
denom <- sum((alpha[-1]*sigma[-1])^2)
AI_accuracy <- 1- (sum(bias_c^2*sigma^2) + 2*bias_c[1]*sum(bias_i) + sum(bias_i)^2 + AI_error_sd^2)/denom
cat("Accuracy:", AI_accuracy, "\n")


load("C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-Intelligence-with-AI/AI_answers_question/Nicheexpert_biassweep/adv_niche_k01_i000_j-0.300000.Rdata")

players_intime <- Result$players_intime

compute_p_revised_vec <- function(interest, belief, AI_belief, alpha_AI) {
  AI_belief * alpha_AI[interest + 1] + (1 - AI_belief) * belief
}

compute_payoff_one <- function(i, players, p_revised, cluster_sum, cluster_count, cluster_mean, B_bar, alpha, sigma, bias, AI_error_sd,
                               payoff_type, agg_type, C_const, N, lambda, alpha_AI) {
  k <- players[i, 1] + 1
  alpha_e <- alpha[k]
  sigma_e <- sigma[k]
  bias_e <- bias[k]
  beta_i <- players[i, 3]
  p_i <- p_revised[i]
  
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
    } else if (payoff_type == "Niche expert") {
      rho_i <- cluster_count[k] / N
      -rho_i * expr
    } else if (payoff_type == "Advantage AI Niche") {
      rho_i <- cluster_count[k] / N
      -rho_i * expr + lambda * beta_i
    } else if (payoff_type == "Disadvantage AI Niche") {
      rho_i <- cluster_count[k] / N
      -rho_i * expr - lambda * beta_i
    }
  } else {
    mu_e <- cluster_mean[k]
    count_e <- cluster_count[k]
    delta_0 <- alpha[1] - cluster_mean[1] - B_bar
    
    if (k == 1) {
      first_term <- p_i * delta_0 * sigma_e^2
    } else {
      first_term <- p_i * (alpha_e - mu_e) * sigma_e^2
    }
    
    if (payoff_type == "Feedback") {
      first_term + beta_i * bias_e * delta_0 - beta_i^2 * AI_error_sd^2 / count_e
    } else if (payoff_type == "Advantage AI Feedback") {
      first_term + beta_i * bias_e * delta_0 - beta_i^2 * AI_error_sd^2 / count_e + lambda * beta_i
    } else if (payoff_type == "Disadvantage AI Feedback") {
      first_term + beta_i * bias_e * delta_0 - beta_i^2 * AI_error_sd^2 / count_e - lambda * beta_i
    } else if (payoff_type == "Balanced") {
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
      
      (-rho_i * expr) + first_term + beta_i * bias_e * delta_0 - beta_i^2 * AI_error_sd^2 / count_e
    } else if (payoff_type == "AI Feedback collective") {
      q <- p_i - alpha_AI[k]
      gamma <- 1 - beta_i
      
      if (k == 1) {
        ft <- q * delta_0 * sigma_e^2
      } else {
        ft <- q * (alpha_e - mu_e) * sigma_e^2
      }
      
      ft - gamma * bias_e * delta_0 + beta_i * gamma * AI_error_sd^2 / count_e
    }
  }
}

compute_snapshot_payoffs <- function(players, alpha, sigma, alpha_AI, bias, AI_error_sd, payoff_type, lambda, m) {
  N <- nrow(players)
  
  p_revised <- compute_p_revised_vec(players[, 1], players[, 2], players[, 3], alpha_AI)
  grp <- factor(players[, 1], levels = 0:m)
  
  cluster_count <- tabulate(players[, 1] + 1, nbins = m + 1)
  cluster_sum <- as.numeric(tapply(p_revised, grp, sum))
  cluster_beta_sum <- as.numeric(tapply(players[, 3], grp, sum))
  
  cluster_mean <- cluster_sum / cluster_count
  B_bar <- sum((cluster_beta_sum / cluster_count) * bias)
  C_const <- sum(alpha^2 * sigma^2)
  
  payoff <- sapply(seq_len(N), function(i) {
    compute_payoff_one(
      i, players, p_revised,
      cluster_sum, cluster_count, cluster_mean,
      B_bar, alpha, sigma, bias, AI_error_sd,
      payoff_type, "clustering", C_const, N, lambda, alpha_AI
    )
  })
  
  tibble(
    id = seq_len(N),
    interest = players[, 1],
    belief = players[, 2],
    AI_belief = players[, 3],
    p_revised = p_revised,
    payoff = payoff
  )
}

plot_strategy_3d_payoff <- function(snapshot_id, payoff_type, lambda, m = 50, q = 0.75) {
  players_snapshot <- players_intime[snapshot_id, , ]
  
  df <- compute_snapshot_payoffs(
    players_snapshot,
    alpha, sigma, alpha_AI, bias, AI_error_sd,
    payoff_type, lambda, m
  )
  
  cutoff <- quantile(abs(df$payoff), q)
  df_bg <- df %>% filter(abs(payoff) < cutoff)
  df_fg <- df %>% filter(abs(payoff) >= cutoff)
  
  max_abs_payoff <- max(abs(df$payoff))
  
  plot_ly() %>%
    add_trace(
      data = df_bg,
      x = ~interest,
      y = ~belief,
      z = ~AI_belief,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 1.5,
        opacity = 0.05,
        color = "gray"
      ),
      hoverinfo = "none",
      showlegend = FALSE
    ) %>%
    add_trace(
      data = df_fg,
      x = ~interest,
      y = ~belief,
      z = ~AI_belief,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 3,
        opacity = 0.85,
        color = ~payoff,
        colorscale = "RdBu",
        reversescale = TRUE,
        cmin = -max_abs_payoff,
        cmax = max_abs_payoff,
        colorbar = list(title = "Payoff")
      ),
      text = ~paste0(
        "ID: ", id,
        "<br>Interest: ", interest,
        "<br>Belief: ", round(belief, 3),
        "<br>AI belief: ", round(AI_belief, 3),
        "<br>Revised belief: ", round(p_revised, 3),
        "<br>Payoff: ", round(payoff, 5)
      ),
      hoverinfo = "text",
      showlegend = FALSE
    ) %>%
    layout(
      title = paste0("Snapshot ", snapshot_id, " | ", payoff_type, " | lambda = ", lambda),
      scene = list(
        xaxis = list(title = "Interest", range = c(0, 50)),
        yaxis = list(title = "Belief", range = c(-10, 10)),
        zaxis = list(title = "AI belief", range = c(0, 1))
      )
    )
}

plot_strategy_3d_payoff(
  snapshot_id = 30,
  payoff_type = "Feedback",
  lambda = 0,
  q = 0.7
)


