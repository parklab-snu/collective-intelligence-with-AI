# Collective intelligence simulation based on Wang et al. "Individual incentives that promote collective intelligence" (PNAS, 2025)

# Compute the payoff of the single player.
# supports for both averaging aggregation and clustering aggregation, Expert, Niche expert, Feedback payoff.
# See original paper for the definition of each aggregation and payoff.
compute_payoff_one <- function(i, players, cluster_sum, cluster_count, cluster_mean, alpha, sigma, payoff_type, agg_type, C_const, N) {
  k <- players[i, 1] + 1
  alpha_e <- alpha[k]
  sigma_e <- sigma[k]
  belief_i <- players[i, 2]
  
  if (payoff_type == "Expert") {
    -((belief_i^2 - 2 * belief_i * alpha_e) * sigma_e^2)
  } else if (payoff_type == "Niche expert") {
    rho_i <- cluster_count[k] / N
    -rho_i * ((belief_i^2 - 2 * belief_i * alpha_e) * sigma_e^2 + C_const)
  } else if (payoff_type == "Feedback") {
    if (agg_type == "clustering") {
      mu_e <- cluster_mean[k]
    } else if (agg_type == "averaging"){
      mu_e <- cluster_sum[k] / N
    }
    belief_i * (alpha_e - mu_e) * sigma_e^2
  }
}

# Update the cluster values based on one step increments (deltas)
# For the efficient tracking of multiple scalar values, we used incremental approach instead of computing all the metric for each step.
# Because single imitation event happens per step (generation), we can track multiple scalar values based on the difference of two players who engaged in imitation (A and B)
# We compute delta_count and delta_sum for change of cluster count (how many players are in the cluster; players who has same interest are in the same cluster) and cluster sum (belief sum of each cluster) at the main loop
# update the tracked scalar value in the update_cluster_inplace function
update_cluster_inplace <- function(state, k, delta_count, delta_sum, alpha, sigma, agg_type, N, eps) {
  # save old values for k <- the interest which is engaged with the imitation event
  old_count <- state$cluster_count[k]
  old_sum <- state$cluster_sum[k]
  old_ratio <- old_count / N
  old_ent_term <- old_ratio * log(old_ratio + eps)
  
  if (agg_type == "clustering") {
    old_mean <- if (old_count > 0) old_sum / old_count else 0
  } else if (agg_type == "averaging") {
    old_mean <- old_sum / N
  }
  old_err_term <- (alpha[k] - old_mean)^2 * sigma[k]^2
  
  # compute new values
  new_count <- old_count + delta_count
  new_sum <- old_sum + delta_sum
  # update state
  state$cluster_count[k] <- new_count
  state$cluster_sum[k] <- new_sum
  
  # compute new values
  new_ratio <- new_count / N
  new_ent_term <- new_ratio * log(new_ratio + eps)
  
  if (agg_type == "clustering") {
    new_mean <- if (new_count > 0) new_sum / new_count else 0
  } else if (agg_type == "averaging") {
    new_mean <- new_sum / N
  }
  # compute error
  new_err_term <- (alpha[k] - new_mean)^2 * sigma[k]^2
  
  # update state
  state$cluster_mean[k] <- new_mean
  state$error_part <- state$error_part - old_err_term + new_err_term
  state$entropy_sum <- state$entropy_sum - old_ent_term + new_ent_term
}

# Because incremental update could make small errors based on floating point error, we resynchronize state values by computing the state value as a whole occasionally.
resync_state <- function(state, players, m, N, alpha, sigma, agg_type, eps) {
  state$cluster_count <- tabulate(players[, 1] + 1, nbins = m + 1)
  grp <- factor(players[, 1], levels = 0:m)
  sum_b <- as.numeric(tapply(players[, 2], grp, sum))
  sum_b[is.na(sum_b)] <- 0
  state$cluster_sum <- sum_b
  if (agg_type == "clustering") {
    state$cluster_mean <- ifelse(state$cluster_count > 0, state$cluster_sum / pmax(state$cluster_count, 1), 0)
  } else if (agg_type == "aggregation") {
    state$cluster_mean <- state$cluster_sum / N
  }
  state$error_part <- sum((alpha - state$cluster_mean)^2 * sigma^2)
  ratios <- state$cluster_count / N
  state$entropy_sum <- sum(ratios * log(ratios + eps))
}


# main simulation
main_opt <- function(m, alpha, sigma, N, players, G, agg_type = "clustering", payoff_type = "Feedback", s = 50, mu = 0, eps = 1e-12, resync_every = 1000) {
  # initialize tracking metrics and constants
  denom <- sum((alpha[-1] * sigma[-1])^2)
  C_const <- sum(alpha^2 * sigma^2)
  accuracy <- numeric(G)
  interest_diversity <- numeric(G)
  players_intime <- array(0, dim = c(G %/% 1000, N, 2))
  bias_sq <- numeric(G)
  variance <- numeric(G)
  
  # initialize state
  state <- new.env()
  state$cluster_sum <- numeric(m + 1)
  state$cluster_count <- numeric(m + 1)
  state$cluster_mean <- numeric(m + 1)
  state$error_part <- 0
  state$entropy_sum <- 0
  
  # initialize with resync_state
  resync_state(state, players, m, N, alpha, sigma, agg_type, eps)
  
  # main loop for G generations
  for (g in 1:G) {
    # Record first, imitate later
    error <- state$error_part
    accuracy[g] <- 1 - error / denom
    bias_sq[g] <- (alpha[1] - state$cluster_mean[1])^2
    variance[g] <- error - bias_sq[g]
    interest_diversity[g] <- exp(-state$entropy_sum)
    
    if (g %% 1000 == 0) {
      players_intime[g %/% 1000, , ] <- players
      #cat("Generation:", g, "\n")
      #cat("Interest diversity:", sprintf("%.2f", interest_diversity[g]), "\n")
      #cat("Accuracy:",           sprintf("%.2f", accuracy[g]),           "\n")
      #cat("\n")
    }
    
    # sample two players
    A <- sample.int(N, 1)
    B <- sample.int(N, 1)
    while (B == A) B <- sample.int(N, 1)
    
    r1 <- runif(1)
    # without mutation
    if (r1 >= mu) {
      # compute payoff of two players
      payoff_A <- compute_payoff_one(A, players, state$cluster_sum, state$cluster_count, state$cluster_mean, alpha, sigma, payoff_type, agg_type, C_const, N)
      payoff_B <- compute_payoff_one(B, players, state$cluster_sum, state$cluster_count, state$cluster_mean, alpha, sigma, payoff_type, agg_type, C_const, N)
      
      # compute the probability of imitation
      p_imitate <- 1 / (1 + exp(s * (payoff_A - payoff_B)))
      r2 <- runif(1)
      
      if (r2 < p_imitate) {
        # save old and new interest number
        k_old <- players[A, 1] + 1
        k_new <- players[B, 1] + 1
        
        # save old and new belief value
        b_old <- players[A, 2]
        b_new <- players[B, 2]
        
        # Imitate
        players[A, ] <- players[B, ]
        
        if (k_old == k_new) {
          # if two players are in the same cluster (same interest), cluster count doesn't change, cluster sum changes with value (b_new - b_old) 
          update_cluster_inplace(state, k_old, 0L, b_new - b_old, alpha, sigma, agg_type, N, eps)
        } else {
          # if two players are in the different cluster, we should update both of them
          update_cluster_inplace(state, k_old, -1L, -b_old, alpha, sigma, agg_type, N, eps)
          update_cluster_inplace(state, k_new, +1L,  b_new, alpha, sigma, agg_type, N, eps)
        }
      }
    } else { # with mutation
      # sample new interest and belief
      new_interest <- sample(0:m, 1)
      new_belief <- rnorm(1, mean = 0, sd = 5)
      # save old and new values
      k_old <- players[A, 1] + 1
      k_new <- new_interest + 1
      b_old <- players[A, 2]
      b_new <- new_belief
      
      # mutate
      players[A, ] <- c(new_interest, new_belief)
      
      if (k_old == k_new) {
        # if original player and mutated interest are same, cluster count doesn't change, cluster sum changes with value (b_new - b_old) 
        update_cluster_inplace(state, k_old, 0L, b_new - b_old, alpha, sigma, agg_type, N, eps)
      } else {
        # if mutated interest is different from the original, we should update both of them
        update_cluster_inplace(state, k_old, -1L, -b_old, alpha, sigma, agg_type, N, eps)
        update_cluster_inplace(state, k_new, +1L,  b_new, alpha, sigma, agg_type, N, eps)
      }
    }
    
  }
  
  # resync for accuracy
  if (g %% resync_every == 0) {
      resync_state(state, players, m, N, alpha, sigma, agg_type, eps)
  }
  
  # Return recordings
  list(accuracy = accuracy,
       players_intime = players_intime,
       interest_diversity = interest_diversity,
       bias_sq = bias_sq,
       variance = variance)
}

# sample the environment
set.seed(42)
# number of factors
m <- 50
# make coef
#alpha <- seq(-5, 5, length.out = m+1)
alpha <- runif(m+1, min = -5, max = 5)
# sample sigma
sigma <- runif(m, min = 0, max = 3)
sigma <- c(1, sigma)

# build players
# number of players
N <- 10000
# sample initial belief (Sample from normal distribution)
belief <- rnorm(N, mean = 0, sd = 5)
# sample initial interest (SRS form 0 to 50)
interest <- sample(0:m, size = N, replace = TRUE)
# build player
players <- cbind(interest, belief)

# run simulation
G <- 160000

# agg_type = "clustering" / "averaging"
# payoff_type = "Expert" / Niche expert" / "Feedback"
Result <- main_opt(m, alpha, sigma, N, players, G, agg_type = 'clustering', payoff_type = 'Niche expert')
out_dir <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective intelligence/0520/Original"

filename <- sprintf("clu_niche.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)

library(ggplot2)
library(tidyr)
library(dplyr)
# exploratory visualization
accuracy <- Result$accuracy
interest_diversity <- Result$interest_diversity
median_AI_belief <- Result$median_AI_belief

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

plot <- ggplot(df_long, aes(x = Generation, y = value, color = metric, linetype = metric)) +
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

averaging_niche_players_intime <- Result$players_intime

belief <- averaging_niche_players_intime[100,,]

df_cl_fe_sc <- as.data.frame(belief)
cl_fe_sc <- ggplot(df_cl_fe_sc, aes(x = V1, y = V2))+
  geom_point(
    size = 2,
    color = "red",
    alpha = 0.2 
  )+
  labs(x = "Interest", y = "Belief")
cl_fe_sc
