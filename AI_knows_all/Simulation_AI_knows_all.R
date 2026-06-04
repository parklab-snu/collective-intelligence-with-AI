# Collective intelligence simulation with AI_knows_all model
# All players share same AI assistent
# AI has two kinds of innate bias: coefficient bias (b^c) and intercept bias (b^i)
# AI has random error with zero mean, tau s.d.
# AI answers with the whole prediction accounting all interests(0 to m) regardless of the player's interest

# Compute the payoff of the single player.
# only supports averaging aggregation.
# supports Expert, Niche expert, Disadvantage AI Niche, Advantage AI Niche, Feedback, Disadvantage AI Feedback, and Advantage AI Feedback payoffs
# Disadvantage and Advantage AI payoffs are modified from Niche expert and Feedback payoffs. These payoffs leverage original payoff and AI payoff by "lambda"
# Disadvantaging AI is made by -lambda * beta_i, Advantaging AI is made by +lambda * beta_i
compute_payoff_one <- function(i, players, cluster_count, alpha, alpha_AI, sigma, AI_error_sd, bias, delta, feedback_global,
                               C_const, C_AI, S_alpha_bc, N, payoff_type, lambda) {
  k        <- players[i, 1] + 1
  alpha_e  <- alpha[k]
  bias_c_e  <- alpha_AI[k] - alpha[k]
  sigma_e  <- sigma[k]
  cA_i     <- players[i, 2]
  beta_i   <- players[i, 3]
  omq      <- 1 - beta_i
  
  
  if (payoff_type == "Feedback") {
    delta_e <- delta[k]
    beta_i * feedback_global + omq * cA_i * delta_e * sigma_e^2 -
      beta_i^2 * AI_error_sd^2 / N
  } else {
    if (k == 1) {
      expr <- omq^2 * cA_i^2 * sigma_e^2 +
        beta_i^2 * C_AI +
        omq^2 * C_const -
        2 * omq^2 * alpha_e * cA_i * sigma_e^2 +
        2 * omq * beta_i * cA_i * bias_c_e * sigma_e^2 -      
        2 * omq * beta_i * (alpha[1] * bias + S_alpha_bc) + 
        2 * omq * beta_i * cA_i * bias   
    } else{
      expr <- omq^2 * cA_i^2 * sigma_e^2 +
        beta_i^2 * C_AI +
        omq^2 * C_const -
        2 * omq^2 * alpha_e * cA_i * sigma_e^2 +
        2 * omq * beta_i * cA_i * bias_c_e * sigma_e^2 -      
        2 * omq * beta_i * (alpha[1] * bias + S_alpha_bc)
    }
    
    if (payoff_type == "Expert") {
      -expr
    } else if (payoff_type == "Niche expert") {
      rho_i <- cluster_count[k] / N
      -rho_i * expr
    } else if (payoff_type == "Disadvantage AI Niche"){
      rho_i <- cluster_count[k] / N
      #-rho_i *beta_i * expr
      -rho_i * expr - lambda * beta_i
    } else if (payoff_type == "Advantage AI Niche"){
      rho_i <- cluster_count[k] / N
      -rho_i * expr + lambda * beta_i
    } else if (payoff_type == "Disadvantage AI Feedback"){
      delta_e <- delta[k]
      beta_i * feedback_global + omq * cA_i * delta_e * sigma_e^2 -
        beta_i^2 * AI_error_sd^2 / N - lambda * beta_i
    } else if (payoff_type == "Advantage AI Feedback"){
      delta_e <- delta[k]
      beta_i * feedback_global + omq * cA_i * delta_e * sigma_e^2 -
        beta_i^2 * AI_error_sd^2 / N + lambda * beta_i
    }
  }
}

# Update the state values based on one step increments (deltas)
# For the efficient tracking of multiple scalar values, we used incremental approach instead of computing all the metric for each step.
# Because single imitation event happens per step (generation), we can track multiple scalar values based on the difference of two players who engaged in imitation (A and B)
# update_state_inplace function updates values for two clusters which engage in the imitation event  
update_state_inplace <- function(state, k_old, k_new, cA_old, cA_new, beta_old, beta_new, N, eps, alpha, sigma) {
  if (k_old != k_new) { # if two clusters are different
    # save the old values
    old_c_old <- state$cluster_count[k_old]
    old_c_new <- state$cluster_count[k_new]
    r_old <- old_c_old / N #ratio
    r_new <- old_c_new / N #ratio
    # compute removed entropy
    ent_remove <- r_old * log(r_old + eps) + r_new * log(r_new + eps)
    # update state
    state$cluster_count[k_old] <- old_c_old - 1
    state$cluster_count[k_new] <- old_c_new + 1
    nr_old <- state$cluster_count[k_old] / N
    nr_new <- state$cluster_count[k_new] / N
    # compute added entropy
    ent_add <- nr_old * log(nr_old + eps) + nr_new * log(nr_new + eps)
    # update entropy value
    state$entropy_sum <- state$entropy_sum - ent_remove + ent_add
    
    # for human accuracy, update cluster sum and cluster mean
    old_sum_old <- state$cluster_sum[k_old]
    old_sum_new <- state$cluster_sum[k_new]
    
    old_mean_old <- old_sum_old/N
    old_mean_new <- old_sum_new/N
    
    state$cluster_sum[k_old] <- old_sum_old - cA_old 
    state$cluster_sum[k_new] <- old_sum_new + cA_new
    new_mean_old <- state$cluster_sum[k_old]/N
    new_mean_new <- state$cluster_sum[k_new]/N
    
    old_err_term_old <- (alpha[k_old] - old_mean_old)^2 * sigma[k_old]^2
    old_err_term_new <- (alpha[k_new] - old_mean_new)^2 * sigma[k_new]^2
    new_err_term_old <- (alpha[k_old] - new_mean_old)^2 * sigma[k_old]^2
    new_err_term_new <- (alpha[k_new] - new_mean_new)^2 * sigma[k_new]^2
    
    state$cluster_mean[k_old] <- new_mean_old
    state$cluster_mean[k_new] <- new_mean_new
    
    state$human_error <- state$human_error - old_err_term_old - old_err_term_new + new_err_term_old + new_err_term_new
  } else{ #when there is no cluster change, we only need to track the belief change for human accuracy
    old_sum <- state$cluster_sum[k_old]
    old_mean <- old_sum/N
    state$cluster_sum[k_old] <- old_sum - cA_old + cA_new
    new_mean <- state$cluster_sum[k_old]/N
    state$cluster_mean[k_old] <- new_mean
    old_error <- (alpha[k_old] - old_mean)^2 * sigma[k_old]^2
    new_error <- (alpha[k_old] - new_mean)^2 * sigma[k_old]^2
    state$human_error <- state$human_error - old_error + new_error
  }
  # update gamma
  state$gamma[k_old] <- state$gamma[k_old] - cA_old * (1 - beta_old) / N
  state$gamma[k_new] <- state$gamma[k_new] + cA_new * (1 - beta_new) / N
  # update beta sum and beta square sum
  state$sum_beta  <- state$sum_beta  + beta_new - beta_old
  state$sum_beta2 <- state$sum_beta2 + beta_new^2 - beta_old^2
}

# Because incremental update could make small errors based on floating point error, we resynchronize state values by computing the state value as a whole occasionally.
resync_state <- function(state, players, m, N, eps, alpha, sigma) {
  state$cluster_count <- tabulate(players[, 1] + 1, nbins = m + 1)
  grp <- factor(players[, 1], levels = 0:m)
  beta <- players[, 3]
  belief <- players[, 2]
  gam <- as.numeric(tapply(belief * (1 - beta), grp, sum)) / N
  gam[is.na(gam)] <- 0
  state$gamma <- gam
  state$sum_beta  <- sum(beta)
  state$sum_beta2 <- sum(beta^2)
  ratios <- state$cluster_count / N
  state$entropy_sum <- sum(ratios * log(ratios + eps))
  
  # resynchronize cluster sum and cluster mean, human error for human accuracy tracking 
  sum_b <- as.numeric(tapply(players[, 2], grp, sum))
  sum_b[is.na(sum_b)] <- 0
  state$cluster_sum <- sum_b
  state$cluster_mean <- state$cluster_sum / N
  state$human_error <- sum((alpha - state$cluster_mean)^2 * sigma^2)
}

# main simulation
main_opt <- function(m, alpha, sigma, N, players, G, AI_error_sd, alpha_AI, bias_c, bias, payoff_type = "Niche expert", s = 50, mu = 0, eps = 1e-12,
                     resync_every = 1000, lambda) {
  # initialize tracking metrics and constants
  denom <- sum((alpha[-1] * sigma[-1])^2)
  C_const <- sum((alpha * sigma)^2)
  
  C_AI <- sum((bias_c * sigma)^2) + 2 * bias_c[1] * bias + bias^2 + AI_error_sd^2
  S_alpha_bc <- sum(alpha * bias_c * sigma^2)
  
  accuracy <- numeric(G)
  median_AI_belief <- numeric(G)
  interest_diversity <- numeric(G)
  players_intime <- array(0, dim = c(G %/% 1000, N, 3))
  bias_sq <- numeric(G)
  variance <- numeric(G)
  
  state <- new.env()
  state$cluster_count <- numeric(m + 1)
  state$gamma <- numeric(m + 1)
  state$sum_beta <- 0
  state$sum_beta2 <- 0
  state$entropy_sum <- 0
  
  # for human error tracking
  state$cluster_sum <- numeric(m + 1)
  state$cluster_mean <- numeric(m + 1)
  state$human_error <- 0
  human_accuracy <- numeric(G)
  
  # initialize with resync_state
  resync_state(state, players, m, N, eps, alpha, sigma)
  # initialize median AI belief
  current_median <- median(players[, 3])
  
  # main loop for G generations
  for (g in 1:G) {
    # Record first, imitate later
    beta_mean <- state$sum_beta / N
    delta <- alpha - beta_mean * alpha_AI - state$gamma
    delta[1] <- delta[1] - beta_mean * bias
    var_nu_mean <- (AI_error_sd^2 / N^2) * state$sum_beta2
    error <- sum(delta^2 * sigma^2) + var_nu_mean
    bias_sq[g]     <- (delta[1])^2
    variance[g] <- error - bias_sq[g]
    accuracy[g] <- 1 - error / denom
    
    # only for human accuracy
    human_error <- state$human_error
    human_accuracy[g] <- 1 - human_error/denom
    
    median_AI_belief[g] <- current_median
    interest_diversity[g] <- exp(-state$entropy_sum)
    if (g %% 1000 == 0) {
      players_intime[g %/% 1000, , ] <- players
      # cat("Generation:", g, "\n")
      # cat("Interest diversity:", sprintf("%.2f", interest_diversity[g]), "\n")
      # cat("Median AI belief:",   sprintf("%.2f", median_AI_belief[g]),   "\n")
      # cat("Accuracy:",           sprintf("%.2f", accuracy[g]),           "\n")
      # cat("\n")
    }
    
    if (payoff_type == "Feedback" || payoff_type == "Disadvantage AI Feedback" || payoff_type == "Advantage AI Feedback") {
      feedback_global <- (alpha_AI[1] + bias) * delta[1] +
        sum(alpha_AI[-1] * delta[-1] * sigma[-1]^2)
    } else {
      feedback_global <- 0
    }
    
    # sample two players
    A <- sample.int(N, 1)
    B <- sample.int(N, 1)
    while (B == A) B <- sample.int(N, 1)
    
    r1 <- runif(1)
    state_changed <- FALSE
    if (r1 >= mu) {
      # compute payoff of two players
      payoff_A <- compute_payoff_one(A, players, state$cluster_count, alpha, alpha_AI, sigma, AI_error_sd, bias, delta, feedback_global, C_const, C_AI, S_alpha_bc, N, payoff_type, lambda)
      payoff_B <- compute_payoff_one(B, players, state$cluster_count, alpha, alpha_AI, sigma, AI_error_sd, bias, delta, feedback_global, C_const, C_AI, S_alpha_bc, N, payoff_type, lambda)
      
      p_imitate <- 1 / (1 + exp(s * (payoff_A - payoff_B)))
      r2 <- runif(1)
      if (r2 < p_imitate) {
        # save old and new interest number
        k_old <- players[A, 1] + 1
        k_new <- players[B, 1] + 1
        # save old and new belief value
        cA_old <- players[A, 2]
        cA_new <- players[B, 2]
        # save old and new AI belief value
        beta_old <- players[A, 3]
        beta_new <- players[B, 3]
        # Imitate
        players[A, ] <- players[B, ]
        
        # update state
        update_state_inplace(state, k_old, k_new, cA_old, cA_new, beta_old, beta_new, N, eps, alpha, sigma)
        state_changed <- TRUE
      }
    } else { # with mutation
      # sample new interest and belief
      new_interest <- sample(0:m, 1)
      new_belief <- rnorm(1, mean = 0, sd = 5)
      new_AI <- runif(1, 0, 1)
      
      # save old and new values
      k_old <- players[A, 1] + 1
      k_new <- new_interest + 1
      cA_old <- players[A, 2]
      cA_new <- new_belief
      beta_old <- players[A, 3]
      beta_new <- new_AI
      
      # mutate
      players[A, ] <- c(new_interest, new_belief, new_AI)
      # update state
      update_state_inplace(state, k_old, k_new, cA_old, cA_new, beta_old, beta_new, N, eps, alpha, sigma)
      state_changed <- TRUE
    }
    if (state_changed) { # when state is updated, recompute the AI belief median
      current_median <- median(players[, 3])
    }
    
    # resync for accuracy
    if (g %% resync_every == 0) {
      resync_state(state, players, m, N, eps, alpha, sigma)
    }
  }
  list(accuracy           = accuracy,
       players_intime     = players_intime,
       median_AI_belief   = median_AI_belief,
       interest_diversity = interest_diversity,
       bias_sq = bias_sq,
       variance = variance,
       human_accuracy = human_accuracy)
}


out_dir <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective intelligence/0520/Adv_Disadv/AI_knows_all/Disadv_feedback"


for(i in 0:8){
  for(j in 0:8){
    set.seed(42)  
    m <- 50
    alpha <- runif(m+1, min = -5, max = 5)
    
    #sigma <- rep(1, m+1)
    sigma <- runif(m, min = 0, max = 3)
    sigma <- c(1, sigma)
    
    N <- 10000
    G <- 160000
    belief <- rnorm(N, mean = 0, sd = 100)
    #Sample initial interest (SRS form 0 to 50)
    interest <- sample(0:m, size = N, replace = TRUE)
    #Sample initial AI belief
    AI_belief <- runif(N, min = 0, max = 1)
    #AI_belief <- rep(1, N)
    #Build player
    players <- cbind(interest, belief, AI_belief)
    
    bias_c <- rep(0, m+1)
    bias_i <- rep(j/20, m+1)
    bias <- sum(bias_i)
    
    lambda <- 2^(i)
    
    alpha_AI <- alpha + bias_c
    AI_error_sd <- 0.3
    #corr<- cor(alpha, bias, method = "pearson")
    denom <- sum((alpha[-1]*sigma[-1])^2)
    AI_accuracy <- 1- (sum(bias_c^2*sigma^2) + 2*bias_c[1]*bias + bias^2 + AI_error_sd^2)/denom
    cat("Accuracy:", AI_accuracy, "\n")
    
    Result <- main_opt(m, alpha, sigma, N, players, G, AI_error_sd, alpha_AI, bias_c, bias, payoff_type = 'Disadvantage AI Feedback', lambda = lambda)
    
    filename <- sprintf("avg_adv_i%02d_j%02d.RData", i, j)
    filepath <- file.path(out_dir, filename)
    
    save(i, j, bias, AI_error_sd, AI_accuracy, Result, file = filepath)
  }
}

# sample the environment
set.seed(42)
# number of factors
m <- 50
# make coef
alpha <- runif(m+1, min = -5, max = 5)
#alpha <- alpha <- seq(-5, 5, length.out = m+1)
#sigma <- rep(1, m+1)
# sample sigma
sigma <- runif(m, min = 0, max = 3)
sigma <- c(1, sigma)

# build players
# number of players
N <- 10000
G <- 160000
# sample initial belief (Sample from normal distribution)
belief <- rnorm(N, mean = 0, sd = 100)
#Sample initial interest (SRS form 0 to 50)
interest <- sample(0:m, size = N, replace = TRUE)
#Sample initial AI belief
AI_belief <- runif(N, min = 0, max = 1)
#AI_belief <- rep(1, N)
# build players
players <- cbind(interest, belief, AI_belief)

# coefficient bias
bias_c <- rep(0.0, m+1)
# intercept bias
bias_i <- rep(0.8, m+1)
# intercept bias sum
bias <- sum(bias_i)

# error
AI_error_sd <- 0.3

alpha_AI <- alpha + bias_c
denom <- sum((alpha[-1]*sigma[-1])^2)
AI_accuracy <- 1- (sum(bias_c^2*sigma^2) + 2*bias_c[1]*bias + bias^2 + AI_error_sd^2)/denom
cat("Accuracy:", AI_accuracy, "\n")

# lambda for Advantage AI / Disadvantage AI payoffs
lambda <- 100

# run simulation
# payoff_type = "Expert" / Niche expert" / "Feedback" / "Advantage AI Niche" / Disadvantage AI Niche" / "Advantage AI Feedback" / "Disadvantage AI Feedback"
Result <- main_opt(m, alpha, sigma, N, players, G, AI_error_sd, alpha_AI, bias_c, bias, payoff_type = 'Advantage AI Feedback', lambda = lambda)

filename <- sprintf("avg_feedback_Acc70_bias18.5.RData")
filepath <- file.path(out_dir, filename)

save(bias, AI_error_sd, AI_accuracy, Result, file = filepath)
