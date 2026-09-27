#Set your project path
project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"

Chatbot_AI_triple <- new.env()

source(
  file.path(project_path, "Functions", "Chatbot_AI_triple_balanced.R"),
  local = Chatbot_AI_triple
)

out_dir <- file.path(project_path, "Simulations", "FigureS9_simulation")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#=========================================================================
#Clustering aggregation
bias_list <- list(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
for(i in bias_list){
  set.seed(42)
  m <- 50
  alpha <- runif(m+1, min = -5, max = 5)
  sigma <- runif(m, min = 0, max = 3)
  sigma <- c(1, sigma)
  N <- 10000
  G <- 200000
  belief <- rnorm(N, mean = 0, sd = 5)
  #Sample initial interest (SRS form 0 to 50)
  interest <- sample(0:m, size = N, replace = TRUE)
  #Sample initial AI belief
  AI_belief <- runif(N, min = 0, max = 1)
  #Build player
  players <- cbind(interest, belief, AI_belief)
  
  bias_i <- rep(i, m+1)
  bias <- sum(bias_i)
  AI_error_sd <- 0.3
  denom <- sum((alpha[-1]*sigma[-1])^2)
  AI_accuracy <- 1- (sum(bias_i)^2 + AI_error_sd^2)/denom
  cat("Accuracy:", AI_accuracy, "\n")
  
  Result<- Chatbot_AI_triple$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Unrevised balanced')
  
  filename <- sprintf("chatbot_unrevised_balanced_i%02f.RData", i)
  filepath <- file.path(out_dir, filename)
  
  save(i, bias_i, AI_error_sd, AI_accuracy, Result, file = filepath)
}

for(i in bias_list){
  set.seed(42)
  m <- 50
  alpha <- runif(m+1, min = -5, max = 5)
  sigma <- runif(m, min = 0, max = 3)
  sigma <- c(1, sigma)
  N <- 10000
  G <- 200000
  belief <- rnorm(N, mean = 0, sd = 5)
  #Sample initial interest (SRS form 0 to 50)
  interest <- sample(0:m, size = N, replace = TRUE)
  #Sample initial AI belief
  AI_belief <- runif(N, min = 0, max = 1)
  #Build player
  players <- cbind(interest, belief, AI_belief)
  
  bias_i <- rep(i, m+1)
  bias <- sum(bias_i)
  AI_error_sd <- 0.3
  denom <- sum((alpha[-1]*sigma[-1])^2)
  AI_accuracy <- 1- (sum(bias_i)^2 + AI_error_sd^2)/denom
  cat("Accuracy:", AI_accuracy, "\n")
  
  Result<- Chatbot_AI_triple$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Combined balanced')
  
  filename <- sprintf("chatbot_combined_balanced_i%02f.RData", i)
  filepath <- file.path(out_dir, filename)
  
  save(i, bias_i, AI_error_sd, AI_accuracy, Result, file = filepath)
}

for(i in bias_list){
  set.seed(42)
  m <- 50
  alpha <- runif(m+1, min = -5, max = 5)
  sigma <- runif(m, min = 0, max = 3)
  sigma <- c(1, sigma)
  N <- 10000
  G <- 200000
  belief <- rnorm(N, mean = 0, sd = 5)
  #Sample initial interest (SRS form 0 to 50)
  interest <- sample(0:m, size = N, replace = TRUE)
  #Sample initial AI belief
  AI_belief <- runif(N, min = 0, max = 1)
  #Build player
  players <- cbind(interest, belief, AI_belief)
  
  bias_i <- rep(i, m+1)
  bias <- sum(bias_i)
  AI_error_sd <- 0.3
  denom <- sum((alpha[-1]*sigma[-1])^2)
  AI_accuracy <- 1- (sum(bias_i)^2 + AI_error_sd^2)/denom
  cat("Accuracy:", AI_accuracy, "\n")
  
  Result<- Chatbot_AI_triple$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Balanced')
  
  filename <- sprintf("chatbot_balanced_i%02f.RData", i)
  filepath <- file.path(out_dir, filename)
  
  save(i, bias_i, AI_error_sd, AI_accuracy, Result, file = filepath)
}

