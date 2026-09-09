#Set your project path
project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"

Chatbot_AI <- new.env()
Omniscient_AI <- new.env()
Without_AI <- new.env()

source(
  file.path(project_path, "Functions", "Chatbot_AI.R"),
  local = Chatbot_AI
)

source(
  file.path(project_path, "Functions", "Omniscient_AI.R"),
  local = Omniscient_AI
)

source(
  file.path(project_path, "Functions", "Without_AI.R"),
  local = Without_AI
)

out_dir <- file.path(project_path, "Simulations", "FigureS4_simulation")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#=========================================================================
#Clustering aggregation
set.seed(42)  
m <- 50
alpha <- runif(m+1, min = -5, max = 5)
sigma <- runif(m, min = 0, max = 3)
sigma <- c(1, sigma)
N <- 10000
G <- 1000000
belief <- rnorm(N, mean = 0, sd = 5)
#Sample initial interest (SRS form 0 to 50)
interest <- sample(0:m, size = N, replace = TRUE)
#Sample initial AI belief
AI_belief <- runif(N, min = 0, max = 1)
#AI_belief <- rep(1, N)
#Build player
players <- cbind(interest, belief, AI_belief)

bias_i <- rep(0.4, m+1)
AI_error_sd <- 0.3


#Chatbot AI under feedback structure
Result <- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Feedback', mu = 0)

filename <- sprintf("chatbot_feedback_0.4_0.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)

#Chatbot AI under feedback structure
Result <- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Feedback', mu = 0.01)

filename <- sprintf("chatbot_feedback_0.4_0.01.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)

players <- cbind(interest, belief)

#Without AI under feedback structure
Result <- Without_AI$main_opt(m, alpha, sigma, N, players, G, agg_type = 'clustering', payoff_type = 'Feedback', mu = 0)

filename <- sprintf("without_feedback_clu_0.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)

#Without AI under feedback structure
Result <- Without_AI$main_opt(m, alpha, sigma, N, players, G, agg_type = 'clustering', payoff_type = 'Feedback', mu = 0.01)

filename <- sprintf("without_feedback_clu_0.01.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)


#=============================================================
#mutation rate sweep
#chatbot
mu_list <- list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07)
bias_list <- list(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
for(i in bias_list){
  for(j in mu_list){
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
    lambda <- 0
    denom <- sum((alpha[-1]*sigma[-1])^2)
    AI_accuracy <- 1- (sum(bias_i)^2 + AI_error_sd^2)/denom
    cat("Accuracy:", AI_accuracy, "\n")
    
    Result<- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Feedback', lambda = lambda, mu = j)
    
    filename <- sprintf("chatbot_feedback_i%02f_j%02f.RData", i, j)
    filepath <- file.path(out_dir, filename)
    
    save(i, j, bias_i, AI_error_sd, AI_accuracy, Result, file = filepath)
  }
}

#without AI
mu_list <- list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07)
for(j in mu_list){
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
  #Build player
  players <- cbind(interest, belief)
  
  Result <- Without_AI$main_opt(m, alpha, sigma, N, players, G, agg_type = 'clustering', payoff_type = 'Feedback', mu = j)
  
  filename <- sprintf("without_feedback_j%02f.RData", j)
  filepath <- file.path(out_dir, filename)
  
  save(Result, file = filepath)
}

#=============================================================
#belief sd sweep
#chatbot
sd_list <- list(3, 4, 5, 6, 7, 8, 9, 10)
bias_list <- list(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
for(i in bias_list){
  for(j in sd_list){
    set.seed(42)
    m <- 50
    alpha <- runif(m+1, min = -5, max = 5)
    sigma <- runif(m, min = 0, max = 3)
    sigma <- c(1, sigma)
    N <- 10000
    G <- 200000
    belief <- rnorm(N, mean = 0, sd = j)
    #Sample initial interest (SRS form 0 to 50)
    interest <- sample(0:m, size = N, replace = TRUE)
    #Sample initial AI belief
    AI_belief <- runif(N, min = 0, max = 1)
    #Build player
    players <- cbind(interest, belief, AI_belief)
    
    bias_i <- rep(i, m+1)
    bias <- sum(bias_i)
    AI_error_sd <- 0.3
    lambda <- 0
    denom <- sum((alpha[-1]*sigma[-1])^2)
    AI_accuracy <- 1- (sum(bias_i)^2 + AI_error_sd^2)/denom
    cat("Accuracy:", AI_accuracy, "\n")
    
    Result<- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Feedback', lambda = lambda)
    
    filename <- sprintf("chatbot_feedback_i%02f_j%02f.RData", i, j)
    filepath <- file.path(out_dir, filename)
    
    save(i, j, bias_i, AI_error_sd, AI_accuracy, Result, file = filepath)
  }
}

#without AI
sd_list <- list(3, 4, 5, 6, 7, 8, 9, 10)
for(j in sd_list){
  set.seed(42)
  m <- 50
  alpha <- runif(m+1, min = -5, max = 5)
  sigma <- runif(m, min = 0, max = 3)
  sigma <- c(1, sigma)
  N <- 10000
  G <- 200000
  belief <- rnorm(N, mean = 0, sd = j)
  #Sample initial interest (SRS form 0 to 50)
  interest <- sample(0:m, size = N, replace = TRUE)
  #Build player
  players <- cbind(interest, belief)
  
  Result <- Without_AI$main_opt(m, alpha, sigma, N, players, G, agg_type = 'clustering', payoff_type = 'Feedback')
  
  filename <- sprintf("without_feedback_j%02f.RData", j)
  filepath <- file.path(out_dir, filename)
  
  save(Result, file = filepath)
}


