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

out_dir <- file.path(project_path, "Simulations", "Figure3_simulation")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#=========================================================================
#Averaging aggregation
lambda_list <- list(-40, -30, -20, -10, 0, 10, 20, 30, 40)
bias_list <- list(-0.4, -0.2, 0.2, 0.4)
for(i in bias_list){
  for(j in lambda_list){
    set.seed(42)
    m <- 50
    alpha <- runif(m+1, min = -5, max = 5)
    sigma <- runif(m, min = 0, max = 3)
    sigma <- c(1, sigma)
    N <- 10000
    G <- 200000
    belief <- rnorm(N, mean = 0, sd = 100)
    #Sample initial interest (SRS form 0 to 50)
    interest <- sample(0:m, size = N, replace = TRUE)
    #Sample initial AI belief
    AI_belief <- runif(N, min = 0, max = 1)
    #AI_belief <- rep(1, N)
    #Build player
    players <- cbind(interest, belief, AI_belief)
    
    bias_i <- rep(i, m+1)
    bias <- sum(bias_i)
    AI_error_sd <- 0.3
    lambda <- j
    denom <- sum((alpha[-1]*sigma[-1])^2)
    AI_accuracy <- 1- (sum(bias_i)^2 + AI_error_sd^2)/denom
    cat("Accuracy:", AI_accuracy, "\n")
    
    Result <- Omniscient_AI$main_opt(m, alpha, sigma, N, players, G, AI_error_sd, bias, payoff_type = 'Advantage AI Feedback', lambda = lambda)
    
    filename <- sprintf("omni_feedback_i%02f_j%02f.RData", i, j)
    filepath <- file.path(out_dir, filename)
      
    save(i, j, bias_i, AI_error_sd, AI_accuracy, Result, file = filepath)
  }
}

for(i in bias_list){
  for(j in lambda_list){
    set.seed(42)
    m <- 50
    alpha <- runif(m+1, min = -5, max = 5)
    sigma <- runif(m, min = 0, max = 3)
    sigma <- c(1, sigma)
    N <- 10000
    G <- 200000
    belief <- rnorm(N, mean = 0, sd = 100)
    #Sample initial interest (SRS form 0 to 50)
    interest <- sample(0:m, size = N, replace = TRUE)
    #Sample initial AI belief
    AI_belief <- runif(N, min = 0, max = 1)
    #AI_belief <- rep(1, N)
    #Build player
    players <- cbind(interest, belief, AI_belief)
    
    bias_i <- rep(i, m+1)
    bias <- sum(bias_i)
    AI_error_sd <- 0.3
    lambda <- j
    denom <- sum((alpha[-1]*sigma[-1])^2)
    AI_accuracy <- 1- (sum(bias_i)^2 + AI_error_sd^2)/denom
    cat("Accuracy:", AI_accuracy, "\n")
    
    Result <- Omniscient_AI$main_opt(m, alpha, sigma, N, players, G, AI_error_sd, bias, payoff_type = 'Advantage AI Niche', lambda = lambda)
    
    filename <- sprintf("omni_niche_i%02f_j%02f.RData", i, j)
    filepath <- file.path(out_dir, filename)
    
    save(i, j, bias_i, AI_error_sd, AI_accuracy, Result, file = filepath)
  }
}


#=========================================================================
#Clustering aggregation
lambda_list <- list(-40, -30, -20, -10, 0, 10, 20, 30, 40)
bias_list <- list(-0.4, -0.2, 0.2, 0.4)
for(i in bias_list){
  for(j in lambda_list){
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
    lambda <- j
    denom <- sum((alpha[-1]*sigma[-1])^2)
    AI_accuracy <- 1- (sum(bias_i)^2 + AI_error_sd^2)/denom
    cat("Accuracy:", AI_accuracy, "\n")
    
    Result<- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Advantage AI Feedback', lambda = lambda)
    
    filename <- sprintf("chatbot_feedback_i%02f_j%02f.RData", i, j)
    filepath <- file.path(out_dir, filename)
    
    save(i, j, bias_i, AI_error_sd, AI_accuracy, Result, file = filepath)
  }
}

for(i in bias_list){
  for(j in lambda_list){
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
    lambda <- j
    denom <- sum((alpha[-1]*sigma[-1])^2)
    AI_accuracy <- 1- (sum(bias_i)^2 + AI_error_sd^2)/denom
    cat("Accuracy:", AI_accuracy, "\n")
    
    Result<- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Advantage AI Niche', lambda = lambda)
    
    filename <- sprintf("chatbot_niche_i%02f_j%02f.RData", i, j)
    filepath <- file.path(out_dir, filename)
    
    save(i, j, bias_i, AI_error_sd, AI_accuracy, Result, file = filepath)
  }
}
