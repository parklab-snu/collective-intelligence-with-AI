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

out_dir <- file.path(project_path, "Simulations", "Figure2,S1,S3,S5_simulation")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#=========================================================================
#Averaging aggregation
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

bias_i <- rep(0.4, m+1)
bias <- sum(bias_i)
AI_error_sd <- 0.3

#Omniscient AI under feedback structure
Result <- Omniscient_AI$main_opt(m, alpha, sigma, N, players, G, AI_error_sd, bias, payoff_type = 'Feedback')

filename <- sprintf("omni_feedback_0.4.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)


#Omnisceint AI under niche-expert structure
Result <- Omniscient_AI$main_opt(m, alpha, sigma, N, players, G, AI_error_sd, bias, payoff_type = 'Niche expert')

filename <- sprintf("omni_niche_0.4.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)


players <- cbind(interest, belief)
#Without AI under feedback structure
Result <- Without_AI$main_opt(m, alpha, sigma, N, players, G, agg_type = 'averaging', payoff_type = 'Feedback')

filename <- sprintf("without_feedback_avg.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)


#Without AI under niche-expert structure
Result <- Without_AI$main_opt(m, alpha, sigma, N, players, G, agg_type = 'averaging', payoff_type = 'Niche expert')

filename <- sprintf("without_niche_avg.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)


#=========================================================================
#Clustering aggregation
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
#AI_belief <- rep(1, N)
#Build player
players <- cbind(interest, belief, AI_belief)

bias_i <- rep(0.4, m+1)
AI_error_sd <- 0.3


#Chatbot AI under feedback structure
Result <- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Feedback')

filename <- sprintf("chatbot_feedback_0.4.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)


#Chatbot AI under niche-expert structure
Result <- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Niche expert')

filename <- sprintf("chatbot_niche_0.4.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)


players <- cbind(interest, belief)

#Without AI under feedback structure
Result <- Without_AI$main_opt(m, alpha, sigma, N, players, G, agg_type = 'clustering', payoff_type = 'Feedback')

filename <- sprintf("without_feedback_clu.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)


#Without AI under niche-expert structure
Result <- Without_AI$main_opt(m, alpha, sigma, N, players, G, agg_type = 'clustering', payoff_type = 'Niche expert')

filename <- sprintf("without_niche_clu.RData")
filepath <- file.path(out_dir, filename)

save(Result, file = filepath)