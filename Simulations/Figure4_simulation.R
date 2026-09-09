project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"

Chatbot_AI <- new.env()
  
source(
  file.path(project_path, "Functions", "Chatbot_AI.R"),
  local = Chatbot_AI
)

out_dir <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI/Simulations/Figure4_simulation"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

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
chatbot_feedback <- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Feedback')

filename <- sprintf("chatbot_feedback_0.4.RData")
filepath <- file.path(out_dir, filename)

save(chatbot_feedback, file = filepath)


#Chatbot AI under niche-expert structure
chatbot_niche <- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Niche expert')

filename <- sprintf("chatbot_niche_0.4.RData")
filepath <- file.path(out_dir, filename)

save(chatbot_niche, file = filepath)

#Chatbot AI under balanced structure
chatbot_balanced <- Chatbot_AI$main_opt(m, alpha, sigma, N, players, G, bias_i, AI_error_sd, agg_type = 'clustering', payoff_type = 'Balanced')

filename <- sprintf("chatbot_balanced_0.4.RData")
filepath <- file.path(out_dir, filename)

save(chatbot_balanced, file = filepath)
