project_path <- "C:/Users/glaucous_winged_gull/Desktop/2026_Park_lab/Collective-intelligence-with-AI"

source(file.path(
  project_path,
  "Chatbot_AI",
  "Functions_Chatbot_AI.R"
))

Result <- main_opt(
  m = m,
  alpha = alpha,
  sigma = sigma,
  N = N,
  players = players,
  G = G,
  alpha_AI = alpha_AI,
  bias = bias_i,
  AI_error_sd = AI_error_sd,
  agg_type = "clustering",
  payoff_type = "Balanced",
  lambda = 0,
  mu = 0,
  w = 0.5
)