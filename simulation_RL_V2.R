###################### simulation q learning #############################
# Install and load required libraries
# Packages nécessaires
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(tidyr)

######################## selon l'article rat ############################

# Paramètres d'apprentissage (indépendants pour chaque option)
alpha_g <- rep(0.8, 4) # Taux d'apprentissage pour les gains (pour chaque option)
alpha_l <- rep(0.8, 4) # Taux d'apprentissage pour les pertes (pour chaque option)
lambda_g <- rep(1, 4) # Poids pour les gains (individuel pour chaque option)
lambda_l <- rep(1, 4) # Poids pour les pertes (individuel pour chaque option)
fl <- rep(0.8, 4) # Facteurs d'oubli pertes (spécifique pour chaque option) remplace les alpha pour les options non choisi
fg <- rep(0.8, 4) # Facteurs d'oubli gains (spécifique pour chaque option) remplace les alpha pour les options non choisi

n_choices <- 500 # Nombre total de choix

# Paramètres des options (récompenses)
options <- list(
  option1 = list(
    gain = sample(3:4, n_choices, replace = TRUE),
    loss = sample(-9:-8, n_choices, replace = TRUE),
    jp = 3000, bs = 0, p_jp = 0.01, p_bs = 0
  ), # Antifragile
  option2 = list(
    gain = sample(8:9, n_choices, replace = TRUE),
    loss = sample(-9:-8, n_choices, replace = TRUE),
    jp = 0, bs = 0, p_jp = 0, p_bs = 0
  ), # Robuste
  option3 = list(
    gain = sample(8:9, n_choices, replace = TRUE),
    loss = sample(-3:-4, n_choices, replace = TRUE),
    jp = 0, bs = -3000, p_jp = 0, p_bs = 0.01
  ), # Fragile
  option4 = list(
    gain = sample(3:4, n_choices, replace = TRUE),
    loss = sample(-3:-4, n_choices, replace = TRUE),
    jp = 3000, bs = -3000, p_jp = 0.01, p_bs = 0.01
  ) # Vulnerable
)

#' This function is the actual runner for the simulation with the params
#' provided by other functions that will prepare the parameters to be run by
#' this one
simulation_runner_RL <- function(n_choices, options, params, model_name = "undefined") {
  # Nombre d'options
  n_arms <- length(options)

  # Initialisation
  Q_values <- rep(0, n_arms)
  Q_values_history <- matrix(NA_real_, nrow = n_choices, ncol = n_arms)
  colnames(Q_values_history) <- paste0("Q", seq_len(n_arms))

  choices_history <- integer(n_choices)
  rewards_history <- numeric(n_choices)
  probs_history <- matrix(NA_real_, nrow = n_choices, ncol = n_arms)
  colnames(probs_history) <- paste0("p", seq_len(n_arms))

  # Récupération des paramètres depuis la liste params
  alphas <- params$alphas
  forgets <- params$forgets
  lambdas <- params$lambdas
  rhos <- params$rhos

  # Normaliser les formats: si scalar, étendre à n_arms
  expand_param <- function(x, default = 0) {
    if (is.null(x)) {
      return(rep(default, n_arms))
    }
    if (length(x) == 1) {
      return(rep(unname(x), n_arms))
    }
    if (!is.null(names(x)) && all(grepl("^lambda_?", names(x)))) {
      # ordered lambda_1..lambda_n
      v <- as.numeric(x)
      return(v[1:n_arms])
    }
    return(as.numeric(x)[1:n_arms])
  }

  lambda_vec <- expand_param(lambdas, default = 1)

  # forgets may be named 'forget' or per-arm
  forget_vec <- expand_param(forgets, default = 0)

  # Alphas more complexe: can be a single 'alpha', two (alpha_loss, alpha_gain) or vectors per arm
  # We store separate gain and loss vectors for easy lookup
  if (!is.null(alphas)) {
    if (!is.null(names(alphas)) && "alpha" %in% names(alphas) && length(alphas) == 1) {
      alpha_gain_vec <- alpha_loss_vec <- rep(unname(alphas["alpha"]), n_arms)
    } else if (!is.null(names(alphas)) && all(c("alpha_loss", "alpha_gain") %in% names(alphas)) && length(alphas) == 2) {
      alpha_loss_vec <- rep(unname(alphas["alpha_loss"]), n_arms)
      alpha_gain_vec <- rep(unname(alphas["alpha_gain"]), n_arms)
    } else if (!is.null(names(alphas)) && any(grepl("^alpha_gain", names(alphas)))) {
      # per-arm alpha_gain_1..4 and alpha_loss_1..4
      # fallback to numeric
      alpha_gain_vec <- expand_param(alphas[grepl("gain", names(alphas))], default = 0.1)
      alpha_loss_vec <- expand_param(alphas[grepl("loss", names(alphas))], default = 0.1)
    } else if (length(alphas) == n_arms) {
      # assume same for gain and loss if vector provided
      alpha_gain_vec <- alpha_loss_vec <- as.numeric(alphas)
    } else {
      alpha_gain_vec <- alpha_loss_vec <- rep(as.numeric(alphas[1]), n_arms)
    }
  } else {
    alpha_gain_vec <- alpha_loss_vec <- rep(0.1, n_arms)
  }

  # rhos: named vector with rho_BS and rho_JP optionally
  rho_JP_val <- 0
  rho_BS_val <- 0
  if (!is.null(rhos)) {
    if (!is.null(names(rhos)) && "rho_JP" %in% names(rhos)) rho_JP_val <- as.numeric(rhos["rho_JP"])
    if (!is.null(names(rhos)) && "rho_BS" %in% names(rhos)) rho_BS_val <- as.numeric(rhos["rho_BS"])
  }

  # Simulation loop
  for (t in seq_len(n_choices)) {
    # Compute subjective values
    V_values <- lambda_vec * Q_values

    # Add rhos according to the simulation file option mapping:
    # option1 = Antifragile (JP possible)
    # option2 = Robust
    # option3 = Fragile (BS possible)
    # option4 = Vulnerable (both)
    if (!is.null(rhos)) {
      V_values[1] <- V_values[1] + rho_JP_val
      if (n_arms >= 3) V_values[3] <- V_values[3] + rho_BS_val
      if (n_arms >= 4) V_values[4] <- V_values[4] + rho_BS_val + rho_JP_val
    }

    # Softmax (numerical stability)
    V_max <- max(V_values)
    exp_V <- exp(V_values - V_max)
    probs <- exp_V / sum(exp_V)
    probs <- pmax(probs, 1e-10)
    probs <- probs / sum(probs)

    # Draw choice
    choice <- sample(seq_len(n_arms), size = 1, prob = probs)

    # Draw reward according to option structure
    opt <- options[[choice]]
    u <- runif(1)
    jp_p <- ifelse(is.null(opt$p_jp), 0, opt$p_jp)
    bs_p <- ifelse(is.null(opt$p_bs), 0, opt$p_bs)

    if (u < jp_p) {
      reward <- ifelse(is.null(opt$jp), 0, opt$jp)
    } else if (u < jp_p + bs_p) {
      reward <- ifelse(is.null(opt$bs), 0, opt$bs)
    } else {
      # normal outcome: either gain or loss
      if (runif(1) < 0.5) {
        reward <- opt$gain[t]
      } else {
        reward <- opt$loss[t]
      }
    }

    # Record probabilities, choice and reward
    probs_history[t, ] <- probs
    choices_history[t] <- choice
    rewards_history[t] <- reward

    # Select learning rate
    if (reward >= 0) {
      alpha_used <- alpha_gain_vec[choice]
    } else {
      alpha_used <- alpha_loss_vec[choice]
    }

    # Q update
    prediction_error <- reward - Q_values[choice]
    Q_values[choice] <- Q_values[choice] + alpha_used * prediction_error

    # Forgetting for non-chosen arms
    not_chosen <- setdiff(seq_len(n_arms), choice)
    Q_values[not_chosen] <- Q_values[not_chosen] * (1 - forget_vec[not_chosen])

    # Save Q history (after update)
    Q_values_history[t, ] <- Q_values
  }

  # Convert histories to data.frame for output
  choices_df <- tibble::tibble(
    trial = seq_len(n_choices),
    choice = choices_history,
    reward = rewards_history
  )

  probs_df <- as.data.frame(probs_history)
  probs_df$trial <- seq_len(n_choices)

  Q_history_df <- as.data.frame(Q_values_history)
  Q_history_df$trial <- seq_len(n_choices)

  # Calcul de la proportion cumulée des choix pour chaque option au cours du temps
  proportions_data <- data.frame(
    Iteration = 1:n_choices,
    Antifragile = cumsum(choices_history == 1) / 1:n_choices,
    Robust = cumsum(choices_history == 2) / 1:n_choices,
    Fragil = cumsum(choices_history == 3) / 1:n_choices,
    Vulnerable = cumsum(choices_history == 4) / 1:n_choices
  )

  result <- list(
    model = model_name,
    params = params,
    choices = choices_df,
    probs = probs_df,
    Q_history = Q_history_df,
    proportions_data = proportions_data
  )

  return(result)
}

simulation_homogeneous_RL <- function(n_choices, options, alpha, forget, lambda) {
  # Preparing the param list for the simulation runner
  params <- list(
    alphas = c("alpha" = alpha),
    forgets = c("forget" = forget),
    lambdas = c("lambda" = lambda)
  )

  results <- simulation_runner_RL(n_choices = n_choices, options = options, params = params, model_name = "HOMOGENEOUS")
  return(results)
}

simulation_gain_loss_RL <- function(n_choices, options, alpha_loss, alpha_gain, forget, lambda) {
  params <- list(
    alphas = c("alpha_loss" = alpha_loss, "alpha_gain" = alpha_gain),
    forgets = c("forget" = forget),
    lambdas = c("lambda" = lambda)
  )
  results <- simulation_runner_RL(n_choices = n_choices, options = options, params = params, model_name = "GAIN_LOSS")
  return(results)
}

simulation_biased_RL <- function(n_choices, options, alpha_loss, alpha_gain, forgets_vec, lambdas_vec) {
  params <- list(
    alphas = c("alpha_loss" = alpha_loss, "alpha_gain" = alpha_gain),
    forgets = lambdas_vec, # here user may pass full vector as forgets_vec
    lambdas = lambdas_vec
  )
  results <- simulation_runner_RL(n_choices = n_choices, options = options, params = params, model_name = "BIASED")
  return(results)
}

simulation_ree_biased_simple_RL <- function(
    n_choices,
    options,
    alpha_l, alpha_g,
    rho_BS, rho_JP,
    forget, lambda) {
  # Preparing the param list for the simulation runner
  params <- list(
    alphas = c("alpha_loss" = alpha_l, "alpha_gain" = alpha_g),
    forgets = c("forget" = forget),
    lambdas = c("lambda" = lambda),
    rhos = c("rho_BS" = rho_BS, "rho_JP" = rho_JP)
  )

  results <- simulation_runner_RL(n_choices = n_choices, options = options, params = params, model_name = "REE_BIASED_SIMPLE")
  return(results)
}

simulation_ree_learning_simple_RL <- function(n_choices, options, alpha1, alpha2, alpha3, alpha4, forget, lambda) {
  params <- list(
    alphas = c(alpha1, alpha2, alpha3, alpha4),
    forgets = c("forget" = forget),
    lambdas = c("lambda" = lambda)
  )
  results <- simulation_runner_RL(n_choices = n_choices, options = options, params = params, model_name = "REE_LEARNING_SIMPLE")
  return(results)
}

simulation_ree_learning_biased_simple_RL <- function(n_choices, options, alpha1, alpha2, alpha3, alpha4, forget, lambda, rho_BS, rho_JP) {
  params <- list(
    alphas = c(alpha1, alpha2, alpha3, alpha4),
    forgets = c("forget" = forget),
    lambdas = c("lambda" = lambda),
    rhos = c("rho_BS" = rho_BS, "rho_JP" = rho_JP)
  )
  results <- simulation_runner_RL(n_choices = n_choices, options = options, params = params, model_name = "REE_LEARNING_BIASED_SIMPLE")
  return(results)
}

simulation_agentRL <- function(alpha_g, alpha_l, lambda_g, lambda_l, fg, fl, n_choices, options) {
  # Initialisation des Q-values pour chaque option (gains et pertes séparés)
  Q1_gain <- 0
  Q2_gain <- 0
  Q3_gain <- 0
  Q4_gain <- 0
  Q1_loss <- 0
  Q2_loss <- 0
  Q3_loss <- 0
  Q4_loss <- 0

  # Historique des choix, outcome et Q value
  choices_history <- integer(n_choices)
  rewards_history <- numeric(n_choices)
  Q1_gain_history <- numeric(n_choices)
  Q2_gain_history <- numeric(n_choices)
  Q3_gain_history <- numeric(n_choices)
  Q4_gain_history <- numeric(n_choices)
  Q1_loss_history <- numeric(n_choices)
  Q2_loss_history <- numeric(n_choices)
  Q3_loss_history <- numeric(n_choices)
  Q4_loss_history <- numeric(n_choices)

  # Simulation du processus d'apprentissage
  for (t in 1:n_choices) {
    # Calcul des valeurs V pour chaque option
    V1 <- lambda_g[1] * Q1_gain + lambda_l[1] * Q1_loss
    V2 <- lambda_g[2] * Q2_gain + lambda_l[2] * Q2_loss
    V3 <- lambda_g[3] * Q3_gain + lambda_l[3] * Q3_loss
    V4 <- lambda_g[4] * Q4_gain + lambda_l[4] * Q4_loss
    print(c(V1, V2, V3, V4))

    # Calcul des valeurs exponentielles de chaque option
    exp_V1 <- exp(V1)
    if (is.infinite(exp_V1)) {
      exp_V1 <- .Machine$double.xmax
    }
    exp_V2 <- exp(V2)
    if (is.infinite(exp_V2)) {
      exp_V2 <- .Machine$double.xmax
    }
    exp_V3 <- exp(V3)
    if (is.infinite(exp_V3)) {
      exp_V3 <- .Machine$double.xmax
    }
    exp_V4 <- exp(V4)
    if (is.infinite(exp_V4)) {
      exp_V4 <- .Machine$double.xmax
    }

    # Somme des valeurs exponentielles
    sum_exp_V <- exp_V1 + exp_V2 + exp_V3 + exp_V4

    # Probabilités pour chaque option
    p1 <- exp_V1 / sum_exp_V
    p2 <- exp_V2 / sum_exp_V
    p3 <- exp_V3 / sum_exp_V
    p4 <- exp_V4 / sum_exp_V

    # Création du vecteur de probabilités
    probabilities <- c(p1, p2, p3, p4)
    print(probabilities)

    # Choix d'une option en fonction des probabilités / ici c'est là ou je pourrais ajouter une boucle if avec epsilon greedy
    choice <- sample(1:4, 1, prob = probabilities)
    choices_history[t] <- choice
    # enregistre les Q value
    Q1_gain_history[t] <- Q1_gain
    Q2_gain_history[t] <- Q2_gain
    Q3_gain_history[t] <- Q3_gain
    Q4_gain_history[t] <- Q4_gain
    Q1_loss_history[t] <- Q1_loss
    Q2_loss_history[t] <- Q2_loss
    Q3_loss_history[t] <- Q3_loss
    Q4_loss_history[t] <- Q4_loss

    # Sélection de l'option choisie et calcul de la récompense
    selected_option <- options[[paste0("option", choice)]] # ou juste choice normalement ça devrait marcher et me prendre l'indice correspondant

    reward <- if (runif(1) < selected_option$p_jp) {
      selected_option$jp # Gain extrême (JP)
    } else if (runif(1) < selected_option$p_bs) {
      selected_option$bs # Perte extrême (BS)
    } else if (runif(1) < 0.5) {
      selected_option$gain[t] # Gain normal
    } else {
      selected_option$loss[t] # Perte normale
    }
    rewards_history[t] <- reward

    # Mise à jour des Q-values pour l'option choisie
    if (choice == 1) {
      if (reward > 0) {
        Q1_gain <- Q1_gain + alpha_g[1] * (reward - Q1_gain)
      } else {
        Q1_loss <- Q1_loss + alpha_l[1] * (reward - Q1_loss)
      }
    } else if (choice == 2) {
      if (reward > 0) {
        Q2_gain <- Q2_gain + alpha_g[2] * (reward - Q2_gain)
      } else {
        Q2_loss <- Q2_loss + alpha_l[2] * (reward - Q2_loss)
      }
    } else if (choice == 3) {
      if (reward > 0) {
        Q3_gain <- Q3_gain + alpha_g[3] * (reward - Q3_gain)
      } else {
        Q3_loss <- Q3_loss + alpha_l[3] * (reward - Q3_loss)
      }
    } else if (choice == 4) {
      if (reward > 0) {
        Q4_gain <- Q4_gain + alpha_g[4] * (reward - Q4_gain)
      } else {
        Q4_loss <- Q4_loss + alpha_l[4] * (reward - Q4_loss)
      }
    }

    # Mise à jour des Q-values pour les options non choisies avec facteur d'oubli
    if (choice != 1) {
      Q1_gain <- Q1_gain * (1 - fg[1])
      Q1_loss <- Q1_loss * (1 - fl[1])
    }
    if (choice != 2) {
      Q2_gain <- Q2_gain * (1 - fg[2])
      Q2_loss <- Q2_loss * (1 - fl[2])
    }
    if (choice != 3) {
      Q3_gain <- Q3_gain * (1 - fg[3])
      Q3_loss <- Q3_loss * (1 - fl[3])
    }
    if (choice != 4) {
      Q4_gain <- Q4_gain * (1 - fg[4])
      Q4_loss <- Q4_loss * (1 - fl[4])
    }
  }

  # Calcul de la proportion cumulée des choix pour chaque option au cours du temps
  proportions_data <- data.frame(
    Iteration = 1:n_choices,
    Antifragile = cumsum(choices_history == 1) / 1:n_choices,
    Robust = cumsum(choices_history == 2) / 1:n_choices,
    Fragil = cumsum(choices_history == 3) / 1:n_choices,
    Vulnerable = cumsum(choices_history == 4) / 1:n_choices
  )
  result <- list(
    proportions_data = proportions_data, rewards_history = rewards_history, choices_history = choices_history,
    Q1_gain_history = Q1_gain_history, Q2_gain_history = Q2_gain_history, Q3_gain_history = Q3_gain_history, Q4_gain_history = Q4_gain_history,
    Q1_loss_history = Q1_loss_history, Q2_loss_history = Q2_loss_history, Q3_loss_history = Q3_loss_history, Q4_loss_history = Q4_loss_history
  )

  return(result)
}

compute_TSREE <- function(proportions_data) {
  TSREE <- 1 + proportions_data$Antifragile - proportions_data$Fragil
  return(TSREE)
}

compute_OSSREE <- function(proportions_data) {
  OSSREE <- proportions_data$Vulnerable - proportions_data$Robust
  return(OSSREE)
}

plot_TSREE_OSSREE <- function(proportions_data) {
  OSSREE <- compute_OSSREE(proportions_data)
  TSREE <- compute_TSREE(proportions_data)
  plot(OSSREE, TSREE,
    col = "darkblue", cex = 2, xlim = c(-1, 1), ylim = c(0, 2), type = "l",
    xlab = "OSSREE", ylab = "TSREE", main = "Evolution of TSREE and OSSREE over trials"
  )
  lines(c(0, 1, 0, -1, 0), c(0, 1, 2, 1, 0))
  lines(c(0, 0), c(0, 2), lty = 2)
  lines(c(-1, 1), c(1, 1), lty = 2)
}

plot_mean_TSREE_OSSREE_one_agent <- function(proportions_data) {
  OSSREE <- compute_OSSREE(proportions_data)
  TSREE <- compute_TSREE(proportions_data)
  mean_OSSREE <- mean(OSSREE)
  mean_TSREE <- mean(TSREE)
  plot(mean_OSSREE, mean_TSREE,
    col = "purple", cex = 2, pch = "+", xlim = c(-1, 1), ylim = c(0, 2), type = "p",
    xlab = "OSSREE", ylab = "TSREE", main = "Mean TSREE and OSSREE over trials"
  )
  lines(c(0, 1, 0, -1, 0), c(0, 1, 2, 1, 0))
  lines(c(0, 0), c(0, 2), lty = 2)
  lines(c(-1, 1), c(1, 1), lty = 2)
}

plot_choices_proportions <- function(proportions_data) {
  # Conversion des données pour ggplot
  proportions_long <- reshape2::melt(proportions_data,
    id.vars = "Iteration",
    variable.name = "Option", value.name = "Proportion"
  )

  # Tracé du graphique
  ggplot(proportions_long, aes(x = Iteration, y = Proportion, color = Option)) +
    geom_line(size = 1.2) +
    labs(
      title = "Proportion of simulated choices through trials",
      x = "trials",
      y = "Proportion of choices"
    ) +
    scale_color_manual(values = c(
      "Antifragile" = "blue", "Robust" = "red",
      "Fragil" = "green", "Vulnerable" = "purple"
    )) +
    theme_minimal() +
    theme(legend.title = element_blank())
}

#### pour un agent RL
result <- simulation_ree_learning_biased_simple_RL(
  n_choices = n_choices,
  options = options, alpha1 = 0.5, alpha2 = 0.5, alpha3 = 0.5, alpha4 = 0.5, forget = 0.1, lambda = 1, rho_BS = 0, rho_JP = 0
)

proportions_data <- result$proportions_data
plot_TSREE_OSSREE(proportions_data)
plot_mean_TSREE_OSSREE_one_agent(proportions_data)
plot_choices_proportions(proportions_data)


### pour plusieurs agents RL

n_agent <- 1000

result_multi_agent <- lapply(1:n_agent, function(i) {
  n_choices <- 400 # Nombre total de choix
  # Paramètres des options (récompenses)
  options <- list(
    option1 = list(
      gain = sample(3:4, n_choices, replace = TRUE),
      loss = sample(-9:-8, n_choices, replace = TRUE),
      jp = 3000, bs = 0, p_jp = 0.05, p_bs = 0
    ),
    option2 = list(
      gain = sample(8:9, n_choices, replace = TRUE),
      loss = sample(-9:-8, n_choices, replace = TRUE),
      jp = 0, bs = 0, p_jp = 0, p_bs = 0
    ),
    option3 = list(
      gain = sample(8:9, n_choices, replace = TRUE),
      loss = sample(-3:-4, n_choices, replace = TRUE),
      jp = 0, bs = -3000, p_jp = 0, p_bs = 0.05
    ),
    option4 = list(
      gain = sample(3:4, n_choices, replace = TRUE),
      loss = sample(-3:-4, n_choices, replace = TRUE),
      jp = 3000, bs = -3000, p_jp = 0.05, p_bs = 0.05
    )
  )
  result <- simulation_ree_learning_biased_simple_RL(
    n_choices = n_choices,
    options = options,
    alpha1 = 0.5, alpha2 = 0.5, alpha3 = 0.5, alpha4 = 0.5,
    forget = 0.2, lambda = 2, rho_BS = -1, rho_JP = 1
  )

  return(result)
})

proportions_data_multi_agent <- do.call("rbind", lapply(seq_along(result_multi_agent), function(i) {
  # Ici on récupère les données de la simu i
  current_proportions_data <- result_multi_agent[[i]]$proportions_data
  current_proportions_data$repetition <- i
  current_proportions_data
}))

## plot des comportements moyens TSREE OSREE
TSREE <- rep(0, length(unique(proportions_data_multi_agent$repetition))) # axe des y
OSSREE <- rep(0, length(unique(proportions_data_multi_agent$repetition))) # axe des x
for (i in unique(proportions_data_multi_agent$repetition)) {
  OSSREE[i] <- mean(proportions_data_multi_agent[proportions_data_multi_agent$repetition == i, ]$Vulnerable) - mean(proportions_data_multi_agent[proportions_data_multi_agent$repetition == i, ]$Robust) # f vulnérable - f robuste
  TSREE[i] <- 1 + mean(proportions_data_multi_agent[proportions_data_multi_agent$repetition == i, ]$Antifragile) - mean(proportions_data_multi_agent[proportions_data_multi_agent$repetition == i, ]$Fragil) # 1 + f antifragile - f fragile
}

plot(OSSREE, TSREE, col = "darkblue", pch = "+", cex = 2, xlim = c(-1, 1), ylim = c(0, 2))
lines(c(0, 1, 0, -1, 0), c(0, 1, 2, 1, 0))
lines(c(0, 0), c(0, 2), lty = 2)
lines(c(-1, 1), c(1, 1), lty = 2)

# llabels=seq(1,length(OSSREE))
# for (i in 1:length(OSSREE)){text(OSSREE[i],TSREE[i]-.1,llabels[i])}

## plot des proportions en fonctions des trials
summarised_proportions_data <- proportions_data_multi_agent %>%
  group_by(Iteration) %>%
  summarise_at(vars(Antifragile:Vulnerable), list(mean = mean, sd = sd)) %>%
  pivot_longer(cols = Antifragile_mean:Vulnerable_sd, names_to = c("Option", ".value"), names_sep = "_") %>%
  mutate(CI_Upper = mean + 1.96 * sd / sqrt(n_agent), CI_Lower = mean - 1.96 * sd / sqrt(n_agent))

ggplot(summarised_proportions_data, aes(x = Iteration, y = mean, color = Option)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper, fill = Option), alpha = 0.2) +
  labs(
    title = "Simulation of choices through trials",
    x = "trials", y = "proportion of choices"
  ) +
  # xlim(0,50)+
  theme_minimal()
