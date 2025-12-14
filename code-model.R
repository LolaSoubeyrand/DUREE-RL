########## this code is the code for the model fithing prediction and robustness ##########
# lola soubeyrand 

# importation des données 
#base <- read.csv("~/Desktop/tasks/pygame-python/Montpellier/description-based/base.csv")
#data_fourchoices <- base
#data_fourchoices$button_name <- as.factor(data_fourchoices$button_name)
#data_fourchoices$id <- as.numeric(data_fourchoices$id)

# Charger les packages nécessaires
library(stats4)

# Fonction de mise à jour des Q-values en utilisant directement la récompense
update_q_value <- function(Q, reward, alpha) {
  Q <- Q + alpha * (reward - Q)
  return(Q)
}

# Fonction de calcul de la log-vraisemblance pour le modèle de base
base_model_log_likelihood <- function(params, choices, rewards) {
  # Extraire les paramètres du modèle
  alpha_g <- params[1]
  alpha_l <- params[2]
  lambda_g <- params[3]
  lambda_l <- params[4]
  
  # Initialiser les Q-values pour chaque option
  Q_values <- list(
    "Antifragile" = 0,
    "Fragile" = 0,
    "Robust" = 0,
    "Vulnerable" = 0
  )
  
  log_likelihood <- 0
  
  for (t in 1:length(choices)) {
    choice <- choices[t]
    reward <- rewards[t]
    
    # Déterminer l'alpha en fonction du signe de la récompense
    alpha <- ifelse(reward > 0, alpha_g, alpha_l)
    
    # Mettre à jour la Q-value pour l'option choisie
    Q_values[[choice]] <- update_q_value(Q_values[[choice]], reward, alpha)
    
    # Calculer les valeurs pour chaque option pour le softmax
    Q_vec <- unlist(Q_values)
    Vt <- lambda_g * Q_vec * (reward > 0) + lambda_l * Q_vec * (reward < 0)
    
    # Prévenir les overflow dans exp() en centrant Vt (softmax stable)
    Vt <- Vt - max(Vt)
    prob <- exp(Vt) / sum(exp(Vt))
    
    # Vérifier que prob[choices[t]] n'est pas nul ou NA
    if (is.na(prob[choice]) || prob[choice] <= 0) {
      return(Inf)  # Retourne une vraisemblance très grande en cas d'échec
    }
    
    # Ajouter la log-probabilité de l'action choisie
    log_likelihood <- log_likelihood + log(prob[choice])
  }
  
  # Retourne la vraisemblance négative pour l'optimisation
  return(-log_likelihood)
}


# Fonction de recherche par grille
grid_search <- function(choices, rewards) {
  # Grille de paramètres
  grid <- expand.grid(
    alpha_g = seq(0.1, 1, length.out = 6),
    alpha_l = seq(0.1, 1, length.out = 6),
    lambda_g = seq(2, 5, length.out = 6),
    lambda_l = seq(2, 5, length.out = 6)
  )
  
  best_params <- NULL
  best_log_likelihood <- Inf
  
  # Boucle de recherche par grille
  for (i in 1:nrow(grid)) {
    params <- as.numeric(grid[i, ])  # Convertir en vecteur numérique pour la fonction
    ll <- base_model_log_likelihood(params, choices, rewards)
    
    # Vérifier que ll n'est pas NA
    if (!is.na(ll) && ll < best_log_likelihood) {
      best_log_likelihood <- ll
      best_params <- params
    }
  }
  
  return(c(grid, best_params))
}

# Estimation du modèle de base pour chaque session
optimize_base_model <- function(choices, rewards) {
  # Étape 1 : Recherche par grille
  init_params <- grid_search(choices, rewards)
  
  # Étape 2 : Optimisation quasi-Newton avec L-BFGS-B pour les bornes
  fit_quasi_newton <- optim(par = init_params,
                            fn = function(par) base_model_log_likelihood(par, choices, rewards),
                            method = "L-BFGS-B",
                            lower = c(0.1, 0.1, 2, 2), # Limite inférieure
                            upper = c(1, 1, 5, 5))              # Limite supérieure
  
  
  return(fit_quasi_newton)  # Retourner les paramètres optimisés par le simplex
}


# Exemple d'application du modèle de base pour un participant
choices <- data_fourchoices_descr[data_fourchoices_descr$id == 1,]$button_name
rewards <- data_fourchoices_descr[data_fourchoices_descr$id == 1,]$button_value

# Estimation pour le participant
params <- optimize_base_model(choices, rewards)
print(params)




# Définition de la fonction de vraisemblance pour le modèle étendu
extended_model_log_likelihood <- function(alpha_g, alpha_l, lambda_g, lambda_l, gamma_g, gamma_l, forgetting_g, forgetting_l, choices, rewards) {
  Qg <- 0
  Ql <- 0
  log_likelihood <- 0
  
  for (t in 1:length(choices)) {
    gain <- rewards[[t]]$gain
    loss <- rewards[[t]]$loss
    rare_gain <- rewards[[t]]$JP
    rare_loss <- rewards[[t]]$BS
    
    # Application des paramètres d'oubli pour les gains et pertes
    Qg <- forgetting_g * Qg + alpha_g * (gain - Qg)
    Ql <- forgetting_l * Ql + alpha_l * (loss - Ql)
    
    # Calcul de la valeur avec les poids rares ajoutés
    Vt <- lambda_g * Qg + lambda_l * Ql + gamma_g * rare_gain + gamma_l * rare_loss
    prob <- exp(Vt) / sum(exp(Vt))
    log_likelihood <- log_likelihood + log(prob[choices[t]])
  }
  
  return(-log_likelihood)
}

# Estimation du modèle étendu avec AIC et BIC
optimize_extended_model <- function(choices, rewards, base_params) {
  fit_extended <- optim(par = c(base_params, gamma_g = 1, gamma_l = 1, forgetting_g = 0.9, forgetting_l = 0.9),
                        fn = function(par) extended_model_log_likelihood(par[1], par[2], par[3], par[4], par[5], par[6], par[7], par[8], choices, rewards),
                        method = "BFGS")
  
  # Calcul de l'AIC et BIC
  k <- length(fit_extended$par)  # Nombre de paramètres
  n <- length(choices)  # Nombre d'observations
  log_likelihood <- -fit_extended$value
  AIC <- -2 * log_likelihood + 2 * k
  BIC <- -2 * log_likelihood + log(n) * k
  
  return(list(params = fit_extended$par, AIC = AIC, BIC = BIC))
}

# Exemple d'application pour un participant avec 2 sessions
optimize_for_participant <- function(session1_choices, session1_rewards, session2_choices, session2_rewards) {
  # Modèle de base pour chaque session
  params_session1 <- optimize_base_model(session1_choices, session1_rewards)
  params_session2 <- optimize_base_model(session2_choices, session2_rewards)
  
  # Modèle étendu pour chaque session
  extended_session1 <- optimize_extended_model(session1_choices, session1_rewards, params_session1)
  extended_session2 <- optimize_extended_model(session2_choices, session2_rewards, params_session2)
  
  # Comparaison des AIC et BIC pour chaque session
  final_model_session1 <- if (extended_session1$AIC < extended_session1$BIC) extended_session1 else params_session1
  final_model_session2 <- if (extended_session2$AIC < extended_session2$BIC) extended_session2 else params_session2
  
  return(list(session1 = final_model_session1, session2 = final_model_session2))
}


###################################################################################################################

# --- Fonctions utilitaires de transformation pour contraintes ----
inv_logit <- function(x) 1 / (1 + exp(-x))
logit <- function(p) log(p / (1 - p))
# lambdas positive: use exp transform
exp_tr <- function(x) exp(x)
log_tr <- function(x) log(x)

# --- forward: calcule probas et loglike pour un jeu de paramètres donné ---
# data: data.frame with columns 'choice' (1..4) and 'reward' (numeric)
# model_type: "simple" or "time"
# params_raw: vector of raw parameters (unconstrained) to be transformés selon modèle
# Returns : negative log-likelihood
neg_loglike_rl <- function(params_raw, data, model_type = "simple", n_options = 4, debug = FALSE){
  Tt <- nrow(data)
  
  # helper to build time-scaling
  t_scaled <- function(t) if(Tt>1) (t-1)/(Tt-1) else 0
  
  # transform raw params depending on model
  p <- list()
  idx <- 1
  
  if(model_type == "simple"){
    # params order:
    # alpha_g_raw, alpha_l_raw, lambda_g_raw, lambda_l_raw, f_g_raw, f_l_raw,
    # beta_JP, beta_BS, beta_softmax_raw
    alpha_g <- inv_logit(params_raw[idx]); idx <- idx+1
    alpha_l <- inv_logit(params_raw[idx]); idx <- idx+1
    lambda_g <- exp_tr(params_raw[idx]); idx <- idx+1
    lambda_l <- exp_tr(params_raw[idx]); idx <- idx+1
    f_g <- inv_logit(params_raw[idx]); idx <- idx+1
    f_l <- inv_logit(params_raw[idx]); idx <- idx+1
    beta_JP <- params_raw[idx]; idx <- idx+1
    beta_BS <- params_raw[idx]; idx <- idx+1
    beta_softmax <- exp_tr(params_raw[idx]); idx <- idx+1  # positive inverse temperature
    
    # replicate per option
    alpha_g_v <- rep(alpha_g, n_options)
    alpha_l_v <- rep(alpha_l, n_options)
    lambda_g_v <- rep(lambda_g, n_options)
    lambda_l_v <- rep(lambda_l, n_options)
    f_g_v <- rep(f_g, n_options)
    f_l_v <- rep(f_l, n_options)
    
    # time functions return constants
    alpha_g_t <- function(t) alpha_g_v
    alpha_l_t <- function(t) alpha_l_v
    lambda_g_t <- function(t) lambda_g_v
    lambda_l_t <- function(t) lambda_l_v
    f_g_t <- function(t) f_g_v
    f_l_t <- function(t) f_l_v
    
  } else if(model_type == "time"){
    # For each grouped parameter we have intercept + slope (raw)
    # order: alpha_g_int, alpha_g_slope, alpha_l_int, alpha_l_slope,
    # lambda_g_int, lambda_g_slope, lambda_l_int, lambda_l_slope,
    # f_g_int, f_g_slope, f_l_int, f_l_slope,
    # beta_JP, beta_BS, beta_softmax_raw
    alpha_g_int <- inv_logit(params_raw[idx]); idx<-idx+1
    alpha_g_sl  <- params_raw[idx]; idx<-idx+1
    alpha_l_int <- inv_logit(params_raw[idx]); idx<-idx+1
    alpha_l_sl  <- params_raw[idx]; idx<-idx+1
    
    lambda_g_int <- exp_tr(params_raw[idx]); idx<-idx+1
    lambda_g_sl  <- params_raw[idx]; idx<-idx+1
    lambda_l_int <- exp_tr(params_raw[idx]); idx<-idx+1
    lambda_l_sl  <- params_raw[idx]; idx<-idx+1
    
    f_g_int <- inv_logit(params_raw[idx]); idx<-idx+1
    f_g_sl  <- params_raw[idx]; idx<-idx+1
    f_l_int <- inv_logit(params_raw[idx]); idx<-idx+1
    f_l_sl  <- params_raw[idx]; idx<-idx+1
    
    beta_JP <- params_raw[idx]; idx<-idx+1
    beta_BS <- params_raw[idx]; idx<-idx+1
    beta_softmax <- exp_tr(params_raw[idx]); idx<-idx+1
    
    # time-varying functions (shared across options)
    alpha_g_t <- function(t){
      p <- inv_logit( logit(alpha_g_int) + alpha_g_sl * t_scaled(t) )
      rep(p, n_options)
    }
    alpha_l_t <- function(t){
      p <- inv_logit( logit(alpha_l_int) + alpha_l_sl * t_scaled(t) )
      rep(p, n_options)
    }
    lambda_g_t <- function(t){
      p <- exp_tr( log(lambda_g_int) + lambda_g_sl * t_scaled(t) )
      rep(p, n_options)
    }
    lambda_l_t <- function(t){
      p <- exp_tr( log(lambda_l_int) + lambda_l_sl * t_scaled(t) )
      rep(p, n_options)
    }
    f_g_t <- function(t){
      p <- inv_logit( logit(f_g_int) + f_g_sl * t_scaled(t) )
      rep(p, n_options)
    }
    f_l_t <- function(t){
      p <- inv_logit( logit(f_l_int) + f_l_sl * t_scaled(t) )
      rep(p, n_options)
    }
    
  } else {
    stop("model_type must be 'simple' or 'time'")
  }
  
  # INITIALISE Q-values and extreme flags
  Q_gain <- rep(0, n_options)
  Q_loss <- rep(0, n_options)
  extreme_flag <- rep(0, n_options) # +1 for JP, -1 for BS, 0 else
  
  loglike <- 0
  tiny <- 1e-12
  
  for(t in 1:nrow(data)){
    # compute Vs for each option at time t
    lamg <- lambda_g_t(t)
    laml <- lambda_l_t(t)
    V <- lamg * Q_gain + laml * Q_loss + (beta_JP * (extreme_flag==1)) + (beta_BS * (extreme_flag==-1))
    # apply softmax scaled by beta_softmax (inverse temperature)
    expV <- exp(beta_softmax * V - max(beta_softmax * V)) # numeric stable
    probs <- expV / sum(expV)
    chosen <- data$choice[t]
    # add log-prob
    p_chosen <- probs[chosen]
    loglike <- loglike + log(p_chosen + tiny)
    
    # observe reward and update Q for the chosen option
    r <- data$reward[t]
    if(r > 0){
      # gain update
      ag <- alpha_g_t(t)[chosen]
      Q_gain[chosen] <- Q_gain[chosen] + ag * (r - Q_gain[chosen])
    } else {
      # loss update
      al <- alpha_l_t(t)[chosen]
      Q_loss[chosen] <- Q_loss[chosen] + al * (r - Q_loss[chosen])
    }
    
    # update extreme_flag only for the chosen option if extreme happened
    if(r == 3000) extreme_flag[chosen] <- 1
    else if(r == -3000) extreme_flag[chosen] <- -1
    # else leave it as is (design choice; can set to 0 to make effect transient)
    
    # forgetting for non-chosen options
    fg <- f_g_t(t)
    fl <- f_l_t(t)
    not_chosen <- setdiff(1:n_options, chosen)
    Q_gain[not_chosen] <- Q_gain[not_chosen] * (1 - fg[not_chosen])
    Q_loss[not_chosen] <- Q_loss[not_chosen] * (1 - fl[not_chosen])
  }
  
  if(debug){
    return(list(negloglike = -loglike, loglike = loglike))
  } else {
    return(-loglike)
  }
}

# --- Wrapper d'estimation: plusieurs départs et diagnostics ---
fit_model_rl <- function(data, model_type = "simple", n_starts = 10, maxit = 2000, verbose = TRUE){
  best <- list(value = Inf)
  best_fit <- NULL
  
  for(s in 1:n_starts){
    # initial random raw params
    if(model_type == "simple"){
      # 9 raw params
      init <- c(rnorm(1,0,1), rnorm(1,0,1), rnorm(1,0.1), rnorm(1,0.1),
                rnorm(1,0,1), rnorm(1,0,1), rnorm(1,0.5), rnorm(1,0.5), rnorm(1,0.1))
    } else if(model_type == "time"){
      init <- c(rnorm(12,0,1), rnorm(2,0.5), rnorm(1,0.1)) # approximate length fits transform code
      # ensure length matches expected: actually time model expects 13 raw before beta_softmax -> safe to use variable
    } else stop("unknown model_type")
    
    opt <- try(optim(par = init,
                     fn = function(par) neg_loglike_rl(par, data, model_type = model_type),
                     method = "BFGS",
                     control = list(maxit = maxit),
                     hessian = FALSE),
               silent = TRUE)
    if(inherits(opt, "try-error")) next
    if(opt$value < best$value){
      best <- opt
      best_fit$par <- opt$par
      best_fit$value <- opt$value
      best_fit$convergence <- opt$convergence
      best_fit$counts <- opt$counts
      best_fit$optim <- opt
    }
    if(verbose) cat(sprintf("start %d -> nll=%.3f conv=%d\n", s, opt$value, opt$convergence))
  }
  
  # compute final hessian around best param (if BFGS available we could use numDeriv)
  if(!is.null(best_fit)){
    # refine with hessian
    library(numDeriv)
    h <- try(hessian(func = function(par) neg_loglike_rl(par, data, model_type = model_type, debug=FALSE),
                     x = best_fit$par), silent = TRUE)
    best_fit$hessian <- if(inherits(h, "try-error")) NULL else h
    # compute AIC, BIC
    k <- length(best_fit$par)
    nll <- best$value
    best_fit$AIC <- 2*k + 2*nll
    best_fit$BIC <- log(nrow(data))*k + 2*nll
  }
  return(best_fit)
}

# ------------------------------
# Exemple d'utilisation
# ------------------------------
# Simule une série de données "observées" (exemple) pour tester la fonction :
set.seed(42)
# Exemple minimal: 200 essais avec choix aléatoire et rewards mélangeés (à remplacer par tes données)
T <- 200
data_sim <- data.frame(choice = sample(1:4, T, replace = TRUE),
                       reward = sample(c(-3000, -50, 0, 50, 3000), T, replace = TRUE, prob = c(0.02,0.24,0.48,0.24,0.02)))

# Ajuste le modèle simple :
fit_simple <- fit_model_rl(data_sim, model_type = "simple", n_starts = 8, verbose = TRUE)
print(fit_simple$value)
print(fit_simple$convergence)
if(!is.null(fit_simple$hessian)) print(eigen(fit_simple$hessian)$values)

# Ajuste le modèle time-varying (plus long)
# fit_time <- fit_model_rl(data_sim, model_type = "time", n_starts = 6, verbose = TRUE)
# print(fit_time)

# ------------------------------
# Notes:
# - Remplace data_sim par ton data.frame réel (colonnes 'choice', 'reward')
# - Pour rendre JP/BS transitoire (seulement sur l'essai suivant), dans le loop remplace :
#     if(r == 3000) extreme_flag[chosen] <- 1 else if(r == -3000) extreme_flag[chosen] <- -1
#   par un mécanisme qui remet extreme_flag[chosen] <- 0 à la fin de la prochaine itération.
# - Tu peux augmenter n_starts et maxit pour rechercher un meilleur optimum (mais ça coûte du temps).
# - Pour comparer modèles emboîtés utilise AIC/BIC (déjà calculés).

