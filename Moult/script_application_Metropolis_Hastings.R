rm(list = ls()) # nettoyage de l'environnement de travail

source("utils_packages.R")
set.seed(433) # Pour la reproductibilité
source("script_dataset_simule.R")

data_exo = subset(data_simul, Annee==1) %>% 
  dplyr::select(Moult_score, Date)


f = function(esp, y){
  abs((1/esp**2)*(y^2*(1-y)-esp^2*y))
}

g = function(esp, y){
  abs((1/esp**2)*(y*(1-y)^2+esp^2*(y-1)))
}


  
get_likelihood  = function(data_=data_exo, T_, tau, esp=0.02){
    
    data_0 = subset(data_, Moult_score == 0)
    data_1 = subset(data_, Moult_score == 1)
    data_01 = subset(data_, Moult_score > 0 & Moult_score < 1)
    
    support = 1
    if (nrow(data_0)>0){
      support = support * (T_ > max(data_0$Date)) * (T_ < min(data_01$Date))
    }
    if (nrow(data_01)>0){
      support = support * (tau > max(data_01$Date)-min(data_01$Date))
    }
    if (nrow(data_1)>0 & nrow(data_0)>0){
      support = support * (tau < min(data_1$Date)-max(data_0$Date))
    }
    
    calcul_terme_n = function(score_, date_) {
      expected_score = (1 / tau) * (date_ - T_)
      alpha = f(esp, expected_score)
      beta = g(esp, expected_score)
      return(dbeta(score_, alpha, beta, log = TRUE))
    }
    
    calcul_terme_global =  function(data_01_){
      sum(map2_dbl(data_01_$Moult_score, data_01_$Date, calcul_terme_n))
    } 
    
    if(support==1){
      return(calcul_terme_global(data_01))
    }
    else{
      return(-Inf)
    }
} 

#get_likelihood(T_=50, tau =100)


get_prior <- function(data_=data_exo, T_, tau){
  
  data_0 = subset(data_, Moult_score == 0)
  data_1 = subset(data_, Moult_score == 1)
  data_01 = subset(data_, Moult_score > 0 & Moult_score < 1)
  
  min_T_ = max(data_0$Date)
  max_T_ = min(data_01$Date)
  
  min_tau = max(data_01$Date)-min(data_01$Date)
  max_tau = 366
  
  dunif(T_, min = min_T_, max = max_T_, log=TRUE) + 
    dunif(tau, min = min_tau, max = max_tau, log=TRUE) %>% 
    return()
}  

get_prior(T_=50, tau =100)

get_posterior <- function(T_, tau, data__=data_exo){
  log_posterior <- get_prior(T_, tau, data_ = data__) + 
    get_likelihood(T_, tau, data_ = data__)
  return(log_posterior)
}

get_posterior(T_=50, tau =100)


get_metropolis_sampling <- function(pars_init, # Premières valeurs 
                                    n_step, # Nombre d'iterations
                                    step_size = 1){
  T_init = pars_init[1]
  tau_init = pars_init[2]
  
  # On initialise notre sortie, qui sera une matrice
  out <- matrix(ncol = 2, nrow = n_step + 1,
                dimnames = list(NULL, c("T_", "tau")))
  out[1, ] <- pars_init # Valeur initiale de la chaîne
  accepted <- rep(NA, n_step + 1) # On va garder ça en mémoire
  log_posterior <- rep(NA, n_step + 1) # On va garder ça en mémoire
  # Il est souvent conseillé de travailler en logarithme
  log_posterior[1] <- get_posterior(T_ = T_init, tau = tau_init)
  if(is.infinite(log_posterior[1])){
    # Si mon point de départ initial est numériquement trop loin
    # pour éviter les problèmes
    stop("First log posterior value is infinite, change pars_init")
  }
  for(i in 1:n_step){
    (candidate <- rnorm(2, out[i, ], sd = step_size)) # On tire Z ########## explication necessaire!
    # Calcul de pi(Z | X, y)
    candidate_log_posterior <- get_posterior(T_=candidate[1], tau = candidate[2])
    log_u <- log(runif(1)) # En log aussi!
    accepted[i + 1] <- log_u < (candidate_log_posterior - log_posterior[i])
    if (accepted[i + 1]) {
      # Si on accepte
      out[i + 1, ] <- candidate
      log_posterior[i + 1] <- candidate_log_posterior
    }
    else {
      # Si on refuse
      out[i + 1, ] <- out[i, ] # La chaine est encore actualisée!
      log_posterior[i + 1] <- log_posterior[i]
    }
  }
  # On sort sous forme de tableau
  tibble(iteration = 0:n_step) %>%
    bind_cols(as_tibble(out)) %>%
    mutate(log_posterior = log_posterior,
           accepted = accepted) %>%
    return()
}

  

data_  = data_exo
data_0 = subset(data_, Moult_score == 0)
data_1 = subset(data_, Moult_score == 1)
data_01 = subset(data_, Moult_score > 0 & Moult_score < 1)

min_T_ = max(data_0$Date)
max_T_ = min(data_01$Date)

min_tau = max(data_01$Date)-min(data_01$Date)
max_tau = 366


pars_init = c(34,115)
n_step = 10000

# get_metropolis_sampling(pars_init, n_step)

set.seed(123)
premier_mcmc <- get_metropolis_sampling(pars_init,n_step, step_size = 1e-1) 


premier_mcmc %>%
  select(-log_posterior, -accepted) %>% # On vire des colonnes
  gather(-iteration, key = "Parametre",
         value = "Sample", factor_key = TRUE) %>%
  ggplot(aes(x = iteration, y = Sample, colour = Parametre)) +
  geom_line() +
  geom_point() +
  labs(y = "Valeur échantillonnée", x = "Iteration",
       title = "Echantillons a posteriori") 

premier_mcmc %>%
  ggplot(aes(x = T_, y = tau)) +
  geom_path() +
  geom_point()


burn_in = 250
premier_mcmc_sample = premier_mcmc[-(1:burn_in), ]

ggplot(premier_mcmc_sample) +
  aes(x = T_) +
  geom_density( color="darkgreen", fill="lightgreen") +
  geom_vline(aes(xintercept=mean(T_)), color="darkgreen", linetype="dashed", size=1)

ggplot(premier_mcmc_sample) +
  aes(x = tau) +
  geom_density( color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(tau)), color="blue", linetype="dashed", size=1)

