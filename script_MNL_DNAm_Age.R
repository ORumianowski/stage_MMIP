rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")
source("script_pretraitement_data_antler.R")

library(rsample)



data_antler = select(data_antler, Age,  DNAmAge) %>% 
  na.omit()

# Definition de la fonction de cout pour n'importe quel predicteur

cost_function <- function(parametre, predicteur, my_data = data_antler){
  sum(map2_dbl(.x = my_data$Age, .y = my_data$DNAmAge, function(x, y) (y - predicteur(parametre, x))^2))
}


# Predicteur Affine ---------------------------------------------------------

predicteur_affine <- function(parametre, Age){
  alpha = parametre[1]
  beta <- parametre[2]
  
  alpha + beta * Age
}


# Predicteur log -------------------------------------------------------


predicteur_log <- function(parametre, Age){
  alpha = parametre[1]
  beta <- parametre[2]
  
  alpha * log(Age+1) + beta * Age
}


# Prédicteur Horvath ------------------------------------------------------

predicteur_Horvath = function(parametre, Age){
  
  alpha = parametre[1]
  beta <- parametre[2]
  adult.age <- parametre[3]
  
  
  alpha * log(Age+1) * (Age < adult.age) +
    (beta * (Age-adult.age)/(adult.age + 1) + alpha * log(adult.age+1)) * (Age>=adult.age)
  
}

# Representation graphique ------------------------------------------------

optimization_function = function(mon_predicteur, nb_par, nb_replicates = 10){
  rerun(nb_replicates,# On fait tourner nb_replicates fois le meme traitement
        {
          param = runif(nb_par, 0, 4) # Generation aleatoire du param initial
          # Optimisation a partir de ce parametre
          res_optim = optim(param, predicteur = mon_predicteur, 
                            cost_function, method = "Nelder-Mead")
          # Aggregation des resultats dans un tableau
          tibble(value = res_optim$value, par =  list(res_optim$par))
        }) %>% 
    bind_rows() %>% # Aggrege dans un tableau
    filter(value == min(value)) %>% # Filtre sur ceux qui atteignent le minimum
    slice(1) %>% # Prend la premiere ligne
    pull(par) %>% # On prend le parametre
    unlist() # On le met en vecteur
}

ggplot(data_antler) +
  aes(x = Age, y = DNAmAge) +
  geom_point()+
  stat_function(aes(color = "log"), 
                fun = predicteur_log, 
                args = list(parametre  = optimization_function(predicteur_log, 
                                                               nb_par = 2))) +
  stat_function(aes(color = "Horvath"), 
                fun = predicteur_Horvath, 
                args = list(parametre  = optimization_function(predicteur_Horvath, 
                                                               nb_par = 3))) +
  
  stat_function(aes(color = "Affine"), 
                fun = predicteur_affine, 
                args = list(parametre  = optimization_function(predicteur_affine, 
                                                               nb_par = 2))) 




# Critere de choix --------------------------------------------------------

cost_function(parametre = optim(optimization_function(predicteur_affine,nb_par = 3), 
                                predicteur = predicteur_Horvath, 
                                cost_function, 
                                method = "Nelder-Mead")$par,
              predicteur = predicteur_Horvath,
              my_data = data_antler)

cost_function(parametre = optim(optimization_function(predicteur_affine,nb_par = 2), 
                                predicteur = predicteur_log,
                                cost_function, 
                                method = "Nelder-Mead")$par,
              predicteur = predicteur_log,
              my_data = data_antler)

cost_function(parametre = optim(optimization_function(predicteur_affine,nb_par = 2), 
                                predicteur = predicteur_affine, 
                                cost_function, 
                                method = "Nelder-Mead")$par,
              predicteur = predicteur_affine,
              my_data = data_antler)

# cross-validation --------------------------------------------------------

cv_function = function(my_prop = 0.7, nb_replicates = 10){
  rerun(nb_replicates,# On fait tourner nb_replicates fois le meme traitement
        { 
          split_cv = initial_split(data_antler, prop=my_prop)
          
          train_data = analysis(split_cv)
          test_data = assessment((split_cv))
          
          res_optim_affine = optim(par = optimization_function(predicteur_affine,nb_par = 2), 
                                   predicteur = predicteur_affine, 
                                   my_data = train_data, 
                                   cost_function, 
                                   method = "Nelder-Mead")$value
          
          res_optim_log = optim(par = optimization_function(predicteur_log,nb_par = 2), 
                                   predicteur = predicteur_log, 
                                   my_data = train_data, 
                                   cost_function, 
                                   method = "Nelder-Mead")$value
          
          res_optim_Horvath = optim(par = optimization_function(predicteur_Horvath,nb_par = 3), 
                                    predicteur = predicteur_Horvath, 
                                    my_data = train_data, 
                                    cost_function, 
                                    method = "Nelder-Mead")$value
          
          tibble(method = c("Affine", "log", "Horvath"),
                 performance = c(res_optim_affine, res_optim_log, res_optim_Horvath))
          
          
        }) %>% 
    bind_rows(.id = "Replicate") 
}

cv_res = cv_function(my_prop = 0.7, nb_replicates = 10)

ggplot(cv_res, aes(x=method, y=performance, color=method)) + 
  geom_boxplot()+ 
  geom_jitter(shape=16, position=position_jitter(0.01))+ 
  ggtitle("Cross-validation \n evaluating models of DNAmAge")

