rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")
source("script_pretraitement_data_antler.R")


data_antler = select(data_antler, Age,  DNAmAge) %>% 
  na.omit()

# Definition de la fonction de cout pour n'importe quel predicteur

cost_function <- function(parametre, predicteur){
  sum(map2_dbl(.x = data_antler$Age, .y = data_antler$DNAmAge, function(x, y) (y - predicteur(parametre, x))^2))
}


# Predicteur Affine ---------------------------------------------------------

predicteur_affine <- function(parametre, Age){
  alpha = parametre[1]
  beta <- parametre[2]
  
  alpha + beta * Age
}


# Predicteur Pierre -------------------------------------------------------


predicteur_Pierre <- function(parametre, Age){
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
    bind_rows(.id = "Replicate") %>% # Aggrege dans un tableau
    filter(value == min(value)) %>% # Filtre sur ceux qui atteignent le minimum
    slice(1) %>% # Prend la premiere ligne
    pull(par) %>% # On prend le parametre
    unlist() # On le met en vecteur
}

ggplot(data_antler) +
  aes(x = Age, y = DNAmAge) +
  geom_point()+
  stat_function(aes(color = "Pierre"), 
                fun = predicteur_Pierre, 
                args = list(parametre  = optimization_function(predicteur_Pierre, 
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

cost_function(parametre = optim(c(2.6, 2.6, 2.4), predicteur = predicteur_Horvath, cost_function, method = "Nelder-Mead")$par,
              predicteur = predicteur_Horvath)
cost_function(parametre = optim(c(1, 1), predicteur = predicteur_Pierre, cost_function, method = "Nelder-Mead")$par,
              predicteur = predicteur_Pierre)
cost_function(parametre = optim(c(1, 1), predicteur = predicteur_affine, cost_function, method = "Nelder-Mead")$par,
              predicteur = predicteur_affine)




