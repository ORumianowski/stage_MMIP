rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")
source("script_pretraitement_data_antler.R")

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
    (alpha * (Age-adult.age)/(adult.age + 1) + alpha * log(adult.age+1)) * (Age>=adult.age)
  
}

# Representation graphique ------------------------------------------------



ggplot(data_antler) +
  aes(x = Age, y = DNAmAge) +
  geom_point()+
  stat_function(aes(color = "Pierre"), 
                fun = predicteur_Pierre, 
                args = list(parametre  = optim(c(1, 1), 
                                               predicteur = predicteur_Pierre, 
                                               cost_function, method = "Nelder-Mead")$par)) +
  stat_function(aes(color = "Horvath"), 
                fun = predicteur_Horvath, 
                args = list(parametre  = optim(c(1, 1, 1), 
                                               predicteur = predicteur_Horvath, 
                                               cost_function, method = "Nelder-Mead")$par)) +
  
  stat_function(aes(color = "Affine"), 
                fun = predicteur_affine, 
                args = list(parametre  = optim(c(1, 1), 
                                               predicteur = predicteur_affine, 
                                               cost_function, method = "Nelder-Mead")$par)) 




# Critere de choix --------------------------------------------------------

cost_function(parametre = optim(c(1, 1, 1), predicteur = predicteur_Horvath, cost_function, method = "Nelder-Mead")$par,
              predicteur = predicteur_Horvath)
cost_function(parametre = optim(c(1, 1), predicteur = predicteur_Pierre, cost_function, method = "Nelder-Mead")$par,
              predicteur = predicteur_Pierre)
cost_function(parametre = optim(c(1, 1), predicteur = predicteur_affine, cost_function, method = "Nelder-Mead")$par,
              predicteur = predicteur_affine)



