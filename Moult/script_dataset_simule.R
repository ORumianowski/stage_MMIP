source("utils_packages.R")
library(moult)

# Chargement du jeu de données --------------------------------------------


# Paramètres de simulation ------------------------------------------------

# Durée de la mue

tau = 100

# Date de début de mue moyenne

mu = 50

# Coeffcients spécifiques audata_simul individus

nb_ind = 1

coef_ind = sample(0, nb_ind, replace = TRUE)


# Coefficient associés audata_simul années de mesures

coef_annee = sample(0,2, replace = TRUE)*5


# Plan d'edata_simulpérience -------------------------------------------------------

# 10 individus & 3 mesures par an & 3 années
# Nb_mesure = 10*3*3 = 90

#Les dates de mesurs sont assimilées à une loi homogène de allant de 40 à 80

# Les dates de prises de mesures

coef_tj = sample(30:120,10, replace = TRUE)


# Création des données simulées -------------------------------------------
 

# Création des dates de débuts de mue individuelles -----------------------

# Tia = mu + alpha_i + beta_a

Tia = tibble(Individu = 1:length(coef_ind))

for (my_coef_annee in coef_annee){
  
  Tia_a =map_dbl(coef_ind, .f = function(colonne = coef_ind, coef = my_coef_annee){colonne + mu + my_coef_annee}) 
  
  Tia = bind_cols(Tia, Tia_a)
  
} 



# a corriger

#map_dbl(coef_tj, Tia, .f = function(tau = tau, my_coef_tj = coef_tj){(1/tau)* (my_coef_tj - Tia)} )


data_simul = NULL

for (a in 1:length(coef_annee)){
  for (j in 1:length(coef_tj)){

    y_aj = tibble(Individu = Tia[[1]], 
           Moult_score = (1/tau)* (coef_tj[j] - Tia[[1+a]]), 
           Date = rep(coef_tj[j],nb_ind),
           Annee = rep(a,nb_ind))
    
    data_simul = bind_rows(data_simul, y_aj)
    
  }
}

data_simul = mutate(data_simul,    
           Annee = as.factor(Annee),
           Individu = as.factor(Individu)) %>% 
  mutate( Moult_score = ifelse(Moult_score < 0, 0, Moult_score)) %>% 
  mutate( Moult_score = ifelse(Moult_score > 1, 1, Moult_score))
