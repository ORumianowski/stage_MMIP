

rm(list = ls()) # nettoyage de l'environnement de travail

source("utils_packages.R")
library(moult)

# Chargement du jeu de données --------------------------------------------



# Paramètres de simulation ------------------------------------------------

# Durée de la mue

tau = 100

# Date de début de mue moyenne

mu = 50

# Coeffcients spécifiques audata_simul individus

coef_ind = sample(-10:10, 100, replace = TRUE)


# Coefficient associés audata_simul années de mesures

coef_annee = sample(-3:3,5, replace = TRUE)*5


# Plan d'edata_simulpérience -------------------------------------------------------

# 10 individus & 3 mesures par an & 3 années
# Nb_mesure = 10*3*3 = 90

#Les dates de mesurs sont assimilées à une loi homogène de allant de 40 à 80

# Les dates de prises de mesures

coef_tj = sample(30:70,5, replace = TRUE)


# Création des données simulées -------------------------------------------
 

# Création des dates de débuts de mue individuelles -----------------------

# Tia = mu + alpha_i + beta_a

Tia = tibble(Individu = 1:length(coef_ind))

for (my_coef_annee in coef_annee){
  
  f =map_dbl(coef_ind, .f = function(colonne = coef_ind, coef = my_coef_annee){colonne + mu + my_coef_annee}) 
  
  Tia = bind_cols(Tia, f)
  
} 



# a corriger

#map_dbl(coef_tj, Tia, .f = function(tau = tau, my_coef_tj = coef_tj){(1/tau)* (my_coef_tj - Tia)} )


data_simul = NULL

for (j in 1:length(coef_tj)){
  for (a in 1:length(coef_annee)){
    
    y_aj = tibble(Individu = Tia[[1]], 
           Moult_score = (1/tau)* (coef_tj[j] - Tia[[1+a]]), 
           Date = rep(coef_tj[j],100),
           Annee = rep(a,100))
    
    data_simul = bind_rows(data_simul, y_aj)
    
  }
}

data_simul = mutate(data_simul,
           Annee = as.factor(Annee),
           Individu = as.factor(Individu))

data_plot = subset(data_simul)
ggplot(data_plot,
       aes(x = Date,
           y = Moult_score,
           color=Annee)) +
  geom_point()


res_moult = moult( data_simul$Moult_score ~ data_simul$Date | 1 
                   
                   | data_simul$Annee,
                   
                   type = 3,
                   prec = 0.01)


res_moult %>% 
  summary()
