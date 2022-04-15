rm(list = ls()) # nettoyage de l'environnement de travail


# Chargement du jeu de données --------------------------------------------

library(readxl) # pour charger les fichiers excel
library(tidyr)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)
library(lme4)

data_antler = read_excel("data/Dataset_JAE-201700618.xls", skip = 2, na = "NA") %>% 
  rename(antler20 = `Antlers length at 20 months of age(mm)- values standardized by the Julian date [see methods`) %>% 
  mutate(antler20 = as.numeric(antler20)) %>% 
  na.omit()
data_antler = select(data_antler, antler20, ) %>% 
  mutate(antler20_log = log(antler20))

# Définition des fonctions de modélisation de la longueur des bois --------

# fonction constante

constante = function(pars){
  
  a = pars[1]
  
  a} 

# fonction linéaire

lineaire = function(pars, x){
  
  a = pars[1]
  b = pars[2]

  a*x+b} 

# fonction de seuil avec une pente et un plateau

one_slope = function(pars, x){
  
  a = pars[1]
  b = pars[2]
  seuil = pars[3]
  
  ifelse(x<seuil, a*x+b, a*seuil+b)
} 

# fonction de seuil avec deux pentes

two_slopes = function(pars, x){
  
  a = pars[1]
  b = pars[2]
  seuil = pars[3]
  c = pars[4]
  OO_2 = (a - c) * seuil + b

  ifelse(test = x<seuil, yes = a*x+b, no = c*x + OO_2)
} 

# fonction exponentielle

fonction_expo = function(pars,  x){
  
  plateau = pars[1] 
  k = pars[2]
  
  plateau * (1 - exp(-k*x))
}




# Sélection de la meilleure fonction --------------------------------------


NLL = function(pars, ma_fonction, y, x) {
  # Values predicted by the model
  sigma = pars[length(pars)]
  parametres_moyenne = pars[-length(pars)] 
  Gpred = ma_fonction(parametres_moyenne, x)
  # Negative log-likelihood 
  -sum(dnorm(x = y, mean = Gpred, sd = sigma, log = TRUE))
}

plan_experience = tibble(ma_fonction = c(lineaire, one_slope, two_slopes),
       initial_pars = list(c(1, 0, 1), c(1, 0, 1, 1), c(1, 0, 0, 2, 1)))

n = 1000
x = abs(rnorm(n))
y = two_slopes(c(1, 0, 1.5, 4), x) + rnorm(length(x), sd = .5)
plot(x, y)
get_AIC = function(initial_pars, ma_fonction){
    neg_log_lik = optim(par = initial_pars, 
          fn = function(p) NLL(p, 
                               ma_fonction = ma_fonction, 
                               y = y, 
                               x = x),
          hessian = TRUE)$value
    2 * neg_log_lik + 2 * length(initial_pars)
}

purrr::pmap_dbl(plan_experience,
            get_AIC)
