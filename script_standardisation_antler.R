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
  select(antler20) %>% 
  na.omit()

data_antler$day = data_antler$antler20 / 2 

data_antler = select(data_antler, antler20, day) %>% 
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
  
  if (x<seuil){
    a*x+b
  }
  else{
    a*seuil+b
  }
} 

# fonction de seuil avec deux pentes

two_slopes = function(pars, x){
  
  a = pars[1]
  b = pars[2]
  seuil = pars[3]
  c = pars[4]
  

  if (x<seuil){
    a*x+b
  }
  else{
    c*x + (a*seuil+b)
  }
} 

# fonction exponentielle

fonction_expo = function(pars,  x){
  
  plateau = pars[1] 
  k = pars[2]
  
  plateau * (1 - exp(-k*x))
}



# Sélection de la meilleure fonction --------------------------------------




# determination des parametres et de l'AIC

NLL = function(pars, data) {

  pred = lineaire(pars, data$day)

  -sum(dnorm(x = data$day, mean = pred, sd = pars[4], log = TRUE))

}


par0 = c(a = 1.0, b = 0.15, sd = 0.1)
fit = optim(par = par0, fn = NLL, data = data_antler, control = list(parscale = abs(par0)),
            hessian = TRUE)
fit$par

library("EstimationTools")
#fit1 <- maxlogL(x = data_antler$antler20,  start=c(2, 3))


# Fonction de standardisation ---------------------------------------------

REF_DATE = 10

standardised_antler = function(antler, day, fct_modele, ref_date=REF_DATE){
  
  fct_modele(ref_date) + antler - fct_modele(day)

}















