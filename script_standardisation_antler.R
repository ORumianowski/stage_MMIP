rm(list = ls()) # nettoyage de l'environnement de travail


# Chargement du jeu de données --------------------------------------------

library(readxl) # pour charger les fichiers excel
library(tidyr)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)
library(lme4)

# Définition des fonctions de modélisation de la longueur des bois --------

# fonction constante

constante = function(pars, x){
  
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
y = lineaire(c(2, 5, 1), x) + rnorm(length(x), sd = .5)
#plot(x, y)

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



# Application aux données réelles -----------------------------------------

data_antler = read_excel("data/Dataset_ODIN_160422.xlsx", skip = 0, na = "NA") %>% 
  mutate(PlateNumber = as.factor(PlateNumber), 
         Batch_number = as.factor(Batch_number),
         ProblemDNA_Concentration = as.factor(ProblemDNA_Concentration),
         PlateTelomere  = as.factor(PlateTelomere),
         DNAmAgeLOO = as.numeric(DNAmAgeLOO),
         `AgeAccelLOO(ComputedUCLA)` = as.numeric(`AgeAccelLOO(ComputedUCLA)`),
         RightAntlerLength    = as.numeric(RightAntlerLength)) %>% 
  rename(AgeAccelLOOUCLA =  `AgeAccelLOO(ComputedUCLA)`)


data_antler_2 = select(data_antler, 
                       Left_AntlerLength, RightAntlerLength, JulianCaptureDate, Age, AntlerType) %>% 
  mutate(AntlerLength = rowMeans(data_antler[,c('Left_AntlerLength', 'RightAntlerLength')], na.rm=TRUE)) %>% 
  mutate(AgeClass = cut(c(data_antler[,"Age"])$Age, breaks = c(0,1,4,8,25))) %>% 
  mutate(AgeClass = as.character(AgeClass))

data_antler_3 = data_antler_2 %>%
  subset( AntlerType == "BV") %>% 
  subset( AgeClass != "(0,1]") %>% 
  na.omit()

x_real = data_antler_3$JulianCaptureDate
y_real = data_antler_3$AntlerLength

ggplot(data_antler_3,
       aes(x = JulianCaptureDate  ,
           y = AntlerLength,
           color=AntlerType)) +
  geom_point()+
  
  geom_abline(slope=rate_GLMM1,
              intercept=intercep_GLMM1,
              colour="red")


## Pbm de generalisation de la fonction get_AIC: mettre les x et y en arguments
plan_experience = tibble(ma_fonction = c(constante, lineaire, one_slope, two_slopes),
                         initial_pars = list(c(1,1), c(1, 0, 1), c(1, 0, 1, 1), c(1, 0, 0, 2, 1)))

get_AIC = function(initial_pars, ma_fonction){
  neg_log_lik = optim(par = initial_pars, 
                      fn = function(p) NLL(p, 
                                           ma_fonction = ma_fonction, 
                                           y = x_real, 
                                           x = y_real),
                      hessian = TRUE)$value
  2 * neg_log_lik + 2 * length(initial_pars)
}

purrr::pmap_dbl(plan_experience,
                get_AIC)



optim_output = optim(par = c(3,0,10), 
      fn = function(p) NLL(p, 
                           ma_fonction = lineaire, 
                           y = x, 
                           x = y))


optim_output$par #Valeur absurde?

ggplot(data_antler_3,
       aes(x = JulianCaptureDate  ,
           y = AntlerLength,
           color=AntlerType)) +
  geom_point()+
  
  geom_abline(slope=optim_output$par[1],
              intercept=optim_output$par[2],
              colour="red")




