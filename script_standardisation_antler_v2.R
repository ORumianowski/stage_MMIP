rm(list = ls()) # nettoyage de l'environnement de travail


# Chargement du jeu de données --------------------------------------------

library(readxl) # pour charger les fichiers excel
library(tidyr)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)
library(lme4)

data_antler = read_excel("data/Dataset_v2.xlsx", skip = 0, na = "NA") %>% 
  select(
         Pop_Id, 
         YearCapture,
         AntlerLength,
         JulianCaptureDate,
         AntlerType,
         AgeClass,
         Population) 

# Problem with the sample
data_antler = data_antler[-which(data_antler$Pop_Id=="CM2301" & data_antler$YearCapture=="2016") , ]
# Problem with the sample (Female)
data_antler = data_antler[-which(data_antler$Pop_Id=="TM368" & data_antler$YearCapture=="2018") , ]

# Unmeasured Antler Length
data_antler = data_antler[-which(data_antler$Pop_Id=="CM2317" & data_antler$YearCapture=="2016") , ]
data_antler = data_antler[-which(data_antler$Pop_Id=="TM3069" & data_antler$YearCapture=="2018") , ]

vis_miss(data_antler)

DATE_REF = 80

std_antler = function(antler_length, date, pars, date_ref = DATE_REF) {
  a = pars[1]
  b = pars[2]
  (antler_length - (a * date + b))  + a * date_ref + b
}


# Définition des fonctions de modélisation de la longueur des bois --------

# fonction constante

constante = function(pars, x) {
  a = pars[1]
  
  a
}

# fonction linéaire

lineaire = function(pars, x = 0) {
  a = pars[1]
  b = pars[2]
  
  a * x + b
}

# fonction de seuil avec une pente et un plateau

one_slope = function(pars, x) {
  a = pars[1]
  b = pars[2]
  seuil = pars[3]
  
  ifelse(x < seuil, a * x + b, a * seuil + b)
}

# fonction de seuil avec deux pentes

two_slopes = function(pars, x) {
  a = pars[1]
  b = pars[2]
  seuil = pars[3]
  c = pars[4]
  OO_2 = (a - c) * seuil + b
  
  ifelse(test = x < seuil,
         yes = a * x + b,
         no = c * x + OO_2)
}


n = 1000
X = abs(rnorm(n))
Y = lineaire(c(2, 5, 1), X) + rnorm(length(X), sd = .5)


plan_experience = tibble(
  ma_fonction = c(constante, lineaire, one_slope, two_slopes),
  initial_pars = list(c(0, 1), c(1, 0, 1), c(1, 0, 1, 1), c(1, 0, 0, 2, 1))
)



NLL = function(pars,
               ma_fonction,
               y = Y,
               x = X) {
  # Values predicted by the model
  sigma = pars[length(pars)]
  parametres_moyenne = pars[-length(pars)]
  Gpred = ma_fonction(parametres_moyenne, x)
  # Negative log-likelihood 
  -sum(dnorm(
  x = y,
  mean = Gpred,
  sd = sigma,
  log = TRUE
  ))
}


get_AIC = function(initial_pars,
                   ma_fonction,
                   y = Y,
                   x = X) {
  neg_log_lik = optim(
    par = initial_pars,
    fn = function(p)
      NLL(
        p,
        ma_fonction = ma_fonction,
        y = Y,
        x = X
      ),
    hessian = TRUE
  )$value
  2 * neg_log_lik + 2 * length(initial_pars)
}

purrr::pmap_dbl(plan_experience,
                get_AIC)

optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)

optim_output$par



# CHIZE -------------------------------------------------------------------


# Premiere classe ---------------------------------------------------------

data_antler_2 = subset(data_antler, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "C" & AntlerLength > 1) 

X = data_antler_2$JulianCaptureDate
Y = data_antler_2$AntlerLength %>%
  log()

purrr::pmap_dbl(plan_experience,
                get_AIC)

optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)

ggplot(data_antler_2,
       aes(x = X,
           y = Y,
           color = AntlerType)) +
  geom_point() +
  
  geom_abline(slope = optim_output$par[1],
              intercept = optim_output$par[2],
              colour = "red")+
  labs( title = "Chizé - Première classe d'âge",
        x= "Julian Capture Date",
        y= "Antler Length (log)")


data_antler_3 = mutate(data_antler_2,
                       antler_std = exp(std_antler(
                         log(AntlerLength), JulianCaptureDate, optim_output$par
                       )))


data_antler_1C = data_antler_3[, c("Pop_Id", "YearCapture", "antler_std")]


# Classes supérieures -----------------------------------------------------


data_antler_2 = subset(data_antler, AntlerType == "BV"| is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "C") 

X = data_antler_2$JulianCaptureDate
Y = data_antler_2$AntlerLength %>%
  log()

purrr::pmap_dbl(plan_experience,
                get_AIC)

optim_output = optim(
  par = c(0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = constante,
      y = Y,
      x = X
    )
)


ggplot(data_antler_2,
       aes(x = X,
           y = Y,
           color = AntlerType)) +
  geom_point() +
  
  geom_abline(slope = 0,
              intercept = optim_output$par[1],
              colour = "red")  +
   labs( title = "Chizé - Classes d'âge supérieures",
         x= "Julian Capture Date",
         y= "Antler Length (log)")


data_antler_3 = mutate(data_antler_2,
                       antler_std = AntlerLength)


data_antler_NC = data_antler_3[, c("Pop_Id", "YearCapture", "antler_std")]

# Trois Fontaines ---------------------------------------------------------

# Premiere classe ---------------------------------------------------------

data_antler_2 = subset(data_antler,AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "TF") 

X = data_antler_2$JulianCaptureDate
Y = data_antler_2$AntlerLength %>%
  log()

purrr::pmap_dbl(plan_experience,
                get_AIC)

optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)

ggplot(data_antler_2,
       aes(x = X,
           y = Y,
           color = AntlerType)) +
  geom_point() +
  
  geom_abline(slope = optim_output$par[1],
              intercept = optim_output$par[2],
              colour = "red")+
  labs( title = "Trois-Fontaines - Première classe d'âge",
        x= "Julian Capture Date",
        y= "Antler Length (log)")


data_antler_3 = mutate(data_antler_2,
                       antler_std = exp(std_antler(
                         log(AntlerLength), JulianCaptureDate, optim_output$par
                       )))


data_antler_1TF = data_antler_3[, c("Pop_Id", "YearCapture", "antler_std")]


# Classes supérieures -----------------------------------------------------


data_antler_2 = subset(data_antler, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "TF") 

X = data_antler_2$JulianCaptureDate
Y = data_antler_2$AntlerLength %>%
  log()

purrr::pmap_dbl(plan_experience,
                get_AIC)

optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)

ggplot(data_antler_2,
       aes(x = X,
           y = Y,
           color = AntlerType)) +
  geom_point() +
  
  geom_abline(slope = optim_output$par[1],
              intercept = optim_output$par[2],
              colour = "red")+
  labs( title = "Trois-Fontaines - Classes d'âge supérieures",
        x= "Julian Capture Date",
        y= "Antler Length (log)")


data_antler_3 = mutate(data_antler_2,
                       antler_std = exp(std_antler(
                         log(AntlerLength), JulianCaptureDate, optim_output$par
                       )))

data_antler_NTF = data_antler_3[, c("Pop_Id", "YearCapture", "antler_std")]

data_antler_std = rbind(data_antler_1C,
                        data_antler_NC,
                        data_antler_1TF,
                        data_antler_NTF)

data_antler_complet <-  merge(data_antler, data_antler_std, by=c("Pop_Id", "YearCapture"), all.x = TRUE)

for (k in 1:nrow(data_antler_complet)){
  if (!is.na(data_antler_complet[k, "AntlerType"])){
    if (data_antler_complet[k, "AntlerType"] == "BD"){
      data_antler_complet[k, "antler_std"] = data_antler_complet[k, "AntlerLength"]
    }
  }
  if (data_antler_complet[k, "AntlerLength"] < 1){
    data_antler_complet[k, "antler_std"] = data_antler_complet[k, "AntlerLength"]
  }
  
}



