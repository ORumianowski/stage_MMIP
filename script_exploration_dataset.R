rm(list = ls()) # nettoyage de l'environnement de travail


# Chargement du jeu de données --------------------------------------------

library(readxl) # pour charger les fichiers excel
library(tidyr)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)
library(lme4)
library(lmerTest)


data_antler = read_excel("data/Dataset_v2.xlsx", skip = 0, na = "NA")


ggplot(data_antler,
       aes(x = AntlerLength,
           y = AgeAccelResiduals,
           color=AgeClass)) +
  geom_point()

reglm1 <- lm(AgeAccelResiduals ~ AntlerLength+ Age +Age_2+ Population+ WeightAnimal.kg+
               AntlerLength:Age +AntlerLength:Age_2 + AntlerLength:Population,  
             data=data_antler)

reglm1 %>% 
  summary()

reglm2 <- lm(AgeAccelResiduals ~ AntlerLength+ AgeClass + Population+ WeightAnimal.kg+
               AntlerLength:AgeClass + AntlerLength:Population,  
             data=data_antler)

reglm2 %>% 
  summary()

