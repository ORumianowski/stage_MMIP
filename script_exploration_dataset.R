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


data_antler = read_excel("data/Dataset_v2.xlsx", skip = 0, na = "NA") %>% 
  mutate(AntlerLengthLog = log(AntlerLength)) %>% 
  select(-ProblemDescription)


ggplot(data_antler,
       aes(x = AntlerLength,
           y = AgeAccelResiduals,
           color=AgeClass)) +
  geom_point()

reglm1 <- lm(AgeAccelResiduals ~ AntlerLength + Age +Age_2+ Population+ WeightAnimal.kg+
               AntlerLength:Age +AntlerLength:Age_2 + AntlerLength:Population,  
             data=data_antler)

reglm1 %>% 
  summary()

reglm2 <- lm(AgeAccelResiduals ~ AntlerLength + AgeClass + Population + WeightAnimal.kg +
               AntlerLength:AgeClass + AntlerLength:Population,  
             data=na.omit(data_antler))
vis_miss(data_antler)

reglm3 = lm(AgeAccelResiduals ~ AntlerLength,
            data = filter(data_antler, AgeClass == "(0,1]"))

ggplot(filter(data_antler, AgeClass == "(0,1]"),
       aes(x = AntlerLength, y = AgeAccelResiduals)) +
  geom_point()
modele <- step(reglm2)
car::Anova(reglm3)
summary(reglm3)
plot(modele, which = 1)
library(emmeans)

ggplot(data_antler,
       aes(x = InvessResiduals,
           y = AgeAccelResiduals,
           color=AgeClass)) +
  geom_point()

reglm3 <- lm(AgeAccelResiduals ~ InvessResiduals+Age +Age_2+ Population+
               AntlerLength:Age +AntlerLength:Age_2 + AntlerLength:Population,  
             data=data_antler)

reglm3 %>% 
  summary()

