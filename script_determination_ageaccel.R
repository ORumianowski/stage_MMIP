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


data_antler = read_excel("data/Dataset_ODIN_160422.xlsx", skip = 0, na = "NA") %>% 
  mutate(PlateNumber = as.factor(PlateNumber), 
         Batch_number = as.factor(Batch_number),
         ProblemDNA_Concentration = as.factor(ProblemDNA_Concentration),
         PlateTelomere  = as.factor(PlateTelomere),
         DNAmAgeLOO = as.numeric(DNAmAgeLOO),
         `AgeAccelLOO(ComputedUCLA)` = as.numeric(`AgeAccelLOO(ComputedUCLA)`),
         RightAntlerLength    = as.numeric(RightAntlerLength)) %>% 
  rename(AgeAccelLOOUCLA =  `AgeAccelLOO(ComputedUCLA)`)


data_antler_2 = select(data_antler, DNAmAgeLOO, Age, AgeAccelLOOUCLA, Cohort, JulianCaptureDate ,WeightAnimal.kg, Population,  Left_AntlerLength, RightAntlerLength) %>% 
  mutate(AntlerLength = rowMeans(data_antler[,c('Left_AntlerLength', 'RightAntlerLength')], na.rm=TRUE)) %>% 
  mutate(AgeClass = cut(c(data_antler[,"Age"])$Age, breaks = c(0,1,4,8,25))) %>% 
  mutate(AgeClass = as.character(AgeClass)) %>% 
  mutate(Age_2 = Age**2) %>% 
  na.omit()


plot(data_antler_2$Age, data_antler_2$DNAmAgeLOO)

regaccel = lm(DNAmAgeLOO ~ Age + Age_2, data_antler_2)

reginvess = lm(AntlerLength ~ WeightAnimal.kg, data_antler_2)



data_antler_3 = data_antler_2 %>% 
  mutate(AgeAccelResiduals = regaccel$residuals) %>% 
  mutate(InvessResiduals = reginvess$residuals)
  subset(Age>=1)

plot(data_antler_3$AgeAccelResiduals, data_antler_3$AgeAccelLOOUCLA)


ggplot(data_antler_3,
       aes(x = InvessResiduals,
           y = AgeAccelResiduals,
           color=AgeClass)) +
  geom_point()

reglm <- lm(AgeAccelResiduals ~ InvessResiduals+ AgeClass + Population+
                  InvessResiduals:AgeClass + InvessResiduals:Population,  
                data=data_antler_3)

reglm %>% 
  summary()
  
