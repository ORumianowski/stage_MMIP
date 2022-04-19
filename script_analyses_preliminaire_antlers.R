rm(list = ls()) # nettoyage de l'environnement de travail


# Chargement du jeu de données --------------------------------------------

library(readxl) # pour charger les fichiers excel
library(tidyr)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)
library(lme4)
library(naniar)
library(UpSetR)
library(corrplot)

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
       Left_AntlerLength, JulianCaptureDate, YearCapture, Cohort, Age, Population,
       WeightAnimal.kg, QC_RTL, DNAmAgeLOO, AgeAccelLOOUCLA) %>% 
  mutate(YearCapture   = as.factor(YearCapture))  %>% 
  mutate(Cohort  = as.factor(Cohort))   # Mise en facteur pour un potentiel effet mixte


# Analyse des NA ----------------------------------------------------------

vis_miss(data_antler_2)

gg_miss_upset(data_antler_2)


# Description de la population --------------------------------------------

table(data_antler$Sex)
table(data_antler$Population)
table(data_antler$Sex, data_antler$Age)

hist(data_antler$Age)

# Relations entre variables ---------------------------------------------

data_antler_3 = select(data_antler, 
                       Left_AntlerLength, JulianCaptureDate, Age,
                       WeightAnimal.kg, QC_RTL, DNAmAgeLOO, AgeAccelLOOUCLA) %>% 
  na.omit()


correlation <- data_antler_3 %>%
  cor()
corrplot(correlation) 



plot(data_antler_2[, c("Left_AntlerLength", "JulianCaptureDate", "Age",
                       "WeightAnimal.kg", "QC_RTL", "DNAmAgeLOO", "AgeAccelLOOUCLA")] , pch=20 , cex=1.5 , col="#69b3a2")


ggplot(data_antler_2,
       aes(x = Left_AntlerLength,
           y = AgeAccelLOOUCLA)) +
  geom_point()

ggplot(data_antler_2,
       aes(x = Left_AntlerLength,
           y = AgeAccelLOOUCLA,
           color=Age,
           size=Age)) +
  geom_point()

ggplot(data_antler_2,
       aes(x = Left_AntlerLength,
           y = QC_RTL)) +
  geom_point()

ggplot(data_antler_2,
       aes(x = JulianCaptureDate  ,
           y = Left_AntlerLength)) +
  geom_point()
