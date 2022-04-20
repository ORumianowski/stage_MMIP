rm(list = ls()) # nettoyage de l'environnement de travail


# Chargement du jeu de données --------------------------------------------

library(readxl)
library(writexl)
library(tidyr)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)
library(lme4)
library(lmerTest)


data_antler = read_excel("data/Dataset_ODIN_160422.xlsx", skip = 0, na = "NA") %>% 
  mutate(PlateNumber = as.factor(PlateNumber), 
         Batch_number = as.factor(Batch_number),
         ProblemDNA_Concentration = as.factor(ProblemDNA_Concentration),
         PlateTelomere  = as.factor(PlateTelomere),
         DNAmAgeLOO = as.numeric(DNAmAgeLOO),
         `AgeAccelLOO(ComputedUCLA)` = as.numeric(`AgeAccelLOO(ComputedUCLA)`),
         RightAntlerLength    = str_remove(RightAntlerLength, "_broken") %>% 
           as.numeric(),
         AntlerLength = map2_dbl(.x = Left_AntlerLength, 
                                 .y = RightAntlerLength,
                                 .f = function(x, y){
                                   if(is.na(x))
                                     return(y)
                                   else if(is.na(y))
                                     return(x)
                                   else
                                     return(max(x, y))
                                 }
         ),
         AgeClass = cut(Age, breaks = c(0,1,4,8,25)) %>% 
           as.character(),
         Age_2 = Age**2)



# creation de la variable DNAm acceleration via les residus ---------------

data_antler_accel = select(data_antler, 
                           DNAmAgeLOO, Age, Age_2, Pop_Id, YearCapture) %>% 
  na.omit

regaccel = lm(DNAmAgeLOO ~ Age + Age_2, data_antler_accel)

data_antler_accel = data_antler_accel %>% 
  mutate(AgeAccelResiduals = regaccel$residuals)


# creation de la variable investissement dans les bois via les res --------


data_antler_invess = select(data_antler, 
                           AntlerLength, WeightAnimal.kg, Pop_Id, YearCapture) %>% 
  na.omit

reginvess = lm(AntlerLength ~ WeightAnimal.kg, data_antler)

data_antler_invess = data_antler_invess %>% 
  mutate(InvessResiduals = reginvess$residuals)


# creation dataset avec les variables tranformées -------------------------

data_antler_accel = data_antler_accel[, c("Pop_Id", "YearCapture", "AgeAccelResiduals")]
data_antler_complet <-  merge(data_antler, data_antler_accel, by=c("Pop_Id", "YearCapture"), all.x = TRUE)

data_antler_invess = data_antler_invess[, c("Pop_Id", "YearCapture", "InvessResiduals")]
data_antler_complet <-  merge(data_antler_complet, data_antler_invess, by=c("Pop_Id", "YearCapture"), all.x = TRUE)


write_xlsx(data_antler_complet,"data/Dataset_v2.xlsx")

