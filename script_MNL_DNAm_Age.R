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
library(stringr)
library(emmeans)
library(cowplot)
library(nlstools)

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
         Age_2 = Age**2,
         Age_log = log((Age))) %>% 
  rename(Year = YearCapture,
         Day = JulianCaptureDate,
         Cohort_Type = `Cohort_Quality_Type(Good/Bad)`,
         DNAmAge = DNAmAgeLOO,
         AgeAccelLOO = `AgeAccelLOO(ComputedUCLA)`,
         Weight = WeightAnimal.kg,
         ProblemDNA  = ProblemDNA_Concentration,
         RTL = QC_RTL) %>% 
  select(Pop_Id, Year, Day, 
         Cohort, Cohort_Type, Population, 
         Age, Age_2, Age_log, AgeClass, Weight, AntlerLength, AntlerType,
         RTL, DNAmAge, AgeAccelLOO, ProblemDNA)%>% 
  select(Age,  DNAmAge) %>% 
  na.omit()

ggplot(data_antler) +
  aes(x = Age, y = DNAmAge) +
  geom_point()+
  geom_abline(slope = 1,
              intercept = 0,
              colour = "red")

adult.age = 3

fct_DNAm = function(age, adult.age){
  (log(age+1)-log(adult.age+1)) * (age<adult.age)+
    (age-adult.age)/(adult.age+1) * (age>=adult.age)
  
}

fct_DNAm_2 = function(age, adult.age){
  (log(age+1)) * (age<adult.age)+
    ((age-adult.age)/(adult.age+1)+log(adult.age+1)) * (age>=adult.age)
  
}

##Est-ce que la manip pour l'homme ne donne pas quelquechose de linéaire? et la manip n'aurait pas été réaliser poiur les chevreuil expliquant la composante logarithme?


X = (1:100)/10
Y = fct_DNAm(X, adult.age)
Y2 = fct_DNAm_2(X, adult.age) 
plot(X, Y)
plot(X, Y2)

ggplot(data_antler) +
  aes(x = Age, y = fct_DNAm_2(Age, 3) ) +
  geom_point()+
  geom_abline(slope = 1,
              intercept = 0,
              colour = "red")

#pbm avec la partie indicative
formule_modele <- as.formula(DNAmAge ~ (log(Age+1)-log(adult.age+1)) * (Age<adult.age)+
                               (Age-adult.age)/(adult.age+1) * (Age>=adult.age) )


summary(data_antler)

modele <- nls(formule_modele,
                        data = data_antler,
                        start = list(adult.age = 2)) 



plotfit(modele, smooth = TRUE)
plot(nlsResiduals(modele), which = c(1)) 

coef(modele)

nlsResiduals(modele)

