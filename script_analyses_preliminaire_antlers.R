rm(list = ls()) # nettoyage de l'environnement de travail


# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")
source("script_pretraitement_data_antler.R")

data_antler = data_antler[, c("AntlerLength", "Day", "Year", "Cohort", "Age", "Population",
                              "Weight", "RTL", "DNAmAge", "AgeAccelLOO")]

data_antler_2 = mutate(data_antler, Year   = as.factor(Year))  %>% 
  mutate(Cohort  = as.factor(Cohort))  


# Analyse des NA ----------------------------------------------------------

vis_miss(data_antler_2)

gg_miss_upset(data_antler_2)


# Description de la population --------------------------------------------

table(data_antler$Sex)
table(data_antler$Population)
table(data_antler_2$AgeClass, data_antler_2$Population)

table(data_antler$Id) 
table(data_antler$Id) %>% 
  table()


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
           color=AgeClass)) +
  geom_point()

data_antler_3 = data_antler_2 %>%
  subset( AgeClass != "(0,1]" ) %>%
  na.omit()

ggplot(data_antler_3,
       aes(x = Left_AntlerLength,
           y = AgeAccelLOOUCLA,
           color= AgeClass,
           size = Age)) +
  geom_point()

ggplot(data_antler_2,
       aes(x = Left_AntlerLength,
           y = QC_RTL)) +
  geom_point()

ggplot(data_antler_2,
       aes(x = AgeAccelLOOUCLA  ,
           y = QC_RTL)) +
  geom_point()

ggplot(data_antler_2,
       aes(x = JulianCaptureDate  ,
           y = Left_AntlerLength,
           color=AgeClass)) +
  geom_point()

ggplot(data_antler,
       aes(x = WeightAnimal.kg  ,
           y = AgeAccelLOOUCLA,
           color=Age,
           size=Age)) +
  geom_point()


reglmer <- lmer(AgeAccelLOOUCLA ~ Left_AntlerLength + JulianCaptureDate +WeightAnimal.kg+ AgeClass + Population+QC_RTL+
                  Left_AntlerLength:AgeClass + Left_AntlerLength:Population+
                   (1 | Cohort),  
                 data=data_antler_2)

ggplot(data_antler) +
  aes(x = Left_AntlerLength, y = JulianCaptureDate) +
  geom_point()
  
