rm(list = ls()) # nettoyage de l'environnement de travail
source("utils_packages.R")
source("script_pretraitement_data_antler.R")
source("script_standardisation_antler.R")
source("script_determination_ageaccel.R")

data_antler_accel = data_antler[,c("DNAmAge", "Age", "Pop_Id", "Year", "AgeAccelLOO", "AgeAccelResiduals")] %>% 
  na.omit

regaccel = lm(DNAmAge ~ Age, data_antler_accel)

data_antler_accel = data_antler_accel %>% 
  mutate(Residus_simple = regaccel$residuals)


data_antler_accel$Diff = data_antler$DNAmAge - data_antler$Age
data_antler_accel = data_antler_accel[, c("Age", "DNAmAge" , "Diff","Residus_simple" , "AgeAccelLOO", "AgeAccelResiduals")]

data_antler_accel %>% 
  summary()


data_antler_accel$Diff %>% 
  var()
data_antler_accel$Residus_simple %>% 
  var()
data_antler_accel$AgeAccelLOO %>% 
  var()
data_antler_accel$AgeAccelResiduals %>% 
  var()


ggplot(data_antler_accel,
       aes(x = AgeAccelResiduals,
           y = AgeAccelLOO)) +
  geom_point()

