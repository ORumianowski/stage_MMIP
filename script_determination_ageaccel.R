
data_antler_accel = data_antler[,c("DNAmAge", "Age", "Age_2", "Age_log", "Pop_Id", "Year")] %>% 
  na.omit

regaccel = lm(DNAmAge ~ Age, data_antler_accel)
regaccel_Age_2 = lm(DNAmAge ~ Age + Age_2, data_antler_accel)
regaccel_Age_log = lm(DNAmAge ~ Age + Age_log, data_antler_accel)



data_antler_accel = data_antler_accel %>% 
  mutate(AgeAccelResiduals = regaccel_Age_log$residuals)


# creation de la variable investissement dans les bois via les residus --------


data_antler_invess = data_antler[,c("Antler_std", "Weight", "Pop_Id", "Year")]%>% 
  na.omit %>% 
  mutate(Weight2 = Weight**2)

reginvess = lm(Antler_std ~ Weight , data_antler_invess)
reginvess_2 = lm(Antler_std ~ Weight + Weight2 , data_antler_invess)
reginvess_log = lm(Antler_std ~ Weight + log(Weight), data_antler_invess)

data_antler_invess = data_antler_invess %>% 
  mutate(InvessResiduals = reginvess$residuals)
data_antler_accel = data_antler_accel[, c("Pop_Id", "Year", "AgeAccelResiduals")]
data_antler_complet <-  merge(data_antler, data_antler_accel, by=c("Pop_Id", "Year"), all.x = TRUE)

data_antler_invess = data_antler_invess[, c("Pop_Id", "Year", "InvessResiduals")]
data_antler <-  merge(data_antler_complet, data_antler_invess, by=c("Pop_Id", "Year"), all.x = TRUE)

