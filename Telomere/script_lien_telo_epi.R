rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")
source("script_pretraitement_data_telomere_epi.R")
source("script_standardisation_antler.R")
source("script_determination_ageaccel.R")



data_antler= mutate(data_antler,
                    RTL = scale(RTL),
                    Tbars = scale(Tbars),
                    cohort = as.factor(Cohort))



ggplot(data_antler,
       aes(x = Tbars,
           y = RTL,
           color=AgeClass)) +
  geom_point()


ggplot(data_antler,
       aes(x = Tbars,
           y = AgeAccelResiduals,
           color=AgeClass)) +
  geom_point()

ggplot(data_antler,
       aes(x = AgeAccelResiduals,
           y = RTL,
           color=AgeClass)) +
  geom_point()




data_antler_lm = dplyr::select(data_antler, RTL, Population,Cohort_Type , Age, Age_2, AgeClass, Weight , Antler_std , AgeAccelResiduals) %>% 
  na.omit()


reg_lm_full = lm(Antler_std ~ RTL + Cohort_Type  + AgeAccelResiduals + Age + Age_2 + Weight + Population, data = data_antler_lm) 

options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)

par(mar = c(3,5,6,4))
plot(ms_full, labAsExpr = TRUE)


reg_lm_full = lm(RTL~ Antler_std + Cohort_Type  + AgeAccelResiduals + Age + Age_2 + Weight + Population, data = data_antler_lm) 

options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)

par(mar = c(3,5,6,4))
plot(ms_full, labAsExpr = TRUE)

# variation de AgeAccelResiduals ------------------------------------------


data_difference = dplyr::select(data_antler, Pop_Id, Year, Antler_std, RTL) %>% 
  na.omit() %>% 
  group_by(Pop_Id) %>% 
  group_map(.f = function(tableau, groupe){
    if(nrow(tableau) == 1){
      return(NULL)
    }
    else
      tibble(difference = arrange(tableau, Year) %>% # On trie le tableau par annee
               pull(RTL) %>%  # On extrait la colonne RTL
               diff(),
             Pop_Id = groupe$Pop_Id) %>% 
      return() # On sort les différences
  }) %>% 
  bind_rows() %>% # Pour aggréger la liste de tableau
  left_join(dplyr::filter(data_antler, Year == 2016))

ggplot(data_difference,
       aes(x = AgeAccelResiduals,
           y = difference,
           color = AgeClass,
           size = Weight,
           label = Pop_Id)) +
  geom_point()+
  geom_text(hjust=1, vjust=0)
