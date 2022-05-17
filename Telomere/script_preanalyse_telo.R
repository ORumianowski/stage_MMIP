rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")
source("script_pretraitement_data_telomere.R")
source("script_standardisation_antler _telo.R")



data_antler= mutate(data_antler,
                    RTL = scale(RTL))

ggplot(data_antler,
       aes(x = AgeClass,
           y = RTL)) +
  geom_boxplot()

ggplot(data_antler,
       aes(x = Population,
           y = RTL)) +
  geom_boxplot()

ggplot(data_antler,
       aes(x = Cohort_Quality_Pop,
           y = RTL)) +
  geom_point()

ggplot(data_antler,
       aes(x = Age,
           y = RTL,
           color=Population)) +
  geom_point()


ggplot(data_antler,
       aes(x = Antler_std,
           y = RTL,
           color=AgeClass)) +
  geom_point()


ggplot(data_antler,
       aes(x = Weight,
           y = RTL,
           color=AgeClass)) +
  geom_point()

ggplot(data_antler,
       aes(x = Tbars,
           y = RTL,
           color=AgeClass)) +
  geom_point()

# raccourcissement télomérique --------------------------------------------

# On calcule la difference par Pop_Id
# Comme on fait le même traitelent pour chacun, on utilisre group_by et group_map
data_difference_telomere = dplyr::select(data_antler, Id, Year, RTL) %>% 
  na.omit() %>% 
  group_by(Id) %>% 
  group_map(.f = function(tableau, groupe){
    if(nrow(tableau) == 1){
      return(NULL)
    }
    else
      tibble(Difference_telo = arrange(tableau, Year) %>% # On trie le tableau par annee
               pull(RTL) %>%  # On extrait la colonne RTL
               diff(),
             Id = groupe$Id) %>% 
      return() # On sort les différences
  }) %>% 
  bind_rows() %>% # Pour aggréger la liste de tableau
  left_join(dplyr::select(filter(data_antler, Year == 2016), 
                          Antler_right, Id, Tbars, Weight, AgeClass))




ggplot(data_difference_telomere,
       aes(x = Weight,
           y = Difference_telo,
           color = AgeClass)) +
  geom_point()

ggplot(data_difference_telomere,
       aes(x = Antler_right,
           y = Difference_telo,
           color = AgeClass)) +
  geom_point()

ggplot(data_difference_telomere,
       aes(x = Tbars,
           y = Difference_telo,
           color = AgeClass)) +
  geom_point()

# glm ---------------------------------------------------------------------


data_antler_lm = dplyr::select(data_antler, RTL, Population , AgeClass, Weight , Antler_std, Cohort_Quality_Pop) %>% 
  na.omit()

reg_lm_full = lm(RTL ~ Population + AgeClass + Weight + Antler_std + Cohort_Quality_Pop, data = data_antler_lm) 

options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)

par(mar = c(3,5,6,4))
plot(ms_full, labAsExpr = TRUE)

reg_lm_1 = lm(RTL ~ Population, data = data_antler_lm) 
reg_lm_2 = lm(RTL ~ Population + Antler_std + Cohort_Quality_Pop, data = data_antler_lm) 

reg_lm_1 %>% 
  summary()

reg_lm_2 %>% 
  summary()


anova(reg_lm_1, reg_lm_2)

