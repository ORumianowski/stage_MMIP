rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")
source("script_pretraitement_data_antler.R")
source("script_standardisation_antler.R")
source("script_determination_ageaccel.R")



# corrélation -------------------------------------------------------------

library(ppcor)

data_antler_cor = data_antler[,c("RTL", "DNAmAge","Age", "Weight", "Antler_std", "InvessResiduals")] %>% 
  na.omit()


correlation <- data_antler_cor %>%
  cor()
corrplot(correlation) 

pcorrelation <- data_antler_cor %>%
  pcor()
corrplot(pcorrelation$estimate)


# analyse graphique -------------------------------------------------------


ggplot(data_antler,
       aes(x = Age,
           y = RTL)) +
  geom_point()

ggplot(data_antler,
       aes(x = Age,
           y = RTL,
           color=Cohort_Type)) +
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
  geom_point()+
  labs( x = "Standardized antler length")


ggplot(data_antler,
       aes(x = Weight,
           y = RTL,
           color=AgeClass)) +
  geom_point()

ggplot(data_antler,
       aes(x = AgeAccelLOO,
           y = RTL,
           color=AgeClass)) +
  geom_point()

ggplot(data_antler,
       aes(x = AgeAccelResiduals,
           y = RTL,
           color=AgeClass)) +
  geom_point()

ggplot(data_antler,
       aes(x = InvessResiduals,
           y = RTL,
           color=AgeClass)) +
  geom_point()

# raccourcissement télomérique --------------------------------------------

# On calcule la difference par Pop_Id
# Comme on fait le même traitelent pour chacun, on utilisre group_by et group_map
difference_telomere = dplyr::select(data_antler, Pop_Id, Year, RTL) %>% 
  na.omit() %>% 
  group_by(Pop_Id) %>% 
  group_map(.f = function(tableau, groupe){
    if(nrow(tableau) == 1){
      return(NULL)
    }
    else
      tibble(Difference = arrange(tableau, Year) %>% # On trie le tableau par annee
               pull(RTL) %>%  # On extrait la colonne RTL
               diff(),
             Pop_Id = groupe$Pop_Id) %>% 
      return() # On sort les différences
  }) %>% 
  bind_rows() %>% # Pour aggréger la liste de tableau
  left_join(dplyr::select(filter(data_antler, Year == 2016), 
                          Pop_Id, AgeAccelLOO))




ggplot(difference_telomere,
       aes(x = AgeAccelLOO,
           y = Difference)) +
  geom_point()

