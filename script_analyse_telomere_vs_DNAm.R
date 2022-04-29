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
data_difference_telomere = dplyr::select(data_antler, Pop_Id, Year, RTL) %>% 
  na.omit() %>% 
  group_by(Pop_Id) %>% 
  group_map(.f = function(tableau, groupe){
    if(nrow(tableau) == 1){
      return(NULL)
    }
    else
      tibble(Difference_telo = arrange(tableau, Year) %>% # On trie le tableau par annee
               pull(RTL) %>%  # On extrait la colonne RTL
               diff(),
             Pop_Id = groupe$Pop_Id) %>% 
      return() # On sort les différences
  }) %>% 
  bind_rows() %>% # Pour aggréger la liste de tableau
  left_join(dplyr::select(filter(data_antler, Year == 2016), 
                          Pop_Id, AgeAccelResiduals))




ggplot(data_difference_telomere,
       aes(x = AgeAccelResiduals,
           y = Difference_telo,
           label = Pop_Id)) +
  geom_point()+
  geom_text(hjust=1, vjust=0)


# variation de AgeAccelResiduals ------------------------------------------

data_difference_DNAm = dplyr::select(data_antler, Pop_Id, Year, Antler_std, AgeAccelResiduals) %>% 
  na.omit() %>% 
  group_by(Pop_Id) %>% 
  group_map(.f = function(tableau, groupe){
    if(nrow(tableau) == 1){
      return(NULL)
    }
    else
      tibble(difference_DNAm = arrange(tableau, Year) %>% # On trie le tableau par annee
               pull(AgeAccelResiduals) %>%  # On extrait la colonne RTL
               diff(),
             difference_DNAm_relative = difference_DNAm /(arrange(tableau, Year) %>%
                                                            slice(1) %>% # On trie le tableau par annee
                                                            pull(AgeAccelResiduals)),
             Pop_Id = groupe$Pop_Id) %>% 
      return() # On sort les différences
  }) %>% 
  bind_rows() %>% # Pour aggréger la liste de tableau
  left_join(dplyr::filter(data_antler, Year == 2016))

ggplot(data_difference_DNAm,
       aes(x = Antler_std,
           y = difference_DNAm,
           color = AgeClass,
           size = Weight,
           label = Pop_Id)) +
  geom_point()+
  geom_text(hjust=1, vjust=0)+
  labs( y = "AgeAccel(N+1) - AgeAccel(N)")


ggplot(data_difference_DNAm,
       aes(x = InvessResiduals,
           y = difference_DNAm,
           color = Cohort_Type,
           size = Weight,
           label = Pop_Id)) +
  geom_point()+
  geom_text(hjust=1, vjust=0)+
  labs( y = "AgeAccel(N+1) - AgeAccel(N)")

lm(difference_DNAm ~ InvessResiduals  + Cohort_Type + Weight, data = data_difference_DNAm) %>% 
  summary()
