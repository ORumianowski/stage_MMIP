rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")
source("script_pretraitement_data_telo.R")

data_antler= mutate(data_antler,
                    RTL = scale(RTL))

ggplot(data_antler,
       aes(x = AgeClass,
           y = RTL)) +
  geom_boxplot()

ggplot(data_antler,
       aes(x = Age,
           y = RTL,
           color=Population)) +
  geom_point()



ggplot(data_antler,
       aes(x = Antler_right,
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

# variation de AgeAccelResiduals ------------------------------------------

data_difference_DNAm = dplyr::select(data_antler, Id, Year, Antler_std, AgeAccelResiduals) %>% 
  na.omit() %>% 
  group_by(Id) %>% 
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

lm(RTL ~ Population + AgeClass + Weight + Antler_right, data = data_antler) %>% 
  summary()
