rm(list = ls()) # nettoyage de l'environnement de travail


# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")

dcohort = read_excel("data/Dataset_ODIN_160422.xlsx", skip = 0, na = "NA") %>% 
  rename(Cohort_Quality_Pop = `CohortQuality/pop`) %>% 
  mutate(Population = as.factor(Population),
         Cohort = as.factor(Cohort), 
         Cohort_Quality_Pop = as.numeric(Cohort_Quality_Pop)) %>% 
  dplyr::select(Population, Cohort, Cohort_Quality_Pop) %>% 
  unique()

