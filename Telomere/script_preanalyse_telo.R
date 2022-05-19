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
                          Antler_std, Id, Tbars, Weight, AgeClass, Cohort_Quality_Pop, Population, Age))




ggplot(data_difference_telomere,
       aes(x = Antler_std,
           y = Difference_telo,
           color = AgeClass)) +
  geom_point()

ggplot(data_difference_telomere,
       aes(x = Tbars,
           y = Difference_telo,
           color = AgeClass)) +
  geom_point()


ggplot(data_difference_telomere,
       aes(x = Weight,
           y = Difference_telo,
           color = AgeClass)) +
  geom_point()

# glm RTL ---------------------------------------------------------------------


data_antler_lm = dplyr::select(data_antler, RTL, Population , AgeClass, Age,  Weight , Antler_std, Cohort_Quality_Pop, Cohort, Tbars) %>% 
  na.omit()

reg_lm_full = lmer(RTL ~ Age + AgeClass + Weight + Antler_std + Population + Cohort_Quality_Pop+ Tbars+
                   Antler_std:AgeClass +   Antler_std:Weight + Antler_std:Population +  Antler_std:Cohort_Quality_Pop+
                   Weight:AgeClass + Weight:Population +  Weight:Cohort_Quality_Pop +
                     (1|Cohort), 
                 data = data_antler_lm) 

options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)


reg_lm_1 = lm(RTL ~ Population, data = data_antler_lm) 
reg_lm_2 = lm(RTL ~ Population + Antler_std, data = data_antler_lm) 

reg_lm_1 %>% 
  summary()

reg_lm_2 %>% 
  summary()

x = data_antler_lm$RTL[data_antler_lm$Population=="C"]
y = data_antler_lm$RTL[data_antler_lm$Population=="TF"]

t.test(x, y)

anova(reg_lm_1, reg_lm_2)



# glm shortening ---------------------------------------------------------------------


data_antler_lm = data_difference_telomere %>% 
  na.omit()

reg_lm_full = lm(Difference_telo ~  Age + AgeClass + Weight + Antler_std + Population + Cohort_Quality_Pop+ Tbars+
                   Antler_std:AgeClass +   Antler_std:Weight + Antler_std:Population +  Antler_std:Cohort_Quality_Pop+
                   Weight:AgeClass + Weight:Population +  Weight:Cohort_Quality_Pop, 
                 data = data_antler_lm) 

options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)

par(mar = c(3,5,6,4))
plot(ms_full, labAsExpr = TRUE)


# Cohort ------------------------------------------------------------------

ggplot(data_antler,
       aes(x = Cohort_Quality_Pop ,
           y = Antler_std,
           color=AgeClass)) +
  geom_point()

data_antler_lm = dplyr::select(data_antler, RTL, Population , AgeClass, Weight , Antler_std, Cohort_Quality_Pop) %>% 
  na.omit()

reg_lm_full = lm(Cohort_Quality_Pop ~  Weight + Antler_std + RTL +
                   Weight:Antler_std + Weight:RTL +
                   Antler_std:RTL,
                 data = data_antler_lm) 

options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)

par(mar = c(3,5,6,4))
plot(ms_full, labAsExpr = TRUE)

reg_lm_1 = lm(Cohort_Quality_Pop ~ Weight, data = data_antler_lm) 
reg_lm_2 = lm(Cohort_Quality_Pop ~ Antler_std, data = data_antler_lm) 

reg_lm_1 %>% 
  summary()

reg_lm_2 %>% 
  summary()


reg_lm_1 %>% 
  car::Anova()


anova(reg_lm_1, reg_lm_2)



