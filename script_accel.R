rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")
source("script_pretraitement_data_antler.R")
source("script_standardisation_antler.R")
source("script_determination_ageaccel.R")


# modele complet sur population entiere -----------------------------------



data_antler_complet = dplyr::select(data_antler, 
                                    Pop_Id, Year, Day, 
                                    Cohort, Cohort_Type, Population, 
                                    Age, Age_2, Age_log, AgeClass,
                                    Weight, AntlerLength, 
                                    DNAmAge, AgeAccelLOO, ProblemDNA,
                                    Antler_std, AgeAccelResiduals) %>% 
  subset(Antler_std>0) %>% 
  mutate(Antler_std_log = log(Antler_std),
         Weight_log = log(Weight)) %>% 
  na.omit()

reg_lm_full <- lmer(AgeAccelResiduals ~ Antler_std_log + Weight_log +
                      Age + AgeClass +
                      Population+ Cohort_Type +
                      Antler_std_log:Population + Antler_std_log:Weight_log +
                      (1|Cohort),  
                    data=data_antler_complet)


options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)

par(mar = c(3,5,6,4))
plot(ms_full, labAsExpr = TRUE)


# modele complet sur les premieres annees ---------------------------------


data_antler_1A = dplyr::select(data_antler, 
                               Pop_Id, Year, Day, 
                               Cohort, Cohort_Type, Population, 
                               Age, Age_2, Age_log, AgeClass,
                               Weight, AntlerLength, 
                               DNAmAge, AgeAccelLOO, ProblemDNA,
                               Antler_std, AgeAccelResiduals) %>% 
  subset(Antler_std>0) %>% 
  subset(Age<1) %>% 
  mutate(Antler_std_log = log(Antler_std),
         Weight_log = log(Weight)) %>% 
  na.omit()


reg_lm_1A <- lm(AgeAccelResiduals ~ Antler_std_log + Population + Weight_log +
                  Antler_std_log:Population + Antler_std_log:Weight_log,  
                data=data_antler_1A)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

par(mar = c(3,5,6,4))
plot(ms1A, labAsExpr = TRUE)



# Les premieres annees avec un point en moins --------------------------------------------

data_antler_1A = dplyr::select(data_antler, 
                               Pop_Id, Year, Day, 
                               Cohort, Cohort_Type, Population, 
                               Age, Age_2, Age_log, AgeClass,
                               Weight, AntlerLength, 
                               DNAmAge, AgeAccelLOO, ProblemDNA,
                               Antler_std, AgeAccelResiduals) %>% 
  subset(Antler_std>=1) %>% 
  subset(Age<1) %>% 
  mutate(Antler_std_log = log(Antler_std),
         Weight_log = log(Weight)) %>% 
  na.omit()


reg_lm_1A <- lm(AgeAccelResiduals ~ Antler_std_log + Population + Weight_log +
                  Antler_std_log:Population + Antler_std_log:Weight_log,  
                data=data_antler_1A)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

par(mar = c(3,5,6,4))
plot(ms1A, labAsExpr = TRUE)

reg_lm_1 <- lm(AgeAccelResiduals ~ Antler_std_log , 
                data=data_antler_1A)


reg_lm_1 %>% 
  summary()

ggplot(data_antler_1A,
       aes(x = Antler_std_log,
           y = AgeAccelResiduals)) +
  geom_point()


# Exploration -------------------------------------------------------------



data_antler_complet = dplyr::select(data_antler, 
                                    Pop_Id, Year, Day, 
                                    Cohort, Cohort_Type, Population, 
                                    Age, Age_2, Age_log, AgeClass,
                                    Weight, AntlerLength, 
                                    DNAmAge, AgeAccelLOO, ProblemDNA,
                                    Antler_std, AgeAccelResiduals) %>% 
  subset(Antler_std>0) %>% 
  mutate(Antler_std_log = log(Antler_std),
         Weight_log = log(Weight)) %>% 
  na.omit()

reg_lm_full <- lmer(AgeAccelResiduals ~ Antler_std_log + Weight_log +
                      Age + AgeClass +
                      Population + Cohort_Type +
                      Antler_std_log:Population + Antler_std_log:Weight_log + Antler_std_log:Cohort_Type + Antler_std_log:AgeClass+
                      Weight_log:Population + Weight_log:Weight_log + Weight_log:Cohort_Type + Weight_log:AgeClass+
                      (1|Cohort),  
                    data=data_antler_complet)


options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)

par(mar = c(3,5,6,4))
plot(ms_full, labAsExpr = TRUE)

