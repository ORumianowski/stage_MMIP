rm(list = ls()) # nettoyage de l'environnement de travail
source("utils_packages.R")
source("script_pretraitement_data_antler.R")
source("script_standardisation_antler.R")
source("script_determination_ageaccel.R")

vis_miss(data_antler)

data_antler_complet = dplyr::select(data_antler, 
                                    Pop_Id, Year, Day, 
                                    Cohort, Cohort_Type, Cohort_Quality_Pop, Population, 
                                    Age, Age_2, Age_log, AgeClass,
                                    Weight, AntlerLength, InvessResiduals,
                                    DNAmAge, AgeAccelLOO, ProblemDNA,
                                    Antler_std, AgeAccelResiduals) %>% 
  subset(Antler_std>=1) %>% 
  mutate(Cohort_Type = factor(Cohort_Type,
                              levels = c("B", "G"))) %>% # Good est le niveau "succes" 
  mutate(Cohort = as.factor(Cohort)) %>% 
  mutate(Antler_std_log = log(Antler_std),
         Weight_log = log(Weight)) %>% 
  na.omit()

reg_lm_full <- lm(Cohort_Quality_Pop ~ scale(Antler_std_log) + scale(Weight_log) +
                       Population +
                      InvessResiduals + AgeAccelResiduals +
                        scale(Antler_std_log):Population + scale(Weight_log):Population + scale(Antler_std_log):scale(Weight_log),
                    data=data_antler_complet)


options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)

par(mar = c(3,5,6,4))
plot(ms_full, labAsExpr = TRUE)


reg_lm_full_1 <- lm(scale(Cohort_Quality_Pop) ~ scale(Weight_log) + Population,  
                     data=data_antler_complet)
reg_lm_full_1 %>% 
  summary()

reg_lm_full_2 <- lm(scale(Cohort_Quality_Pop) ~ scale(Weight_log) + Population +
                      + scale(Weight_log):Population ,  
                    data=data_antler_complet)
reg_lm_full_2 %>% 
  summary()



ggplot(data_antler_complet,
              aes(x = scale(Weight_log),
                  y = scale(Cohort_Quality_Pop),
                  color=Population)) +
  geom_point()




predict(reg_lm_full)

modele_glm_fixe = data_antler_complet %>% 
  select(Antler_std_log, Weight_log, Cohort_Type, Cohort) %>% 
  mutate_if(is.numeric, function(x) (x - mean(x)) / sd(x)) %>% 
  as.data.frame() %>% 
  glmer(Cohort_Type ~ Antler_std_log + Weight_log + Antler_std_log:Weight_log + 
        + (1|Cohort),
      family = binomial, data = .) 

newdata = expand.grid(Antler_std_log = seq(-5, 5, length.out = 101),
                      Weight_log = seq(-5, 5, length.out = 101),
                      Cohort = unique(data_antler_complet$Cohort)) 
prediction_grille = predict(modele_glm_fixe, newdata = newdata, type = "response")
newdata %>% 
  mutate(pred = prediction_grille) %>%  
  ggplot(aes(x = Antler_std_log, y = Weight_log, fill = pred)) + 
  geom_raster() +
  facet_wrap(~ Cohort) +
  scale_fill_viridis_c()


ggplot(data_antler_complet,
       aes(x = scale(Antler_std_log),
           y = scale(Weight_log),
           color=Cohort_Type)) +
  geom_point()


data_antler_ACP = dplyr::select(data_antler_complet, 
                                    Cohort, Cohort_Type, Cohort_Quality_Pop,
                                    Weight,
                                    Antler_std) %>% 
  na.omit()





resPCA <- PCA(data_antler_ACP[, c("Weight", "Antler_std")], scale.unit = TRUE, ncp = 6, graph = FALSE)


fviz_pca_ind(resPCA, col.ind = factor(data_antler_ACP$Cohort_Type),axes=1:2)


data_antler_complet <- mutate(data_antler_complet, 
  Taille=resPCA$ind$coord[,1],
  InvessACP=resPCA$ind$coord[,2])

ggplot(data_antler_complet,
       aes(x = Taille,
           y = InvessACP,
           color=Cohort_Type)) +
  geom_point()




reg_lm_full <- glm(Cohort_Type ~ Taille ,  family= binomial,
                     data=data_antler_complet) 


reg_lm_full %>% 
  summary()


reg_lm_full <- lm(Cohort_Quality_Pop ~ Taille,
                     data=data_antler_complet) 

reg_lm_full %>% 
  summary()

