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

data_antler_delta = tibble()

data_antler_delta_1 = data_antler[, c("Pop_Id", "Year", "RTL")] %>% 
  na.omit()

for (id in unique(data_antler_delta_1$Pop_Id)){
  
  if (table(data_antler_delta_1$Pop_Id)[id]==2){
    
    data_antler_delta = rbind(data_antler_delta,
                              data_antler_delta_1[data_antler_delta_1$Pop_Id==id, ] ) 
    
  }
}


data_antler_delta_3 = data_antler_delta[, c("Pop_Id")] %>% 
  unique() %>% 
  tibble()

colnames(data_antler_delta_3) = c("Pop_Id")

data_antler_delta_3$var_RTL = NA

data_antler_delta_3$accel_DNAm = NA

for (id in data_antler_delta_3$Pop_Id){
  
  data_antler_delta_3$accel_DNAm[data_antler_delta_3$Pop_Id==id] = 
    as.numeric(data_antler[data_antler$Pop_Id==id & data_antler$Year==2016,"AgeAccelLOO"])
    #sum(data_antler_delta_2[data_antler_delta_2$Pop_Id==id,"AgeAccelLOO"])/2
  data_antler_delta_3$var_RTL[data_antler_delta_3$Pop_Id==id] = as.numeric(
    data_antler[data_antler$Pop_Id==id & data_antler$Year==2017,"RTL"]-
      data_antler[data_antler$Pop_Id==id & data_antler$Year==2016,"RTL"]
  )
}


ggplot(data_antler_delta_3,
       aes(x = accel_DNAm,
           y = var_RTL)) +
  geom_point()

