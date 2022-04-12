rm(list = ls()) # nettoyage de l'environnement de travail


# Chargement du jeu de données --------------------------------------------

library(readxl) # pour charger les fichiers excel
library(tidyr)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)
library(lme4)

data_JFM18 = read_excel("data/Dataset_JAE-201700618.xls", skip = 2, na = "NA") %>% 
  rename(pop = `Population  (CH=Chizé / TF=Trois Fontaines)`,
         survival6 = `Probability of surviving beyond 6 years of age (0= no / 1=yes)`,
         mass20 = `Mass at 20 months (Kg)`,
         antler20 = `Antlers length at 20 months of age(mm)- values standardized by the Julian date [see methods`,
         cohort = Cohort) %>% 
  mutate(survival6 = as.factor(survival6), 
         mass20 = as.numeric(mass20),
         antler20 = as.numeric(antler20),
         pop = as.factor(pop))


## Reproduction du GLM: surviving beyond 6 years ~ relative antler length + body mass 

# sélection des colonnes nécessaires 

data_antler20_survival6 = select(data_JFM18, 
       survival6, mass20, antler20, cohort, pop) %>% 
  mutate(cohort = as.factor(cohort))  %>% # Mise en facteur pour un potentiel effet mixte
  mutate(antler20_log = log(antler20),
         mass20_log = log(mass20))


# problem NA --------------------------------------------------------------

na.omit(data_antler20_survival6) %>% 
  summary()


# Regression --------------------------------------------------------------


 
reglmer <- glmer(survival6 ~ antler20_log + mass20_log + pop + 
                   antler20_log:pop + 
                   (1 | cohort), 
                 family= binomial, 
                 data=data_antler20_survival6,
                 control = glmerControl(optCtrl = list(maxfun = 1e5)))



reg_glm =  glm(survival6 ~ antler20_log + mass20_log + pop + 
                   antler20_log:pop, 
                 family= binomial, 
                 data= data_antler20_survival6)

reg_glm %>% 
  summary()

reglmer %>% 
  summary()

isSingular(reglmer)



# Graphiques associés -----------------------------------------------------



#creation X

x = 30*(1.05)**(0:50) 
bornes = c(30, 260)

groupe = findInterval(x, bornes)
X = x[groupe==1]



data_graph = select(data_JFM18, 
                             survival6,  antler20, pop) %>% 
  na.omit() 

data_graph$group = NA

data_graph = subset(data_graph, pop == "CH" ) 




for ( i in 1:length(data_graph$antler20)){
  
  for (k in 1:(length(X)-1) ){
    
    if ( X[k] < data_graph$antler20[i] & data_graph$antler20[i] < X[k+1] ){
      
      data_graph$group[i] =  (X[k]+X[k+1]) /2
    }
  }
}

data_graph$survival6 = data_graph$survival6 %>% 
  as.character() %>% 
  as.numeric()

group_count_alive <- aggregate(survival6 ~ group, data = data_graph, FUN=sum)
group_effectif <-  aggregate(survival6 ~ group, data = data_graph, FUN=length)

data_graph2 = tibble(group = group_count_alive$group,
                     nb_alive = group_count_alive$survival6,
                     effectif = group_effectif$survival6)

data_graph2$proportion = data_graph2$nb_alive / data_graph2$effectif 


graphe <- ggplot(data_graph2, aes(group,  proportion, size=effectif)) + 
  geom_point() +
  
  labs(
    title = "Chizé",
    x= "Standardized antler length
    20 months old (mm)",
    y= "Survival to 6 years of age"
  ) +
  coord_trans(x="log2")

print(graphe)
