rm(list = ls()) # nettoyage de l'environnement de travail

source("utils_packages.R")
source("script_dataset_simule.R")

data_plot = subset(data_simul)

ggplot(data_plot,
       aes(x = Date,
           y = Moult_score,
           color=Annee)) +
  geom_point()


res_moult = moult( data_simul$Moult_score ~ data_simul$Date | 1 
                   
                   | data_simul$Annee,
                   
                   type = 5,
                   prec = 0.01)


res_moult %>% 
  summary()

print(tau)
print(mu)