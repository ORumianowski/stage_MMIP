rm(list = ls()) # nettoyage de l'environnement de travail

source("utils_packages.R")
source("script_dataset_simule.R")

ggplot(data_simul,
       aes(x = Date,
           y = Moult_score,
           color = Annee)) +
  geom_point()


f = function(esp, y){
  (1/esp**2)*(y^2*(1-y)-esp^2*y)
}

g = function(esp, y){
  (1/esp**2)*(y*(1-y)^2+esp^2*(y-1))
}

data_exo = dplyr::select(data_simul, Moult_score, Date, Annee)

nb_annee = data_exo$Annee %>%
  unique() %>% 
  length()
  


NegLogLikelihood = function(par, data_=data_simul, esp=0.02){
  
  tau = par[1]
  T_ = par[2]
  coef_annee = par[3:3+nb_annee-1]
  
  L = 1
  A = length(coef_annee)
  
  for (a in 1:A){
    
    data_annee_a = subset(data_, Annee==a)
    
    Ta = T_ + coef_annee[a]
  
      #determination de la composante des oiseaux avant la mue
      data_0 = subset(data_annee_a, Moult_score==0)
      M = nrow(data_0)
      
      terme_0 = 1
      
      if (M > 0){
        for (m in 1:M){
          terme_0 = terme_0 * (data_0$Date[m] < Ta)
        }
      }
    
      
      
      #determination de la composante des oiseaux après la mue
      data_1 = subset(data_annee_a, Moult_score==1)
      P = nrow(data_1)
      
      terme_1 = 1
      if (P > 0){
        for (p in 1:P){
          terme_1 = terme_1 * (data_1$Date[p] > Ta+ tau)  
        }
      }
      
      #determination de la composante des oiseaux pendant la mue
      data_01 = subset(data_annee_a, Moult_score > 0 & Moult_score < 1)
      N = nrow(data_01)
      
      terme_01 = 1
      for (n in 1:N){
        alpha = f(esp, data_01$Moult_score[n])
        beta = g(esp, data_01$Moult_score[n])
        terme_01 = terme_01*(data_01$Date[n] > Ta)*(data_01$Date[n] < Ta+ tau)*dbeta( (1/tau)*(data_01$Date[n]-Ta), alpha, beta, ncp = 0, log = FALSE)
        
      }
      print(terme_0)
      print(terme_1)
      print(terme_01)
      La = terme_0*terme_1*terme_01
      
      L = L*La
  
  }
  
  return((-1)*log(L)) 


  }


NegLogLikelihood(data_=data_exo, rep(0, times=nb_annee), esp=0.02)




init_par = c(100, 50, rep(0, times=nb_annee))

res_optim = optim(
  par = init_par,
  fn = NegLogLikelihood
)

##a priori, il n'y a aucune methodes d'estimation qui permet d'avoir un lower et qui gère les infinis


res_optim$par

