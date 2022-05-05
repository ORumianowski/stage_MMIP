rm(list = ls()) # nettoyage de l'environnement de travail

source("utils_packages.R")
source("script_dataset_simule.R")


f = function(esp, y){
  (1/esp**2)*(y**2*(1-y)-esp**2*y)
}

g = function(esp, y){
  (1/esp**2)*(y*(1-y)**2+esp**2*(y-1))
}





Likelihood = function(data_, T_, tau, esp=0.02){
  
  #determination de la composante des oiseaux avant la mue
  data_0 = subset(data_, Moult_score==0)
  M = nrow(data_0)
  
  terme_0 = 1
  for (m in 1:M){
    terme_0 = terme_0 * (max(data_0$Date) < T_)
  }
    
  #determination de la composante des oiseaux après la mue
  data_1 = subset(data_, Moult_score==1)
  P = nrow(data_1)
  
  terme_1 = 1
  for (p in 1:P){
    terme_1 = terme_1 * (min(data_1$Date) > T_+ tau)  
  }
  
  #determination de la composante des oiseaux pendant la mue
  data_01 = subset(data_, Moult_score > 0 & Moult_score < 1)
  N = nrow(data_01)
  
  terme_01 = 1
  for (n in 1:N){
    alpha = f(data_$Moult_score[n], esp)
    beta = g(data_$Moult_score[n], esp)
    terme_01 = terme_01*(max(data_0$Date) > T_)*(min(data_1$Date) < T_+ tau)*dbeta( (1/tau)*(data_01$Date-T_), alpha, beta, ncp = 0, log = FALSE)
    
  }
  
  L = terme_0*terme_1*terme_01
  
  return(L)  
}

data_exo = dplyr::select(data_simul, Moult_score, Date)


Likelihood(data_=data_exo, T_=45, tau=90, esp=0.02)

