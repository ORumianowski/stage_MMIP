rm(list = ls()) # nettoyage de l'environnement de travail

source("utils_packages.R")
set.seed(133) # Pour la reproductibilité
source("script_dataset_simule.R")

data_exo = subset(data_simul, Annee==1) %>% 
  dplyr::select(Moult_score, Date)


ggplot(data_exo,
       aes(x = Date,
           y = Moult_score)) +
  geom_point()


f = function(esp, y){
  (1/esp**2)*(y^2*(1-y)-esp^2*y)
}

g = function(esp, y){
  (1/esp**2)*(y*(1-y)^2+esp^2*(y-1))
}




NegLogLikelihood = function(par, data_ = data_exo, esp = 0.02) {
  tau = par[1]
  T_ = par[2]
  
  #determination de la composante des oiseaux avant la mue
  data_0 = subset(data_, Moult_score == 0)
  
  if (nrow(data_0) > 0) {
    terme_0 = ifelse(T_ > max(data_0$Date), 0, - Inf)
  }
  else{ 
    terme_0 = 0
  }
  
  #determination de la composante des oiseaux après la mue
  data_1 = subset(data_, Moult_score == 1)
  if (nrow(data_1) > 0) {
    terme_1 = ifelse(T_ + tau < min(data_1$Date))
  }
  else{
    terme_1 = 0
  }
  
  #determination de la composante des oiseaux pendant la mue
  data_01 = subset(data_, Moult_score > 0 &
                     Moult_score < 1)
  calcul_terme_n = function(score_, date_){
    bon_intervalle = log(date_ > T_) + log(date_ < (T_ + tau))
    if(is.infinite(bon_intervalle)){
      return(-Inf)
    }
    else{
      expected_score = (1 / tau) * (date_ - T_)
      alpha = f(esp, expected_score)
      beta = g(esp, expected_score)
      return(dbeta(score_, alpha, beta,log = TRUE))
    }
  }
  terme_01 =  sum(map2_dbl(data_01$Moult_score, data_01$Date, calcul_terme_n))
  L = terme_0 + terme_1 + terme_01
  return((-1) * L)
}

  


NegLogLikelihood(data_=data_exo, c(101, 50), esp=0.02)

res_optim = optim(
  par = c(105, 50),
  fn = NegLogLikelihood,
)

res_optim$par
