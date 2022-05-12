rm(list = ls()) # nettoyage de l'environnement de travail

source("utils_packages.R")
set.seed(433) # Pour la reproductibilité
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


# Inference Bayesienne ----------------------------------------------------

T_ = 50 #runif(1, min = 1, max = 366)
tau = 100 #runif(1, min = 1, max = 366)

data_baye = data_exo %>% 
  mutate(moult_score_type = ifelse(Moult_score==0, "0", "01")) %>% 
  mutate(moult_score_type = ifelse(Moult_score==1, "1", moult_score_type))


L_Y_sachant_tau_T_  = function(data_, T_, tau, esp){
  
  data_0 = subset(data_, Moult_score == 0)
  data_1 = subset(data_, Moult_score == 1)
  data_01 = subset(data_, Moult_score > 0 & Moult_score < 1)
  
  support = 1
  if (nrow(data_0)>0){
    support = support * (T_ > max(data_0$Date)) * (T_ < min(data_01$Date))
  }
  if (nrow(data_01)>0){
    support = support * (tau > max(data_01$Date)-min(data_01$Date))
  }
  if (nrow(data_1)>0 & nrow(data_0)>0){
    support = support * (tau < min(data_1$Date)-max(data_0$Date))
  }
  
  calcul_terme_n = function(score_, date_) {
    expected_score = (1 / tau) * (date_ - T_)
    alpha = f(esp, expected_score)
    beta = g(esp, expected_score)
    return(dbeta(score_, alpha, beta))
    }
   

   calcul_terme_global =  function(data_01_){
     prod(map2_dbl(data_01_$Moult_score, data_01_$Date, calcul_terme_n))
   } 


  if(support==1){
    return(calcul_terme_global(data_01))
  }
  else{
    return(0)
  }
  
} 

L_Y_sachant_tau_T_ (data_exo, T_, tau, esp=0.02)


get_a_sample = function(T_, tau){
  condition = FALSE
  while(!condition){
    y = L_Y_sachant_tau_T_ (data_exo, T_, tau, esp=0.02)
    u = runif(1)
    condition = u <= L_Y_sachant_tau_T_ (data_exo, T_, tau, esp=0.02)
  }
  return(y)
}

get_a_sample(50, 100)


newdata = expand.grid(T_= seq(48, 52, length.out = 100),
                     tau = seq(95, 105, length.out = 100)) 

prediction_grille = map2_dbl(newdata$T_, newdata$tau, get_a_sample)
newdata %>% 
  mutate(pred = prediction_grille) %>%  
  ggplot(aes(x = T_, y = tau, fill = pred)) + 
  geom_raster() +
  scale_fill_viridis_c()



# méthode acceptation-rejet -----------------------------------------------



data_0 = subset(data_exo, Moult_score == 0)
data_1 = subset(data_exo, Moult_score == 1)
data_01 = subset(data_exo, Moult_score > 0 & Moult_score < 1)

min_T_ = max(data_0$Date)
max_T_ = min(data_01$Date)

min_tau = max(data_01$Date)-min(data_01$Date)
max_tau = 366

f_ = function(data_, T_, tau, esp=0.02){
  
  data_01 = subset(data_, Moult_score > 0 & Moult_score < 1)
  
  calcul_terme_n = function(score_, date_) {
    expected_score = (1 / tau) * (date_ - T_)
    alpha = f(esp, expected_score)
    beta = g(esp, expected_score)
    return(log(dbeta(score_, alpha, beta)))
  }
  
  calcul_terme_global =  function(data_01_){
    sum(map2_dbl(data_01_$Moult_score, data_01_$Date, calcul_terme_n))
  } 
  
  calcul_terme_global(data_01)

}


f_(data_exo, T_=50, tau=100)




  
g_ = function(data_, T_, tau){
  
  data_0 = subset(data_, Moult_score == 0)
  data_1 = subset(data_, Moult_score == 1)
  data_01 = subset(data_, Moult_score > 0 & Moult_score < 1)
  
  min_T_ = max(data_0$Date)
  max_T_ = min(data_01$Date)
  
  min_tau = max(data_01$Date)-min(data_01$Date)
  max_tau = 366
  
  terme1 = (1/(max_T_-min_T_)) * (min_T_ < T_ & T_ < max_T_) 
  terme2 = (1/(max_tau-min_tau)) * (min_tau < tau & tau < max_tau) 
  
  return(log(terme1*terme2))
}

g_(data_exo, T_=50, tau=100)

to_optim = function(pars, data_=data_exo){
  T_ = pars[1]
  tau = pars[2]
  return(-f_(data_, T_, tau)-g_(data_, T_, tau))
}


res_optim = optim(par = c(50,120) , fn = to_optim )

res_optim$par
res_optim$value

M = (-res_optim$value) %>% 
  exp()
M

get_a_sample = function(M_=M, data_=data_exo){
  condition = FALSE
  while(!condition){
    T_ = runif(1, min = min_T_ , max = max_T_)
    tau = runif(1, min = min_tau , max = max_tau)
    
    log_y = g_(data_, T_, tau)
    log_u = runif(1) %>% 
      log()
    log_u <= f_(data_, T_=50, tau=100) - (log(M_)*g_(data_, T_, tau))
  }
  return(y)
}

get_a_sample(T_ = 50, tau = 100)













