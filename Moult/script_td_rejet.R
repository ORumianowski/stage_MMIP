rm(list = ls()) # nettoyage de l'environnement de travail

source("utils_packages.R")

lambda = 1/5

f_X = function(k, lambda_=lambda){
  (exp(-lambda_)*lambda_^(k))/factorial(k)
}

f_Y = function(k, lambda_=lambda){
  (1-lambda_)*lambda_^(k-1)
}

alpha = function(f,g,M,y){
  f(y) / (M*g(y))
}


get_a_sample = function(lambda_=lambda, f_X_=f_X, f_Y_=f_Y, alpha_=alpha){
  condition = FALSE
  while(!condition){
    y = rgeom(1, prob = lambda_)
    u = runif(1)
    M =  exp(-lambda) / (1 - lambda)
    condition = u <= alpha(f_X_, f_Y_, M, y)
    
  }
  return(y)
}

n = 1000

sample = rerun(n, get_a_sample()) %>% 
  unlist() %>% 
  tibble(n = 1:n, x = .) 


ggplot(sample, aes(x=x)) + 
  geom_histogram(color="black", fill="white", bins = 30)
