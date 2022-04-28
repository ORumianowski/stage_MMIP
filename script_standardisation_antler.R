
# Problem with the sample
data_antler = data_antler[-which(data_antler$Pop_Id=="CM2301" & data_antler$Year=="2016") , ]
# Problem with the sample (Female)
data_antler = data_antler[-which(data_antler$Pop_Id=="TM368" & data_antler$Year=="2018") , ]

# Unmeasured Antler Length
data_antler = data_antler[-which(data_antler$Pop_Id=="CM2317" & data_antler$Year=="2016") , ]
data_antler = data_antler[-which(data_antler$Pop_Id=="TM3069" & data_antler$Year=="2018") , ]


data_antler_used_std =data_antler[,c("Pop_Id", 
                                     "Year",
                                     "Day",
                                     "AntlerLength",
                                     "AntlerType",
                                     "AgeClass",
                                     "Population")]
                              


# Fonctions de standardisation --------------------------------------------

DATE_REF = 80

std_antler = function(antler_length, date, pars, date_ref = DATE_REF) {
  a = pars[1]
  b = pars[2]
  (antler_length - (a * date + b))  + a * date_ref + b
}

# Définition des fonctions de modélisation de la longueur des bois --------

# fonction constante

constante = function(pars, x) {
  a = pars[1]
  
  a
}

# fonction linéaire

lineaire = function(pars, x = 0) {
  a = pars[1]
  b = pars[2]
  
  a * x + b
}

# fonction de seuil avec une pente et un plateau

one_slope = function(pars, x) {
  a = pars[1]
  b = pars[2]
  seuil = pars[3]
  
  ifelse(x < seuil, a * x + b, a * seuil + b)
}

# fonction de seuil avec deux pentes

two_slopes = function(pars, x) {
  a = pars[1]
  b = pars[2]
  seuil = pars[3]
  c = pars[4]
  OO_2 = (a - c) * seuil + b
  
  ifelse(test = x < seuil,
         yes = a * x + b,
         no = c * x + OO_2)
}

plan_experience = tibble(
  ma_fonction = c(constante, lineaire, one_slope, two_slopes),
  initial_pars = list(c(0, 1), c(1, 0, 1), c(1, 0, 1, 1), c(1, 0, 0, 2, 1))
)


NLL = function(pars,
               ma_fonction,
               y = Y,
               x = X) {
  # Values predicted by the model
  sigma = pars[length(pars)]
  parametres_moyenne = pars[-length(pars)]
  Gpred = ma_fonction(parametres_moyenne, x)
  # Negative log-likelihood 
  -sum(dnorm(
    x = y,
    mean = Gpred,
    sd = sigma,
    log = TRUE
  ))
}

get_AIC = function(initial_pars,
                   ma_fonction,
                   y = Y,
                   x = X) {
  neg_log_lik = optim(
    par = initial_pars,
    fn = function(p)
      NLL(
        p,
        ma_fonction = ma_fonction,
        y = Y,
        x = X
      ),
    hessian = TRUE
  )$value
  2 * neg_log_lik + 2 * length(initial_pars)
}

#purrr::pmap_dbl(plan_experience,get_AIC)
n = 1000
X = abs(rnorm(n))
Y = lineaire(c(2, 5, 1), X) + rnorm(length(X), sd = .5)



optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)


# Application aux données réelles -----------------------------------------

# CHIZE -------------------------------------------------------------------

# Premiere classe ---------------------------------------------------------

data_antler_2 = subset(data_antler_used_std, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "C" & AntlerLength > 1) # Les bois inf. à 1 sont considérer comme invariants

X = data_antler_2$Day
Y = data_antler_2$AntlerLength %>%
  log()

optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)


data_antler_3 = mutate(data_antler_2,
                       Antler_std = exp(std_antler(
                         log(AntlerLength), Day, optim_output$par
                       )))


data_antler_1C = data_antler_3[, c("Pop_Id", "Year", "Antler_std")]


# Classes supérieures -----------------------------------------------------


data_antler_2 = subset(data_antler_used_std, AntlerType == "BV"| is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "C") 

X = data_antler_2$Day
Y = data_antler_2$AntlerLength %>%
  log()

optim_output = optim(
  par = c(0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = constante,
      y = Y,
      x = X
    )
)




data_antler_3 = mutate(data_antler_2,
                       Antler_std = AntlerLength)


data_antler_NC = data_antler_3[, c("Pop_Id", "Year", "Antler_std")]

# Trois Fontaines ---------------------------------------------------------

# Premiere classe ---------------------------------------------------------

data_antler_2 = subset(data_antler_used_std,AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "TF") 

X = data_antler_2$Day
Y = data_antler_2$AntlerLength %>%
  log()

optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)



data_antler_3 = mutate(data_antler_2,
                       Antler_std = exp(std_antler(
                         log(AntlerLength), Day, optim_output$par
                       )))


data_antler_1TF = data_antler_3[, c("Pop_Id", "Year", "Antler_std")]


# Classes supérieures -----------------------------------------------------


data_antler_2 = subset(data_antler_used_std, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "TF") 

X = data_antler_2$Day
Y = data_antler_2$AntlerLength %>%
  log()
optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)



data_antler_3 = mutate(data_antler_2,
                       Antler_std = exp(std_antler(
                         log(AntlerLength), Day, optim_output$par
                       )))

data_antler_NTF = data_antler_3[, c("Pop_Id", "Year", "Antler_std")]

data_antler_std = rbind(data_antler_1C,
                        data_antler_NC,
                        data_antler_1TF,
                        data_antler_NTF)

data_antler_complet <-  merge(data_antler_used_std, data_antler_std, by=c("Pop_Id", "Year"), all.x = TRUE)


# Traitement des cas Antler_Type==BD  & AntlerLength <= 1 --------------------

for (k in 1:nrow(data_antler_complet)){
  if (!is.na(data_antler_complet[k, "AntlerType"])){
    if (data_antler_complet[k, "AntlerType"] == "BD"){ 
      data_antler_complet[k, "Antler_std"] = data_antler_complet[k, "AntlerLength"]
    }
  }
  if (data_antler_complet[k, "AntlerLength"] <= 1){
    data_antler_complet[k, "Antler_std"] = data_antler_complet[k, "AntlerLength"]
  }
}

data_antler_complet <- data_antler_complet[,c("Pop_Id", "Year", "Antler_std")]
data_antler <-  merge(data_antler, data_antler_complet, by=c("Pop_Id", "Year"), all.x = TRUE)

