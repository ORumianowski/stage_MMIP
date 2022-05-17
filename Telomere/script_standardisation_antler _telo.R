# Problem with the sample
data_antler = data_antler[-which(data_antler$Id=="M2301" & data_antler$Year=="2016") , ]

# Unmeasured Antler Length
data_antler = data_antler[-which(data_antler$Id=="M2317" & data_antler$Year=="2016") , ]


data_antler_used_std =data_antler[,c("Id", 
                                     "Year",
                                     "Day",
                                     "AntlerLength",
                                     "AntlerType",
                                     "AgeClass",
                                     "Population")] %>% 
  subset(AntlerLength > 1)
                              


# Fonctions de standardisation --------------------------------------------

DATE_REF = 60

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


n = 1000
X = abs(rnorm(n))
Y = lineaire(c(2, 5, 1), X) + rnorm(length(X), sd = .5)


# CHIZE -------------------------------------------------------------------

# Premiere classe ---------------------------------------------------------

data_antler_2 = subset(data_antler_used_std, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "C")

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
    ))

data_antler_1C = data_antler_2 %>% 
  mutate(Antler_std = std_antler(log(AntlerLength), Day, optim_output$par) %>% 
           exp()) %>% 
  dplyr::select(Id, Population, Year, Antler_std)
  


# Classes supérieures -----------------------------------------------------


data_antler_2 = subset(data_antler_used_std, AntlerType == "BV"| is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "C") 

X = data_antler_2$Day
Y = data_antler_2$AntlerLength %>%
  log()


data_antler_NC = data_antler_2 %>% 
  mutate(Antler_std = AntlerLength) %>% 
  dplyr::select(Id, Population, Year, Antler_std)


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

data_antler_1TF = data_antler_2 %>% 
  mutate(Antler_std = std_antler(log(AntlerLength), Day, optim_output$par) %>% 
           exp()) %>% 
  dplyr::select(Id, Population, Year, Antler_std)


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

data_antler_NTF = data_antler_2 %>% 
  mutate(Antler_std = std_antler(log(AntlerLength), Day, optim_output$par) %>% 
           exp()) %>% 
  dplyr::select(Id, Population, Year, Antler_std)



# merge -------------------------------------------------------------------

data_antler_std = rbind(data_antler_1C,
                        data_antler_NC,
                        data_antler_1TF,
                        data_antler_NTF)

data_antler_complet <-  merge(data_antler_used_std, data_antler_std, by=c("Id", "Population","Year"), all.x = TRUE)


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

data_antler_complet <- data_antler_complet[,c("Id", "Year", "Population", "Antler_std")]
data_antler <-  merge(data_antler, data_antler_complet, by=c("Id", "Year", "Population"), all.x = TRUE)


rm()
rm(k)
rm(n)
rm(X)
rm(Y)
rm(DATE_REF)
rm(optim_output)
rm(constante)
rm(date_day)
rm(get_AIC)
rm(lineaire)
rm(NLL)
rm(std_antler)
rm(data_antler_1C)
rm(data_antler_NC)
rm(data_antler_1TF)
rm(data_antler_NTF)
rm(data_antler_std)
rm(data_antler_complet)

rm(data_antler_2)
rm(data_antler_used_std)


