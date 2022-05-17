rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")

data_antler_telo = read_excel("data/Dataset_Telomere_Bois040522.xlsx", skip = 0, na = "NA") %>% 
  rename(RTL = QC_RTL,
         Population = population,
         Tbars = tbars,
         Id = Id_JM,
         Cohort = Cohorte,
         AntlerType = State,
         Weight = Masse) %>% 
  mutate(Id = as.factor(Id),
        Year = as.factor(Year),
        Cohort = as.factor(Cohort),
        Population = as.factor(Population),
        Sex = as.factor(Sex),
        AntlerType = as.factor(AntlerType),
        AntlerLength = map2_dbl(.x = Antler_left, 
                                .y = Antler_right,
                                .f = function(x, y){
                                  if(is.na(x))
                                    return(y)
                                  else if(is.na(y))
                                    return(x)
                                  else
                                    return(max(x, y)) }
        ),
        AgeClass = cut(Age, breaks = c(0,1,4,8,25))) %>% 
  dplyr::select(Id, Year, Population, Tbars, RTL, AgeClass, AntlerLength, AntlerType, Cohort, Weight) 

#convertir la date
#recyclage lors du merge



data_antler_1 = read_excel("data/Dataset_ODIN_160422.xlsx", skip = 0, na = "NA") %>% 
  rename(Year = YearCapture,
         Day = JulianCaptureDate,
         Cohort_Type = `Cohort_Quality_Type(Good/Bad)`,
         DNAmAge = DNAmAgeLOO,
         Weight = WeightAnimal.kg,
         ProblemDNA  = ProblemDNA_Concentration,
         RTL = QC_RTL,
         Cohort_Quality_Pop = `CohortQuality/pop`) %>% 
  mutate(Id = as.factor(Id),
         Year = as.factor(Year),
         Pop_Id = as.factor(Pop_Id),
         Population = as.factor(Population),
         PlateTelomere  = as.factor(PlateTelomere),
         DNAmAgeLOO = as.numeric(DNAmAge),
         RightAntlerLength    = str_remove(RightAntlerLength, "_broken") %>% 
           as.numeric(),
         AntlerLength = map2_dbl(.x = Left_AntlerLength, 
                                 .y = RightAntlerLength,
                                 .f = function(x, y){
                                   if(is.na(x))
                                     return(y)
                                   else if(is.na(y))
                                     return(x)
                                   else
                                     return(max(x, y))
                                 }
         ),
         AgeClass = cut(Age, breaks = c(0,1,4,8,25)) %>% 
           as.character(),
         Age_2 = Age**2,
         Age_log = log((Age))) %>% 
  dplyr::select(Id, Year, Day, Cohort, Cohort_Type, Cohort_Quality_Pop, Population,
                DNAmAge,
                ProblemDNA, RTL,
                Age, AgeClass, Age_2, Age_log,
                Weight,   AntlerLength, AntlerType,
                Pop_Id)




data_antler <- merge(data_antler_1, data_antler_telo, 
                     by=c("Id", "Year", "Population", "AgeClass", "RTL", "Weight", "AntlerLength", "Cohort", "AntlerType" ), all=TRUE )
