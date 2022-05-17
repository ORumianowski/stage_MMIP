date_day = function(date){
  
  if (format(date, format = "%Y")=="2016"){
    startdate <- as.Date("01/12/2015","%d/%m/%Y")
    new_date = difftime(date,startdate ,units="days") %>% 
      as.integer()
    return(new_date)
  }
  
  if (format(date, format = "%Y")=="2017"){
    startdate <- as.Date("01/12/2016","%d/%m/%Y")
    new_date = difftime(date,startdate ,units="days")%>% 
      as.integer()
    return(new_date)
  }
}

data_antler = read_excel("data/Dataset_Telomere_Bois040522.xlsx", skip = 0, na = "NA") %>% 
  rename(RTL = QC_RTL,
         Population = population,
         AntlerType = State,
         Tbars = tbars,
         Id = Id_JM,
         Weight = Masse) %>% 
  mutate(Id = as.factor(Id),
         Year = as.factor(Year),
         Cohort = as.factor(Cohorte),
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
                                     return(max(x, y))
                                 }
         ),
         Day = map_dbl(DateCapture, date_day),
         AgeClass = cut(Age, breaks = c(0,1,4,8,25))) %>% 
  dplyr::select(Id, Year, Day, Population, Weight,
                Cohort, Sex, Age, Tbars, RTL, AntlerLength, AntlerType, AgeClass) 


