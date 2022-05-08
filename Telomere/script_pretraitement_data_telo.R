data_antler = read_excel("Dataset_Telomere_Bois040522.xlsx", skip = 0, na = "NA") %>% 
  rename(RTL = QC_RTL,
         Population = population,
         Tbars = tbars,
         Id = Id_JM_Pop,
         Weight = Masse) %>% 
  dplyr::select(Id, Year, DateCapture, Cohorte, Population, Sex, Age, Weight, Tbars, RTL, Antler_right, Antler_left, State) %>% 
  mutate(Id = as.factor(Id),
        Year = as.factor(Year),
        Cohorte = as.factor(Cohorte),
        Population = as.factor(Population),
        Sex = as.factor(Sex),
        State = as.factor(State),
        AgeClass = cut(Age, breaks = c(0,1,4,8,25)))