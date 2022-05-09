
data_antler = read_excel("data/Dataset_ODIN_160422.xlsx", skip = 0, na = "NA") %>% 
  mutate(PlateNumber = as.factor(PlateNumber), 
         Batch_number = as.factor(Batch_number),
         ProblemDNA_Concentration = as.factor(ProblemDNA_Concentration),
         PlateTelomere  = as.factor(PlateTelomere),
         DNAmAgeLOO = as.numeric(DNAmAgeLOO),
         `AgeAccelLOO(ComputedUCLA)` = as.numeric(`AgeAccelLOO(ComputedUCLA)`),
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
  rename(Year = YearCapture,
         Day = JulianCaptureDate,
         Cohort_Type = `Cohort_Quality_Type(Good/Bad)`,
         DNAmAge = DNAmAgeLOO,
         AgeAccelLOO = `AgeAccelLOO(ComputedUCLA)`,
         Weight = WeightAnimal.kg,
         ProblemDNA  = ProblemDNA_Concentration,
         RTL = QC_RTL,
         Cohort_Quality_Pop = `CohortQuality/pop`)


data_antler = data_antler[,c("Pop_Id", "Year", "Day", "Cohort", "Cohort_Type", "Cohort_Quality_Pop", "Population",
                             "DNAmAge", "AgeAccelLOO", "RTL",
                             "ProblemDNA", 
                             "Age", "AgeClass", "Age_2", "Age_log",
                             "Weight",   "AntlerLength", "AntlerType") ]
