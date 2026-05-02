#Reloading necessary libraries 
library(tidyverse)

#Reformatting 'Syndrome' and 'AgeGrp' so that the values don't have spaces 
  
  #All values of 'Syndrome' and 'AgeGrp' variable
  unique(combined_condns$Syndrome)
  unique(combined_condns$AgeGrp)
  
  #Syndrome 
  combined_condns <- combined_condns %>% 
    mutate(Syndrome = if_else(combined_condns$Syndrome == 'Medically attended diarrhea - inpatient', "Inpatient", combined_condns$Syndrome))
  combined_condns <- combined_condns %>% 
    mutate(Syndrome = if_else(combined_condns$Syndrome == 'Medically attended diarrhea - outpatient', "Outpatient", combined_condns$Syndrome))
  combined_condns <- combined_condns %>% 
    mutate(Syndrome = if_else(combined_condns$Syndrome == 'Community detected diarrhea', "Community-detected", combined_condns$Syndrome))  

  #AgeGrp
  combined_condns <- combined_condns %>% mutate(AgeGrp = if_else(combined_condns$AgeGrp == 'Pre-school age children', 'PreschoolAgeChildren', combined_condns$AgeGrp))
  combined_condns <- combined_condns %>% mutate(AgeGrp = if_else(combined_condns$AgeGrp == 'Combined ages', 'Combined', combined_condns$AgeGrp))
  combined_condns <- combined_condns %>% mutate(AgeGrp = if_else(combined_condns$AgeGrp == 'School age children', 'SchoolAgeChildren', combined_condns$AgeGrp))
  
  
#Dropping 'Notes' column
combined_condns$Notes <- NULL