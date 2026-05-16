#Reloading necessary libraries 
library(tidyverse)

###

#Reformatting 'SYNDROME' and 'AGEGRP' so that the values don't have spaces 
  
  #All values of 'SYNDROME' and 'AGEGRP' variable
  unique(combined_condns2$SYNDROME)
  unique(combined_condns2$AGEGRP)
  
  #SYNDROME 
  combined_condns2 <- combined_condns2 %>% 
    mutate(SYNDROME = if_else(combined_condns2$SYNDROME == 'Medically attended diarrhea - inpatient', "Inpatient", combined_condns2$SYNDROME))
  combined_condns2 <- combined_condns2 %>% 
    mutate(SYNDROME = if_else(combined_condns2$SYNDROME == 'Medically attended diarrhea - outpatient', "Outpatient", combined_condns2$SYNDROME))
  combined_condns2 <- combined_condns2 %>% 
    mutate(SYNDROME = if_else(combined_condns2$SYNDROME == 'Community detected diarrhea', "Community-detected", combined_condns2$SYNDROME))  

  #AGEGRP
  combined_condns2 <- combined_condns2 %>% mutate(AGEGRP = if_else(combined_condns2$AGEGRP == 'Pre-school age children', 'PreschoolAgeChildren', combined_condns2$AGEGRP))
  combined_condns2 <- combined_condns2 %>% mutate(AGEGRP = if_else(combined_condns2$AGEGRP == 'Combined ages', 'Combined', combined_condns2$AGEGRP))
  combined_condns2 <- combined_condns2 %>% mutate(AGEGRP = if_else(combined_condns2$AGEGRP == 'School age children', 'SchoolAgeChildren', combined_condns2$AGEGRP))
  
  
#Dropping 'Notes' column
combined_condns2$NOTES <- NULL

### 

#Reformatting Age Range
library(dplyr)

combined_condns2$AGERANGE <- if_else(combined_condns2$AgeLBMon >= 0 & combined_condns2$AgeUBMon < 60, "<5yr", ">= 5yr")
#
