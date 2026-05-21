#Reloading necessary libraries 
library(tidyverse)
library(lubridate)

###

#Dropping 'Notes' column
combined_condns2$NOTES <- NULL

###

#Reformatting 'SYNDROME' variable
  
  #All values of 'SYNDROME'  variable
  unique(combined_condns2$SYNDROME)
  
  #SYNDROME 
  combined_condns2 <- combined_condns2 %>% 
    mutate(SYNDROME = if_else(combined_condns2$SYNDROME == 'Medically attended diarrhea - inpatient', "03Inpatient", combined_condns2$SYNDROME))
  combined_condns2 <- combined_condns2 %>% 
    mutate(SYNDROME = if_else(combined_condns2$SYNDROME == 'Medically attended diarrhea - outpatient', "02Outpatient", combined_condns2$SYNDROME))
  combined_condns2 <- combined_condns2 %>% 
    mutate(SYNDROME = if_else(combined_condns2$SYNDROME == 'Community detected diarrhea', "01CommunityDetected", combined_condns2$SYNDROME))  
  combined_condns2 <- combined_condns2 %>% 
    mutate(SYNDROME = if_else(combined_condns2$SYNDROME == 'Asymptomatic', "00Asymptomatic", combined_condns2$SYNDROME))  

      #Filtering out 'Mortality'
  combined_condns2 <- combined_condns2 %>% filter(SYNDROME != 'Mortality')
  
      #Checking to make sure that 'Syndrome' is classified correctly 
  assert_that(all(combined_condns2$SYNDROME %in% c("00Asymptomatic","01CommunityDetected", "02Outpatient","03Inpatient")))
  
  
### Age Range

#Dropping those studies with above pre-school age children
combined_condns2 <- combined_condns2 %>% filter(AgeLBMon < 60)

#Generating Age Range
combined_condns2$AGERANGE <- ifelse(combined_condns2$AgeLBMon >= 0 & combined_condns2$AgeUBMon < 12, "0-1yr",
                                      ifelse(combined_condns2$AgeLBMon >= 12 & combined_condns2$AgeUBMon < 24, "1-2yr",
                                             ifelse(combined_condns2$AgeLBMon >= 24 & combined_condns2$AgeUBMon < 60, "3-6yr", "MixedAges")))
### Diagnostic Method

unique(combined_condns2$DXMethod)

  #DXMethod
combined_condns2 <- combined_condns2 %>% mutate(DiagMethod = if_else(DXMethod == 'Molecular', "01Dx_Molc", "01Dx_Conv"))

### Calculating study length midpoint

  #Converting to date format using the R package 'lubridate'
combined_condns2$SITE_START <- lubridate::ymd(combined_condns2$SITE_START)
combined_condns2$SITE_END <- lubridate::ymd(combined_condns2$SITE_END)

  #Deriving the midpoint
combined_condns2$Midpoint <- (combined_condns2$SITE_END - combined_condns2$SITE_START)/2

  #Converting to numeric
combined_condns2$Midpoint <- as.numeric(combined_condns2$Midpoint)
