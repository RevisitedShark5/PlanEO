#Reloading necessary libraries 
library(tidyverse)
library(lubridate)

###

#Dropping 'Notes' column
combined_condns2$NOTES <- NULL

### Syndrome

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
  

### Samples & Cases
combined_condns2$SAMPLES <- round(combined_condns2$SAMPLES) #Ensuring SAMPLES are all integers
combined_condns2$CASES <- round(combined_condns2$CASES) #Ensuring CASES are all integers
  

### Age Range

  #Dropping those studies with above pre-school age children
combined_condns2 <- combined_condns2 %>% filter(AgeLBMon < 60)

  #Generating Age Range
combined_condns2$AGERANGE <- ifelse(combined_condns2$AgeLBMon >= 0 & combined_condns2$AgeUBMon < 12, "0-1yr",
                                      ifelse(combined_condns2$AgeLBMon >= 12 & combined_condns2$AgeUBMon < 24, "1-2yr",
                                             ifelse(combined_condns2$AgeLBMon >= 24 & combined_condns2$AgeUBMon < 60, "3-5yr", "MixedAges")))
  #Assert-check for Age Range
assertthat::assert_that(noNA(combined_condns2$AGERANGE))

### Diagnostic Method

unique(combined_condns2$DXMethod)

  #Recoding 'DXMethod'
combined_condns2 <- combined_condns2 %>% mutate(DIAGMETHOD = if_else(DXMethod == 'Molecular', "01Dx_Molc", "01Dx_Conv"))

  #Assert-check for Diagnostic Method 
assertthat::assert_that(noNA(combined_condns2$DIAGMETHOD))

### Calculating study length MIDPOINT

  #Converting to date format using the R package 'lubridate'
combined_condns2$SITE_START <- lubridate::ymd(combined_condns2$SITE_START)
combined_condns2$SITE_END <- lubridate::ymd(combined_condns2$SITE_END)

  #Deriving the MIDPOINT
combined_condns2$MIDPOINT <- (combined_condns2$SITE_END - combined_condns2$SITE_START)/2

  #Converting to numeric
combined_condns2$MIDPOINT <- as.numeric(combined_condns2$MIDPOINT)

  #Assert-check for MIDPOINT Derivation
combined_condns2 <- combined_condns2 %>% filter(!is.na(MIDPOINT)) #There is one study missing both a SITE_Start and SITE_END (File ID: 41975)
combined_condns2$MIDPOINT <- round(combined_condns2$MIDPOINT)

assertthat::assert_that(noNA(combined_condns2$MIDPOINT)) 
assertthat::assert_that(all(combined_condns2$MIDPOINT != 0))



### Rotavirus Vaccination

  #Assert-check for Rotavirus Vaccination
assertthat::assert_that(noNA(combined_condns2$SITE_RV_VAX)) #Same study as above is missing SITE_RV_VAX

### Urban/Rural

unique(combined_condns2$SITE_URBAN)

  #Recoding 'SITE_URBAN'
combined_condns2 <- combined_condns2 %>% mutate(URBAN = if_else(SITE_URBAN == 'Urban', 'YES', 'NO'))


### Consolidating only relevant variables for analysis
combined_condns3 <- select(combined_condns2, SITE_ID, EST_ID, CONDITION_ID, SYNDROME, SEX, SUBJECTS, SAMPLES, CASES, PREV,
                           SE, FILENAME, CONDITION, SOURCE_ID, SITE_WHO_REGION, SITE_INCOME, SITE_LEVEL, SITE_ISO, SITE_COUNTRY,
                           SITE_RV_VAX, AGERANGE, DIAGMETHOD, MIDPOINT, URBAN)

