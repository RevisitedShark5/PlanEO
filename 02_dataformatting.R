#Reloading necessary libraries 
library(lubridate)

### Data Formating --------------------------------------------------------------------------------

#Dropping 'Notes' column
combined_condns2$NOTES <- NULL

#Dropping redundant Covidence ID column 
combined_condns2$`Covidence ID`<- NULL

### Syndrome --------------------------------------------------------------------------------------

#Reformatting 'SYNDROME' variable
  
  #All values of 'SYNDROME'  variable -> Indicates different categories of 'Syndrome' variable
  unique(combined_condns2$SYNDROME)
  
  #SYNDROME 
  combined_condns2 <- combined_condns2 %>%
    mutate(SYNDROME = if_else(combined_condns2$SYNDROME == 'Medically attended diarrhea - unspecified', "04Unspecified", combined_condns2$SYNDROME))
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
  
      #Filtering out 'Unspecified'
  combined_condns2 <- combined_condns2 %>% filter(SYNDROME != '04Unspecified')
  
      #Assert-checking to make sure that 'Syndrome' is classified correctly 
  assert_that(all(combined_condns2$SYNDROME %in% c("00Asymptomatic","01CommunityDetected", "02Outpatient","03Inpatient")))
  

### Samples & Cases -------------------------------------------------------------------------------
combined_condns2$SAMPLES <- round(combined_condns2$SAMPLES) #Ensuring SAMPLES are all integers
combined_condns2$CASES <- round(combined_condns2$CASES) #Ensuring CASES are all integers
  

### Age Range -------------------------------------------------------------------------------------

  #Dropping those studies with above pre-school age children
combined_condns2 <- combined_condns2 %>% filter(AgeLBMon < 60)

  #Generating Age Range
combined_condns2$AGERANGE <- ifelse(combined_condns2$AgeLBMon >= 0 & combined_condns2$AgeUBMon < 12, "0-11mo",
                                      ifelse(combined_condns2$AgeLBMon >= 12 & combined_condns2$AgeUBMon < 24, "12-23mo",
                                             ifelse(combined_condns2$AgeLBMon >= 24 & combined_condns2$AgeUBMon < 60, "24-59mo", "99MixedAges")))
  
  #Assert-check to make sure Age Range is classified correctly 
assert_that(all(combined_condns2$AGERANGE %in% c("0-11mo","12-23mo", "24-59mo","99MixedAges")))

  #Assert-check for no NA Age Range
assertthat::assert_that(noNA(combined_condns2$AGERANGE))


### Diagnostic Method -----------------------------------------------------------------------------

unique(combined_condns2$DXMethod)

  #Recoding 'DXMethod' to be binary
combined_condns2 <- combined_condns2 %>% mutate(DIAGMETHOD = if_else(DXMethod == 'Molecular', "01Dx_Molc", "02Dx_NonMolc"))

  #Dropping those without 'Molecular' for 'DIAGMethod' (non-PCR)
combined_condns2 <- combined_condns2 %>% filter(DIAGMETHOD == '01Dx_Molc')
  
  #Assert-check that all remaining observations have 'DIAGMETHOD' == '01Dx_Molc
assertthat::assert_that(all(combined_condns2$DIAGMETHOD == '01Dx_Molc'))
  
  #Assert-check for no NAs for Diagnostic Method 
assertthat::assert_that(noNA(combined_condns2$DIAGMETHOD))

### Calculating study length MIDPOINT -------------------------------------------------------------

  #Converting to date format using the R package 'lubridate'
combined_condns2$SITE_START <- lubridate::ymd(combined_condns2$SITE_START)
combined_condns2$SITE_END <- lubridate::ymd(combined_condns2$SITE_END)

  #Deriving the MIDPOINT
combined_condns2$MIDPOINT <- (combined_condns2$SITE_END - combined_condns2$SITE_START)/2

  #Converting to numeric
combined_condns2$MIDPOINT <- as.numeric(combined_condns2$MIDPOINT)

  #Rounding MIDPOINT to nearest integer
combined_condns2$MIDPOINT <- round(combined_condns2$MIDPOINT)

  #Removing those with missing MIDPOINT
combined_condns2 <- combined_condns2 %>% filter(!is.na(MIDPOINT)) 

  #Assert-check for MIDPOINT Derivation
assertthat::assert_that(noNA(combined_condns2$MIDPOINT)) #Assert-that check no NA for Midpoint
assertthat::assert_that(all(combined_condns2$MIDPOINT != 0)) #Assert-that check that none are zero



### Rotavirus Vaccination -------------------------------------------------------------------------

  #Recoding values to all-caps
  combined_condns2$SITE_RV_VAX <- toupper(combined_condns2$SITE_RV_VAX)

  #Assert-check for no NAs Rotavirus Vaccination
assertthat::assert_that(noNA(combined_condns2$SITE_RV_VAX)) 

### Urban/Rural -----------------------------------------------------------------------------------

unique(combined_condns2$SITE_URBAN)

  #Recoding 'SITE_URBAN'
combined_condns2 <- combined_condns2 %>% mutate(URBAN = if_else(SITE_URBAN == 'Urban', 'YES', 'NO'))


### Consolidating only relevant variables for analysis --------------------------------------------
combined_condns3 <- select(combined_condns2, SITE_ID, EST_ID, CONDITION_ID, SYNDROME, SEX, SUBJECTS, SAMPLES, CASES, PREV,
                           SE, COVIDENCEID, CONDITION, SOURCE_ID, SITE_WHO_REGION, SITE_INCOME, SITE_LEVEL, SITE_ISO, SITE_COUNTRY,
                           SITE_RV_VAX, AGERANGE, DIAGMETHOD, MIDPOINT, URBAN)

