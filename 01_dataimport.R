#Clearing 
remove(list=ls())

#Loading Libraries 
library(tidyverse)
library(readxl)
library(metafor)
library(usethis)
library(assertthat)

### Importing relevant condition-specific dfs -----------------------------------------------------

#Adenovirus
aden <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                    sheet = "ADEN")
aden$condition <- 'Adenovirus'


#Astrovirus
astr <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "ASTR")
astr$condition <- 'Astrovirus'


#Campylobacter
campy <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                    sheet = "CAMP")
campy$condition <- 'Campylobacter'

#Cyclospora
cycl <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "CYCL")
cycl$condition <- 'Cyclospora'

#Cryptosporidium
cryp<- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                  sheet = "CRYP")
cryp$condition <- 'Cryptosporidium'

#EAEC
eaec <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "EAEC")
eaec$condition <- 'EAEC'

#Entamoeba 
enta <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "ENTA")
enta$condition <- 'Entamoeba'

#EPTP
eptp <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "EPTP")
eptp$condition <- 'EPTP'

#EPAP
epap <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "EPAP")
epap$condition <- 'EPAP'

#ETLT
etlt <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "ETLT")
etlt$condition <- 'ETLT'

#ETST
etst <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "ETST")
etst$condition <- 'ETST'

#Giardia
giar <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "GIAR")
giar$condition <- 'Giardia'

#Norovirus 
noro <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "NORO")
noro$condition <- 'Norovirus'

#Rotavirus 
rota <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "ROTA")
rota$condition <- 'Rotavirus'

#Salmonella
salm <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "SALM")
salm$condition <- 'Salmonella'

#Sapovirus
sapo <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "SAPO")
sapo$condition <- 'Sapovirus'


#Shigella
shig <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "SHIG")
shig$condition <- 'Shigella'


#STEC
stec <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "STEC")
stec$condition <- 'STEC'

#Vibrio Cholera 
vibr <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                   sheet = "VIBR")
vibr$condition <- 'Vibrio'


### Combining & Standardizing Conditions ----------------------------------------------------------

#Reformatting column titles for 'r_bind' command so that each condition has identical variable (column) names
condition_list <- list(A = aden, B = astr, C = campy, D = cryp, E = cycl, F = eaec, G = enta, H = epap, 
                       I = eptp, J = etlt, K = etst, L = giar, M = noro, N = rota, O = salm, P = sapo, 
                       Q = shig, R = stec, S = vibr)

#Standardizing variable (column) names 
new_names <- c("SITE_ID", "EST_ID", "CONDITION_ID", "SYNDROME", "AGEGRP",
               "AgeLBMon", "AgeUBMon", "AgeMeanYr", "AgeLBYr", "AgeUBYr",
               "SEX", "DXMethod", "STRAIN", "SUBJECTS", "SAMPLES", "CASES",
               "PREV", "SE", "NOTES", "COVIDENCEID", "PLANEO_SOURCE", "CONDITION")

condition_list <- lapply(condition_list, 
                         setNames, nm = new_names)

list2env(condition_list, envir = .GlobalEnv)

#Combining dfs into sinle df using 'rbind'
combined_condns <- rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)

#Only keeping combined dataframe for conditions 
remove(list = setdiff(ls(), "combined_condns"))

### Incorporation of Location Data ----------------------------------------------------------------

#Reading in Geographic/Location Data
locationData <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets//Plan-EO Literature review results.xlsx", 
                                                               sheet = "SITE_index")

#Duplication 'SITE_ID' for locationData
locationDataDuplicates <- locationData %>%
  group_by(SITE_ID) %>%
  filter(n() > 1) %>%
  arrange(SITE_ID)

#Filtering out those duplicates (duplicates have missing 'CovidenceID', otherwise nearly identical)
locationData <- locationData %>% filter(!is.na(`Covidence ID`))


#Combining location-data with condition-data 
combined_condns2 <- combined_condns %>% inner_join(locationData, 
                                                  by = 'SITE_ID',
                                                  relationship = 'many-to-one')

### Addressing Missing Data -----------------------------------------------------------------------

#Filtering out those with missing location data
missingLocation <- combined_condns2 %>% 
  filter(is.na(combined_condns2$SITE_WHO_REGION) | is.na(combined_condns2$SITE_LEVEL) | is.na(combined_condns2$SITE_COUNTRY))

write.csv(missingLocation, "missingLocation.csv")

#Dropping those with missing location-data
combined_condns2 <- combined_condns2 %>% 
  filter(!is.na(combined_condns2$SITE_WHO_REGION), !is.na(combined_condns2$SITE_LEVEL), 
         !is.na(combined_condns2$SITE_COUNTRY))

#Filtering out those with missing sample, cases, prevalence, and/or SE data
missingCases <- combined_condns2 %>% filter(is.na(SAMPLES)|is.na(CASES)|is.na(PREV)|is.na(SE))

#Dropping those with missing sample, cases, prevalence, and/or SE data
combined_condns2 <- combined_condns2 %>% 
  filter(!is.na(combined_condns2$SAMPLES), !is.na(combined_condns2$CASES), 
         !is.na(combined_condns2$PREV), !is.na(combined_condns2$SE))


### Separating out individual conditions ----------------------------------------------------------

#Separating out each individual condition -> Unrelated for analysis but helpful in identifying potential issues

condition_values <- unique(combined_condns2$CONDITION)

for (val in condition_values) {
  assign(
    paste0("dx_", make.names(val)),
    combined_condns2[combined_condns2$CONDITION == val, ],
    envir = .GlobalEnv
  )
}

### Data Quality Assurance and Fidelity Checks ----------------------------------------------------

#Assert-check for duplicate rows
assertthat::assert_that(!any(duplicated(combined_condns2)))

#Assert-checks for non-missing data 
assertthat::assert_that(noNA(combined_condns2$SITE_ID))
assertthat::assert_that(noNA(combined_condns2$EST_ID))
assertthat::assert_that(noNA(combined_condns2$CONDITION_ID))
assertthat::assert_that(noNA(combined_condns2$CONDITION))
assertthat::assert_that(noNA(combined_condns2$AGEGRP))
assertthat::assert_that(noNA(combined_condns2$SYNDROME))
assertthat::assert_that(noNA(combined_condns2$AGEGRP))

assertthat::assert_that(noNA(combined_condns2$SITE_WHO_REGION))
assertthat::assert_that(noNA(combined_condns2$SITE_LEVEL))
assertthat::assert_that(noNA(combined_condns2$SITE_COUNTRY))
assertthat::assert_that(noNA(combined_condns2$CASES))
assertthat::assert_that(noNA(combined_condns2$PREV))
assertthat::assert_that(noNA(combined_condns2$SE))

#Assert-checks for prevalence
assertthat::assert_that(all(combined_condns2$SAMPLES > 0)) #Check for non-negative prevalence denominator
assertthat::assert_that(all(combined_condns2$CASES >= 0)) #Check for positive (zero inclusive) cases 
assertthat::assert_that(all(combined_condns2$PREV >= 0 & combined_condns2$PREV <= 1)) #Check that prevalence is 0 <= PREV <= 1

assertthat::assert_that(all(combined_condns2$SE >= 0 & combined_condns2$SE <= 1)) #Check that SE is 0 <= SE <= 1
