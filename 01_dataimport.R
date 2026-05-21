#Clearing 
remove(list=ls())

#Loading Libraries 
library(tidyverse)
library(readxl)
library(metafor)
library(usethis)
library(assertthat)

#Importing relevant condition-specific dfs

#Adenovirus
aden <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                    sheet = "ADEN")
aden$ADEN_AGE_MED_YR <- NA
aden <- aden[, c(1:(ncol(aden)-2), ncol(aden), ncol(aden)-1)] #For some reason, on 'Literature Tracking', adenovirus is missing an Age_Median_YR variable which is found in all the other conditions
aden$condition <- 'Adenovirus'


#Astrovirus
astr <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "ASTR")
astr$condition <- 'Astrovirus'


#Campylobacter
campy <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                    sheet = "CAMP")
campy$condition <- 'Campylobacter'

#Cyclospora
cycl <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "CYCL")
cycl$condition <- 'Cyclospora'

#Cryptosporidium
cryp<- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                  sheet = "CRYP")
cryp$condition <- 'Cryptosporidium'

#EAEC
eaec <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "EAEC")
eaec$condition <- 'EAEC'

#Entamoeba 
enta <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "ENTA")
enta$condition <- 'Entamoeba'

#EPTP
eptp <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "EPTP")
eptp$condition <- 'EPTP'

#EPAP
epap <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "EPAP")
epap$condition <- 'EPAP'

#ETLT
etlt <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "ETLT")
etlt$condition <- 'ETLT'

#ETST
etst <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "ETST")
etst$condition <- 'ETST'

#Giardia
giar <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "GIAR")
giar$condition <- 'Giardia'

#Norovirus 
noro <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "NORO")
noro$condition <- 'Norovirus'

#Rotavirus 
rota <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "ROTA")
rota$condition <- 'Rotavirus'

#Salmonella
salm <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "SALM")
salm$condition <- 'Salmonella'

#Sapovirus
sapo <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "SAPO")
sapo$condition <- 'Sapovirus'


#Shigella
shig <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "SHIG")
shig$condition <- 'Shigella'


#STEC
stec <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "STEC")
stec$condition <- 'STEC'

#Vibrio Cholera 
vibr <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                   sheet = "VIBR")
vibr$condition <- 'Vibrio'

#Reformatting column titles for r_bind
condition_list <- list(A = aden, B = astr, C = campy, D = cryp, E = cycl, F = eaec, G = enta, H = epap, 
                       I = eptp, J = etlt, K = etst, L = giar, M = noro, N = rota, O = salm, P = sapo, 
                       Q = shig, R = stec, S = vibr)

condition_list <- lapply(condition_list, function(x) {
  colnames(x) <- c("SITE_ID", "EST_ID", "CONDITION_ID", 'SYNDROME', 'AGEGRP','AgeLBMon', 
                   'AgeUBMon', 'AgeMeanYr', 'AgeLBYr', 'AgeUBYr', 'SEX', 'DXMethod',
                   'STRAIN', 'SUBJECTS', 'SAMPLES', 'CASES', 'PREV', 'SE', 'NOTES', 'AgeMedianYr', 
                   'FILENAME', 'CONDITION')
  x
})

list2env(condition_list, envir = .GlobalEnv)

#Combining dfs 
combined_condns <- rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)

#Only keeping combined dataframe for conditions 
remove(list = setdiff(ls(), "combined_condns"))

###

#Reading in 'LiteratureTracking' File 
locationData <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                                                               sheet = "SITE_index")

  
### 

#Combining location-data with condition-data 
combined_condns2 <- combined_condns %>% left_join(locationData, by = 'SITE_ID')


#Dropping those with missing location-data
combined_condns2 <- combined_condns2 %>% 
  filter(!is.na(combined_condns2$SITE_WHO_REGION), !is.na(combined_condns2$SITE_LEVEL), 
         !is.na(combined_condns2$SITE_COUNTRY))

#Dropping those with missing sample, cases, prevalence, and/or SE data
combined_condns2 <- combined_condns2 %>% 
  filter(!is.na(combined_condns2$SAMPLES), !is.na(combined_condns2$CASES), 
         !is.na(combined_condns2$PREV), !is.na(combined_condns2$SE))


###

condition_values <- unique(combined_condns2$CONDITION)

for (val in condition_values) {
  assign(
    paste0("dx_", make.names(val)),
    combined_condns2[combined_condns2$CONDITION == val, ],
    envir = .GlobalEnv
  )
}

### Data Quality Assurance and Fidelity Checks 

###

#Assert-checks for non-missing data 
assertthat::assert_that(noNA(combined_condns2$SITE_ID))
assertthat::assert_that(noNA(combined_condns2$EST_ID))
assertthat::assert_that(noNA(combined_condns2$CONDITION_ID))
assertthat::assert_that(noNA(combined_condns2$CONDITION))
assertthat::assert_that(noNA(combined_condns2$AGEGRP))

assertthat::assert_that(noNA(combined_condns2$SITE_WHO_REGION))
assertthat::assert_that(noNA(combined_condns2$SITE_LEVEL))
assertthat::assert_that(noNA(combined_condns2$SITE_COUNTRY))
assertthat::assert_that(noNA(combined_condns2$CASES))
assertthat::assert_that(noNA(combined_condns2$PREV))
assertthat::assert_that(noNA(combined_condns2$SE))

