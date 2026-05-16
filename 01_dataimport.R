#Clearing 
remove(list=ls())

#Loading Libraries 
library(tidyverse)
library(readxl)
library(metafor)
library(usethis)
library(assertthat)

#Importing relevant condition-specific dfs


#Campylobacter
campy <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                    sheet = "CAMP")
campy$condition <- 'Campylobacter'

#Cyclospora
cycl <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "CYCL")
cycl$condition <- 'Cyclospora'

#Cryptosporidium
cryp<- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                  sheet = "CRYP")
cryp$condition <- 'Cryptosporidium'

#EAEC
eaec <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "EAEC")
eaec$condition <- 'EAEC'

#Entamoeba 
enta <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ENTA")
enta$condition <- 'Entamoeba'

#EPTP
eptp <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "EPTP")
eptp$condition <- 'EPTP'

#EPAP
epap <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "EPAP")
epap$condition <- 'EPAP'

#ETLT
etlt <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ETLT")
etlt$condition <- 'ETLT'

#ETST
etst <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ETST")
etst$condition <- 'ETST'

#Giardia
giar <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "GIAR")
giar$condition <- 'Giardia'

#Norovirus 
noro <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "NORO")
noro$condition <- 'Norovirus'

#Rotavirus 
rota <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ROTA")
rota$condition <- 'Rotavirus'

#Salmonella
salm <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "SALM")
salm$condition <- 'Salmonella'

#Shigella
shig <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "SHIG")
shig$condition <- 'Shigella'

#STEC
stec <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "STEC")
stec$condition <- 'STEC'

#Vibrio Cholera 
vibr <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "VIBR")
vibr$condition <- 'Vibrio'

#Reformatting column titles for r_bind
condition_list <- list(A = campy, B = cryp, C = cycl, D = eaec, E = enta, F = epap, 
                       G = eptp, H = etlt, I = etst, J = giar, K = noro, L = rota, M = salm, 
                       N = shig, O = stec, P = vibr)

condition_list <- lapply(condition_list, function(x) {
  colnames(x) <- c("SITE_ID", "EST_ID", "CONDITION_ID", 'SYNDROME', 'AGEGRP','AgeLBMon', 
                   'AgeUBMon', 'AgeMeanYr', 'AgeLBYr', 'AgeUBYr', 'SEX', 'DXMethod',
                   'STRAIN', 'SUBJECTS', 'SAMPLES', 'CASES', 'PREV', 'SE', 'AgeMedianYr',
                   'AgePresac', 'AgeSAC','AgeTeen', 'AgeAdult', 'NOTES', 'CONDITION')
  x
})

list2env(condition_list, envir = .GlobalEnv)

#Combining dfs 
combined_condns <- rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)

#Only keeping combined dataframe for conditions 
remove(list = setdiff(ls(), "combined_condns"))

###

#Reading in 'LiteratureTracking' File 
locationData <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                                                               sheet = "SITE_index")
studyData <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                        sheet = "SOURCE_index")
  
### 

#Combining location-data with condition-data 
combined_condns2 <- combined_condns %>% left_join(locationData, by = 'SITE_ID')
combined_condns2 <- combined_condns2 %>% left_join(studyData, by = 'SOURCE_ID')

#dropping those with missing location-data
combined_condns2 <- combined_condns2 %>% 
  filter(!is.na(combined_condns2$SITE_WHO_REGION), !is.na(combined_condns2$SITE_LEVEL), 
         !is.na(combined_condns2$SITE_COUNTRY))


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

