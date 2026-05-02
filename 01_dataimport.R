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
campy$condition <- 'campylobacter'

#Cyclospora
cycl <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "CYCL")
cycl$condition <- 'cyclospora'

#Cryptosporidium
cryp<- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                  sheet = "CRYP")
cryp$condition <- 'cryptosporidium'

#EAEC
eaec <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "EAEC")
eaec$condition <- 'eaec'

#Entamoeba 
enta <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ENTA")
enta$condition <- 'entamoeba'

#EPTP
eptp <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "EPTP")
eptp$condition <- 'eptp'

#EPAP
epap <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "EPAP")
epap$condition <- 'epap'

#ETLT
etlt <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ETLT")
etlt$condition <- 'etlt'

#ETST
etst <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ETST")
etst$condition <- 'etst'

#Giardia
giar <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "GIAR")
giar$condition <- 'giardia'

#Norovirus 
noro <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "NORO")
noro$condition <- 'norovirus'

#Rotavirus 
rota <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ROTA")
rota$condition <- 'rotavirus'

#Salmonella
salm <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "SALM")
salm$condition <- 'salmonella'

#Shigella
shig <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "SHIG")
shig$condition <- 'shigella'

#STEC
stec <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "STEC")
stec$condition <- 'stec'

#Vibrio Cholera 
vibr <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "VIBR")
vibr$condition <- 'vibrio'

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
lit_tracking <- read_excel("~/Desktop/PlanEO_ModelBuild/Datasets/Plan-EO Literature tracking.xlsx", 
                           sheet = "SITE_index")
### 

#Combining location-data with condition-data 
combined_condns2 <- combined_condns %>% left_join(lit_tracking, by = 'SITE_ID')

#dropping those with missing location-data
combined_condns2 <- combined_condns2 %>% 
  filter(!is.na(combined_condns2$SITE_WHO_REGION), !is.na(combined_condns2$SITE_LEVEL), 
         !is.na(combined_condns2$SITE_COUNTRY))


###

#Assert-checks for missing data 
assertthat::assert_that(noNA(combined_condns2$SITE_WHO_REGION))
assertthat::assert_that(noNA(combined_condns2$SITE_LEVEL))
assertthat::assert_that(noNA(combined_condns2$SITE_COUNTRY))
assertthat::assert_that(noNA(combined_condns2$CASES))
assertthat::assert_that(noNA(combined_condns2$PREV))
assertthat::assert_that(noNA(combined_condns2$SE))
