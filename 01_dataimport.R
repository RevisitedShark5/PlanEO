#Clearing 
remove(list=ls())

#Loading Libraries 
library(tidyverse)
library(readxl)
library(metafor)
library(usethis)

#Importing relevant condition-specific dfs


#Campylobacter
campy <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                    sheet = "CAMP")
campy$condition <- 'campylobacter'

#Cyclospora
cycl <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "CYCL")
cycl$condition <- 'cyclospora'

#Cryptosporidium
cryp<- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                  sheet = "CRYP")
cryp$condition <- 'cryptosporidium'

#EAEC
eaec <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "EAEC")
eaec$condition <- 'eaec'

#Entamoeba 
enta <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ENTA")
enta$condition <- 'entamoeba'

#EPTP
eptp <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "EPTP")
eptp$condition <- 'eptp'

#EPAP
epap <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "EPAP")
epap$condition <- 'epap'

#ETLT
etlt <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ETLT")
etlt$condition <- 'etlt'

#ETST
etst <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ETST")
etst$condition <- 'etst'

#Giardia
giar <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "GIAR")
giar$condition <- 'giardia'

#Norovirus 
noro <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "NORO")
noro$condition <- 'norovirus'

#Rotavirus 
rota <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "ROTA")
rota$condition <- 'rotavirus'

#Salmonella
salm <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "SALM")
salm$condition <- 'salmonella'

#Shigella
shig <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "SHIG")
shig$condition <- 'shigella'

#STEC
stec <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "STEC")
stec$condition <- 'stec'

#Vibrio Cholera 
vibr <- read_excel("Desktop/PLANEO_ModelBuild/Datasets/2026-02-13_FERG_results_UVA-SOM.xlsx", 
                   sheet = "VIBR")
vibr$condition <- 'vibrio'

#Reformatting column titles for r_bind
condition_list <- list(A = campy, B = cryp, C = cycl, D = eaec, E = enta, F = epap, 
                       G = eptp, H = etlt, I = etst, J = giar, K = noro, L = rota, M = salm, 
                       N = shig, O = stec, P = vibr)

condition_list <- lapply(condition_list, function(x) {
  colnames(x) <- c("SiteID", "EstID", "ConditionID", 'Syndrome', 'AgeGrp','AgeLBMon', 
                   'AgeUBMon', 'AgeMeanYr', 'AgeLBYr', 'AgeUBYr', 'Sex', 'DxMethod',
                   'Strain', 'Subjects', 'Samples', 'Cases', 'Prev', 'SE', 'AgeMedianYr',
                   'AgePresac', 'AgeSAC','AgeTeen', 'AgeAdult', 'Notes', 'Condition')
  x
})

list2env(condition_list, envir = .GlobalEnv)

#Combining dfs 
combined_condns <- rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)

#Only keeping combined dataframe
remove(list = setdiff(ls(), "combined_condns"))
