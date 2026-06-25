#(Re)loading Relevant Libraries
library(car)
library(tidyverse)

#Ensuring reference level is set to '00Asymptomatic'
combined_condns4 <- combined_condns4 %>%
  mutate(SYNDROME = relevel(factor(SYNDROME), ref = "00Asymptomatic"))

#Deriving random-effects meta-analytical Adjusted ORs
aORs <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE) %>%
  group_modify(~ {
    model1 <- rma.mv(yi=yi,
                     V=vi,
                     mods= ~ SYNDROME,
                     data= .x,
                     verbose = TRUE,
                     control = list(iter.max = 10000, eval.max = 1000),
                     method= "REML",
                     random= ~ 1 | SITE_ID/EST_ID)
    
    tibble(
      category = rownames(model1$b),
      logOR = as.numeric(model1$b),
      se    = as.numeric(model1$se),
      pval  = as.numeric(model1$pval),
      ci.lb = as.numeric(model1$ci.lb),
      ci.ub = as.numeric(model1$ci.ub),
      studies = model1$k,
      aOR   = exp(as.numeric(model1$b)),
      lower = exp(as.numeric(model1$ci.lb)),
      upper = exp(as.numeric(model1$ci.ub))
    )
  })

#Recoding 'SYNDROME' variable to be concordant for merging
aORs <- aORs %>%
  mutate(
    SYNDROME = str_remove(category, "^SYNDROME"),
    SYNDROME = na_if(SYNDROME, "intrcpt")
  )

#Dropping intercept
aORs <- aORs %>%
  filter(!is.na(SYNDROME))
