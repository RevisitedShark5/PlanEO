library(metafor)


## Model 1 - General Prevalence 

#Deriving logit-prevalence and SE for each individual study - 
combined_condns3 <- escalc(measure = "PLO",
              xi = CASES, 
              ni = SAMPLES,
              data = combined_condns2)


#Random-effects meta-analysis by condition
results1 <- combined_condns3 %>%
  group_by(CONDITION) %>%
  group_modify(~ {
    model1 <- rma(yi, vi, data = .x, method = "REML")
    
    #need to return it to a df
    tibble(
      logitPrev = model1$b[1], 
      se = model1$se,
      pval = model1$pval,
      ci.lb = model1$ci.lb,
      ci.ub = model1$ci.ub
    )
  })

#Estimated prevalence
results1$prev <- plogis(results1$logitPrev)

## Model 2 - Incorporation of Syndrome (e.g., "inpatient/outpatient") and Age Range


#Random-effects meta-analysis by condition
results2 <- combined_condns3 %>%
  group_by(CONDITION, SYNDROME, AGERANGE) %>%
  group_modify(~ {
    model2 <- rma(yi, vi, data = .x, method = "REML")
    
    #need to return it to a df
    tibble(
      logitPrev = model2$b[1], 
      se = model2$se,
      pval = model2$pval,
      ci.lb = model2$ci.lb,
      ci.ub = model2$ci.ub
    )
  })

#Estimated prevalence
results2$prev <- plogis(results2$logitPrev)

