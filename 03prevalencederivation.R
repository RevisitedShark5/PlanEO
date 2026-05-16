library(metafor)


## 

#Deriving logit-prevalence and SE for each individual study
combined_condns3 <- escalc(measure = "PLO",
              xi = CASES, 
              ni = SAMPLES,
              data = combined_condns2)


#Random-effects meta-analysis by condition
results1 <- combined_condns3 %>%
  group_by(CONDITION) %>%
  group_modify(~ {
    model1 <- rma(yi, vi, data = .x, method = "REML")
    
    # return a tibble (required)
    tibble(
      estimate = model1$b[1],
      se = model1$se,
      pval = model1$pval,
      ci.lb = model1$ci.lb,
      ci.ub = model1$ci.ub
    )
  })

#Estimated prevalence
results$prev <- plogis(results$estimate)
