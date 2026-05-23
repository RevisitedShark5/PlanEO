#(Re)loading relevant libraries
library(metafor)
library(lme4)

## Deriving logit-prevalence and SE for each individual study  
combined_condns4 <- escalc(measure = "PLO",
                           xi = CASES, 
                           ni = SAMPLES,
                           data = combined_condns3)

### Prevalence from random-effects meta-analytical model by Condition


#General Condition-Specific Raw Prevalence and SE

  #Estimated prevalence derived from logit-prevalence
rawPrev1 <- combined_condns4 %>% 
  group_by(CONDITION) %>%
  summarize(RawPrev = mean(PREV),
            RawSE = mean(SE))

  #Model 1 - Random-effects meta-analysis by condition
Model1 <- combined_condns4 %>%
  group_by(CONDITION) %>% 
  group_modify(~ {
    model1 <- rma.mv(yi=.x$yi, 
                     V=.x$vi,
                     data=.x,
                     method="REML",
                     level=95,
                     random = ~ 1 | SITE_ID/EST_ID)
    
    tibble(
      logitPrev = model1$b[1],
      se        = model1$se[1],
      pval      = model1$pval[1],
      ci.lb     = model1$ci.lb[1],
      ci.ub     = model1$ci.ub[1],
      studies   = model1$k,
      tau2_site = model1$sigma2[1],
      tau2_est  = model1$sigma2[2]
    )
  })

  #Estimated prevalence derived from logit-prevalence
Model1$prev <- plogis(Model1$logitPrev)

  #Comparison of Raw Prevalence and Random-Effects Model Prevalence
prevComparison1 <- rawPrev1 %>% left_join(Model1, by='CONDITION')
prevComparison1 <- select(prevComparison1, CONDITION, RawPrev, prev)


### Prevalence from random-effects meta-analytical model by Condition & Age Range


#General Condition-Specific Raw Prevalence and SE

#Estimated prevalence derived from logit-prevalence
rawPrev2 <- combined_condns4 %>% 
  group_by(CONDITION, AGERANGE) %>%
  summarize(RawPrev = mean(PREV),
            RawSE = mean(SE))

#Model 2 - Random-effects meta-analysis by condition and age range
Model2 <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE) %>% 
  group_modify(~ {
    model2 <- rma.mv(yi      = .x$yi, 
                     V       = .x$vi,
                     data    = .x,
                     method  = "REML",
                     level   = 95,
                     random  = ~ 1 | SITE_ID/EST_ID)
    
    tibble(
      logitPrev = model2$b[1],
      se        = model2$se[1],
      pval      = model2$pval[1],
      ci.lb     = model2$ci.lb[1],
      ci.ub     = model2$ci.ub[1],
      studies   = model2$k,
      tau2_site = model2$sigma2[1],
      tau2_est  = model2$sigma2[2]
    )
  })

#Estimated prevalence derived from logit-prevalence
Model2$prev <- plogis(Model2$logitPrev)

#Comparison of Raw Prevalence and Random-Effects Model Prevalence
prevComparison2 <- rawPrev2 %>% left_join(Model2, by=c('CONDITION', 'AGERANGE'))
prevComparison2 <- select(prevComparison2, CONDITION, AGERANGE, RawPrev, prev)

###

#General Condition-Specific Raw Prevalence and SE

#Estimated prevalence derived from logit-prevalence
rawPrev3 <- combined_condns4 %>% 
  group_by(CONDITION, AGERANGE) %>%
  summarize(RawPrev = mean(PREV),
            RawSE = mean(SE))

#Model 3 - Random-effects meta-analysis by condition and age range
Model3 <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE) %>% 
  group_modify(~ {
    model3 <- rma.mv(yi      = .x$yi, 
                     V       = .x$vi,
                     data    = .x,
                     method  = "REML",
                     level   = 95,
                     random  = ~ 1 | SITE_ID/EST_ID)
    
    tibble(
      logitPrev = model3$b[1],
      se        = model3$se[1],
      pval      = model3$pval[1],
      ci.lb     = model3$ci.lb[1],
      ci.ub     = model3$ci.ub[1],
      studies   = model3$k,
      tau2_site = model3$sigma2[1],
      tau2_est  = model3$sigma2[2]
    )
  })

#Estimated prevalence derived from logit-prevalence
Model3$prev <- plogis(Model3$logitPrev)

#Comparison of Raw Prevalence and Random-Effects Model Prevalence
prevComparison3 <- rawPrev3 %>% left_join(Model3, by=c('CONDITION', 'AGERANGE'))
prevComparison3 <- select(prevComparison3, CONDITION, AGERANGE, RawPrev, prev)

