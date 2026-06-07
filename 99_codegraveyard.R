### Prevalence from random-effects meta-analytical model by Condition -----------------------------

#General Condition-Specific Raw Prevalence and SE (MODEL 1)

#Estimated prevalence derived from logit-prevalence
rawPrev1 <- combined_condns4 %>% 
  group_by(CONDITION) %>%
  summarize(RawPrev = mean(PREV),
            RawSE = mean(SE))

#Model 1 - Random-effects meta-analysis by CONDITION exclusively
Model1 <- combined_condns4 %>%
  group_by(CONDITION) %>% 
  group_modify(~ {
    model1 <- rma.mv(yi=.x$yi, 
                     V=.x$vi,
                     data=.x,
                     method="REML",
                     level=95,
                     verbose = TRUE,
                     random = ~ 1 | SITE_ID/EST_ID)
    
    tibble(
      logitPrev = model1$b[1],
      se        = model1$se[1],
      pval      = model1$pval[1],
      ci.lb     = model1$ci.lb[1],
      ci.ub     = model1$ci.ub[1],
      studies   = model1$k,
      sigma2_site = model1$sigma2[1],
      sigma2_est  = model1$sigma2[2]
    )
  })

#Back-transforming estimated prevalence derived from logit-prevalence
Model1$prev <- plogis(Model1$logitPrev)

#Comparison of Raw Prevalence and Random-Effects Model Prevalence
prevComparison1 <- rawPrev1 %>% left_join(Model1, by='CONDITION')
prevComparison1 <- select(prevComparison1, CONDITION, RawPrev, prev)


### Prevalence from random-effects meta-analytical model by CONDITION & AGERANGE (MODEL 2) --------


#Estimated raw prevalence according to CONDITION and AGE-RANGE derived from logit-prevalence
rawPrev2 <- combined_condns4 %>% 
  group_by(CONDITION, AGERANGE) %>%
  summarize(RawPrev = mean(PREV),
            RawSE = mean(SE))

#Model 2 - Random-effects meta-analysis by CONDITION and AGERANGE
Model2 <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE) %>% 
  group_modify(~ {
    model2 <- rma.mv(yi = .x$yi, 
                     V = .x$vi,
                     data = .x,
                     method  = "REML",
                     verbose = TRUE,
                     level = 95,
                     random  = ~ 1 | SITE_ID/EST_ID)
    
    tibble(
      logitPrev = model2$b[1],
      se        = model2$se[1],
      pval      = model2$pval[1],
      ci.lb     = model2$ci.lb[1],
      ci.ub     = model2$ci.ub[1],
      studies   = model2$k,
      sigma2_site = model2$sigma2[1],
      sigma2_est  = model2$sigma2[2]
    )
  })

#Estimated prevalence derived from logit-prevalence
Model2$prev <- plogis(Model2$logitPrev)

#Comparison of Raw Prevalence and Random-Effects Model Prevalence
prevComparison2 <- rawPrev2 %>% left_join(Model2, by=c('CONDITION', 'AGERANGE'))
prevComparison2 <- select(prevComparison2, CONDITION, AGERANGE, RawPrev, prev)

### Prevalence from random-effects meta-analytical model by CONDITION & SYNDROME (MODEL 3)

#Estimated prevalence according to CONDITION and SYNDROME derived from logit-prevalence
rawPrev3 <- combined_condns4 %>% 
  group_by(CONDITION, SYNDROME) %>%
  summarize(RawPrev = mean(PREV),
            RawSE = mean(SE))

#Model 3 - Random-effects meta-analysis by CONDITION and SYNDROME
Model3 <- combined_condns4 %>%
  group_by(CONDITION, SYNDROME) %>% 
  group_modify(~ {
    model3 <- rma.mv(yi      = .x$yi, 
                     V       = .x$vi,
                     data    = .x,
                     method  = "REML",
                     verbose = TRUE,
                     level   = 95,
                     random  = ~ 1 | SITE_ID/EST_ID)
    
    tibble(
      logitPrev = model3$b[1],
      se        = model3$se[1],
      pval      = model3$pval[1],
      ci.lb     = model3$ci.lb[1],
      ci.ub     = model3$ci.ub[1],
      studies   = model3$k,
      sigma2_site = model3$sigma2[1],
      sigma2_est  = model3$sigma2[2]
    )
  })

#Estimated prevalence derived from logit-prevalence
Model3$prev <- plogis(Model3$logitPrev)

#Comparison of Raw Prevalence and Random-Effects Model Prevalence
prevComparison3 <- rawPrev3 %>% left_join(Model3, by=c('CONDITION', 'SYNDROME'))
prevComparison3 <- select(prevComparison3, CONDITION, SYNDROME, RawPrev, prev)

###########################


### Prevalence from random-effects meta-analytical model by CONDITION & AGERANGE & SYNDROME (MODEL 4)  

#Estimated raw prevalence according to CONDITION, AGERANGE, and SYNDROME derived from logit-prevalence
rawPrev4 <- combined_condns4 %>% 
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  summarize(RawPrev = mean(PREV),
            RawSE = mean(SE))

#Checking number of groups per cell
cell_count <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  summarise(
    k = n_distinct(SITE_ID))

write.csv(cell_count, "cell_count.csv")

###

#IMPORTANT! - BYPASS FOR (K <= 1) IN ORDER TO RUN THE RANDOM-EFFECTS LOGIT MODEL! DO NOT FORGET TO REMOVE WHEN CELL SIZE INCREASES 
combined_condns4 <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  filter(n() > 1) %>%
  ungroup()

###

#Model 4 - Random-effects meta-analysis by CONDITION and SYNDROME
Model4 <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  group_modify(~ {
    fit <- rma.mv(yi = .x$yi,
                  V = .x$vi,
                  data = .x,
                  verbose = TRUE,
                  method = "REML",
                  random = ~ 1 | SITE_ID/EST_ID)
    tibble(prev_pop = plogis(fit$b[1]))
  }) %>%
  ungroup()


#Comparison of Raw Prevalence and Random-Effects Model Prevalence
prevComparison4 <- rawPrev4 %>% left_join(Model4, by=c('CONDITION', 'AGERANGE', 'SYNDROME'))
prevComparison4 <- select(prevComparison4, CONDITION, AGERANGE, SYNDROME, RawPrev, prev_pop)