#(Re)loading relevant libraries
library(metafor)
library(lme4)

### Deriving logit-prevalence and SE for each individual study ------------------------------------ 
combined_condns4 <- escalc(measure = "PLO",
                           xi = CASES, 
                           ni = SAMPLES,
                           data = combined_condns3)

  #'escalc' function using 'PLO' (i.e., log-odds) for effect size for each study

### Raw Prevalence by CONDITION, SYNDROME, & AGERANGE ---------------------------------------------
rawPrev <- combined_condns4 %>% 
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  summarize(RawPrev = mean(PREV),
            RawSE = mean(SE))


### Checking number of groups per cell ------------------------------------------------------------
  
cell_count <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  summarize(k = n_distinct(SITE_ID), .groups = "drop") %>%
  complete(CONDITION, AGERANGE, SYNDROME, fill = list(k = 0))
  
  #Exporting cell count
    write.csv(cell_count, "cell_count.csv")

### Model Configuration ---------------------------------------------------------------------------    
    

#IMPORTANT! - BYPASS FOR (K <= 1) IN ORDER TO RUN THE RANDOM-EFFECTS LOGIT MODEL! DO NOT FORGET TO REMOVE WHEN CELL SIZE INCREASES
combined_condns4 <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  filter(n() > 1) %>%
  ungroup()

#Checking number of levels for categorical fixed-effects covariates to ensure eventual model convergence 

  #SITE_RV_VAX
    CNDN_RVvax <- combined_condns4 %>% 
  group_by(CONDITION) %>% 
  summarize(n_levels = n_distinct(SITE_RV_VAX))
    
    assert_that(all(CNDN_RVvax$n_levels > 1))

  #URBAN
    CNDN_Urban <- combined_condns4 %>% 
  group_by(CONDITION) %>% 
  summarize(n_levels = n_distinct(URBAN))

    assert_that(all(CNDN_Urban > 1))    
    
  #SITE_INCOME
    CNDN_SITEINCOME <- combined_condns4 %>%
      group_by(CONDITION) %>%
      summarize(n_levels = n_distinct(SITE_INCOME))
    
    assert_that(all(CNDN_SITEINCOME > 1))


#Model1 is a recreation of the 'FERG' model which runs a hierarchical mixed-effects model with SYNDROME + AGERANGE as fixed effects with SITE_ID/EST_ED as a random effect ----------------------------

    Model1 <- combined_condns4 %>%
      split(.$CONDITION) %>%
      map_dfr(function(df) {
        
        # Fitting Model1 ~ Hierarchical mixed-effects model with SYNDROME + AGERANGE as fixed-effects 
                            #and SITE_ID/EST_ID as random-effect
        
        fit <- rma.mv(yi = df$yi,
                      V = df$vi,
                      data = df,
                      method = "REML",
                      verbose = TRUE,
                      control = list(iter.max = 10000, eval.max = 1000),
                      mods = ~ SYNDROME + AGERANGE,
                      random = list (~ 1 | SITE_ID/EST_ID,
                                     ~1 | SITE_WHO_REGION/SITE_COUNTRY))
        
        pred_grid <- df %>%
          distinct(SYNDROME, AGERANGE) 
        
        # Dropping the intercept column — 'predict.rma.mv' adds it internally, so keeping it would make it redundant
        newmods <- model.matrix(fit$formula.mods, data = pred_grid)
        newmods <- newmods[, colnames(newmods) != "(Intercept)", drop = FALSE]
        
        # Using model to generate predictions by CONDITION, SYNDROME, AND AGERANGE
        preds <- predict(fit, newmods = newmods)
        
        pred_grid %>%
          select(SYNDROME, AGERANGE) %>%
          mutate(
            prev_pop1 = plogis(preds$pred),
            prev_cilb1 = plogis(preds$ci.lb),
            prev_ciub1 = plogis(preds$ci.ub)
          )
        
      }, .id = "CONDITION")


Model1 <- arrange(Model1, CONDITION, SYNDROME, AGERANGE)

### Model 2: Incorporation of 'SITE_RV_VAX' as additional fixed effect 

#Model2 is a recreation of the 'FERG' model which runs a hierarchical mixed-effects model with SYNDROME/AGERANGE/SITE_INCOME as fixed effects with SITE_ID/EST_ED as a random effect. 

Model2 <- combined_condns4 %>%
  split(.$CONDITION) %>%
  map_dfr(function(df) {
    
    # Set/standardizing fixed-effect referent levels for SITE_RV_VAX and URBAN
    df <- df %>%
      mutate(SITE_RV_VAX = factor(SITE_RV_VAX, levels = unique(combined_condns4$SITE_RV_VAX)))
    
    # Fitting Model1 ~ Hierarchical mixed-effects model with SYNDROME/AGERANGE/SITE_RV_VAX/MIDPOINT/URBAN as fixed-effects 
    #and SITE_ID/EST_ID as random-effect
    
    fit <- rma.mv(yi = df$yi,
                  V = df$vi,
                  data   = df,
                  method = "REML",
                  verbose = TRUE,
                  control = list(iter.max = 10000, eval.max = 1000),
                  mods = ~ SYNDROME + AGERANGE + SITE_RV_VAX,
                  random = list (~ 1 | SITE_ID/EST_ID,
                                 ~1 | SITE_WHO_REGION/SITE_COUNTRY))
    
    pred_grid <- df %>%
      distinct(SYNDROME, AGERANGE) %>%
      mutate(SITE_RV_VAX = factor("YES", levels = unique(combined_condns4$SITE_RV_VAX)))
    
    # Dropping the intercept column — 'predict.rma.mv' adds it internally, so keeping it would make it redundant
    newmods <- model.matrix(fit$formula.mods, data = pred_grid)
    newmods <- newmods[, colnames(newmods) != "(Intercept)", drop = FALSE]
    
    # Using model to generate predictions by CONDITION, SYNDROME, AND AGERANGE
    preds <- predict(fit, newmods = newmods)
    
    pred_grid %>%
      select(SYNDROME, AGERANGE) %>%
      mutate(
        prev_pop2 = plogis(preds$pred),
        prev_cilb2 = plogis(preds$ci.lb),
        prev_ciub2 = plogis(preds$ci.ub)
      )
    
  }, .id = "CONDITION")


Model2 <- arrange(Model2, CONDITION, SYNDROME, AGERANGE)

### Model 3: Incorporation of 'URBAN' -------------------------------------------------------------

Model3 <- combined_condns4 %>%
  split(.$CONDITION) %>%
  map_dfr(function(df) {
    
    # Set/standardizing fixed-effect referent levels for SITE_RV_VAX and URBAN
    
    df <- df %>% mutate(SITE_RV_VAX = factor(SITE_RV_VAX, levels = unique(combined_condns4$SITE_RV_VAX)),
                        URBAN = factor(URBAN, levels = unique(combined_condns4$URBAN)))
    
    
    # Fitting Model1 ~ Hierarchical mixed-effects model with SYNDROME/AGERANGE/SITE_RV_VAX/MIDPOINT/URBAN as fixed-effects 
    #and SITE_ID/EST_ID as random-effect
    
    fit <- rma.mv(yi = df$yi,
                  V = df$vi,
                  data   = df,
                  method = "REML",
                  verbose = TRUE,
                  control = list(iter.max = 10000, eval.max = 1000),
                  mods = ~ SYNDROME + AGERANGE + SITE_RV_VAX + URBAN,
                  random = list (~ 1 | SITE_ID/EST_ID,
                                 ~1 | SITE_WHO_REGION/SITE_COUNTRY))
    
    pred_grid <- df %>%
      distinct(SYNDROME, AGERANGE) %>%
      mutate(SITE_RV_VAX = factor("YES", levels = unique(combined_condns4$SITE_RV_VAX)),
             URBAN = factor("YES", levels = unique(combined_condns4$URBAN)))
    
    # Dropping the intercept column — 'predict.rma.mv' adds it internally, so keeping it would make it redundant
    newmods <- model.matrix(fit$formula.mods, data = pred_grid)
    newmods <- newmods[, colnames(newmods) != "(Intercept)", drop = FALSE]
    
    # Using model to generate predictions by CONDITION, SYNDROME, AND AGERANGE
    preds <- predict(fit, newmods = newmods)
    
    pred_grid %>%
      select(SYNDROME, AGERANGE) %>%
      mutate(
        prev_pop3 = plogis(preds$pred),
        prev_cilb3 = plogis(preds$ci.lb),
        prev_ciub3 = plogis(preds$ci.ub)
      )
    
  }, .id = "CONDITION")


Model3 <- arrange(Model3, CONDITION, SYNDROME, AGERANGE)


### Model 4 - Incorporation of Maximal Additional Fixed Effects -----------------------------------
    
    #Model4 is a recreation of the 'FERG' model which runs a hierarchical mixed-effects model with SYNDROME/AGERANGE/SITE_RV_VAX/MIDPOINT/URBAN as fixed effects with SITE_ID/EST_ED as a random effect. 
    
    Model4 <- combined_condns4 %>%
      split(.$CONDITION) %>%
      map_dfr(function(df) {
        
        # Set/standardizing fixed-effect referent levels for SITE_RV_VAX and URBAN
        df <- df %>%
          mutate(SITE_RV_VAX = factor(SITE_RV_VAX, levels = unique(combined_condns4$SITE_RV_VAX)),
                 URBAN = factor(URBAN, levels = unique(combined_condns4$URBAN)),
                 SITE_INCOME = factor(SITE_INCOME, levels = unique(combined_condns4$SITE_INCOME)))
        
        # Fitting Model1 ~ Hierarchical mixed-effects model with SYNDROME/AGERANGE/SITE_RV_VAX/MIDPOINT/URBAN as fixed-effects 
        #and SITE_ID/EST_ID as random-effect
        
        fit <- rma.mv(yi = df$yi,
                      V = df$vi,
                      data  = df,
                      method = "REML",
                      verbose = TRUE,
                      control = list(iter.max = 10000, eval.max = 1000),
                      mods = ~ SYNDROME + AGERANGE + SITE_RV_VAX + SITE_INCOME + URBAN,
                      random = list (~ 1 | SITE_ID/EST_ID,
                                     ~1 | SITE_WHO_REGION/SITE_COUNTRY))
        
        pred_grid <- df %>%
          distinct(SYNDROME, AGERANGE) %>%
          mutate(
            SITE_RV_VAX = factor("YES", levels = unique(combined_condns4$SITE_RV_VAX)),
            SITE_INCOME = factor("01LowIncome", levels = unique(combined_condns4$SITE_INCOME)),
            URBAN  = factor("YES", levels = unique(combined_condns4$URBAN)))
        
        # Dropping the intercept column — 'predict.rma.mv' adds it internally, so keeping it would make it redundant
        newmods <- model.matrix(fit$formula.mods, data = pred_grid)
        newmods <- newmods[, colnames(newmods) != "(Intercept)", drop = FALSE]
        
        # Using model to generate predictions by CONDITION, SYNDROME, AND AGERANGE
        preds <- predict(fit, newmods = newmods)
        
        pred_grid %>%
          select(SYNDROME, AGERANGE) %>%
          mutate(
            prev_pop4 = plogis(preds$pred),
            prev_cilb4 = plogis(preds$ci.lb),
            prev_ciub4 = plogis(preds$ci.ub)
          )
        
      }, .id = "CONDITION")


Model4 <- arrange(Model4, CONDITION, SYNDROME, AGERANGE)


    
### Cleaning up code for output -------------------------------------------------------------------

#Combining raw prevalence and hierarchical mixed-effects modeled prevalence
MEPrevComparison <- Model1 %>% 
  left_join(Model2, by=c('CONDITION', 'SYNDROME', 'AGERANGE')) %>%
  left_join(Model3, by=c('CONDITION', 'SYNDROME', 'AGERANGE')) %>%
  left_join(Model4, by=c('CONDITION', 'SYNDROME', 'AGERANGE'))
  

MEPrevComparison <- select(MEPrevComparison, CONDITION, SYNDROME, AGERANGE, prev_pop1, prev_pop2, prev_pop3, prev_pop4)
write.csv(MEPrevComparison, "MEPrevComparison.csv")

#Change 'derivedPrev' to whichever Model you want to incorporate for PAF calculation 
derivedPrev <- select(MEPrevComparison, CONDITION, SYNDROME, AGERANGE, prev_pop3)


#Export to CSV
write.csv(derivedPrev, "derivedPrev.csv")
