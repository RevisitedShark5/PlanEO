#(Re)loading relevant libraries
library(metafor)
library(lme4)

### Deriving logit-prevalence and SE for each individual study ------------------------------------ 
combined_condns4 <- escalc(measure = "PLO",
                           xi = CASES, 
                           ni = SAMPLES,
                           data = combined_condns3)

### Raw Prevalence by CONDITION, SYNDROME, & AGERANGE ---------------------------------------------
rawPrev <- combined_condns4 %>% 
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  summarize(RawPrev = mean(PREV),
            RawSE = mean(SE))


### Checking number of groups per cell ------------------------------------------------------------
  
  cell_count <- combined_condns4 %>%
    group_by(CONDITION, AGERANGE, SYNDROME) %>%
    summarise(k = n_distinct(SITE_ID))
  
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
  summarise(n_levels = n_distinct(SITE_RV_VAX))
    
    assert_that(all(CNDN_RVvax$n_levels > 1))

  #URBAN
    CNDN_Urban <- combined_condns4 %>% 
  group_by(CONDITION) %>% 
  summarise(n_levels = n_distinct(URBAN))

    assert_that(all(CNDN_Urban > 1))    

#Model1 is a recreation of the 'FERG' model which runs a hierarchical mixed-effects model with SYNDROME/AGERANGE as random-effects with SITE_ID/EST_ED and SITE_WHO_REGION/SITE_COUNTRY as random effects. 
#Model1 differs from the above model in that instead of running a mixed-effects model on each combination of CONDITION, SYNDROME, and AGERANGE, it instead runs a general ME model that calculate logit-prevalence with SYNDROME/AGERANGE as fixed effects.

    Model1 <- combined_condns4 %>%
      split(.$CONDITION) %>%
      map_dfr(function(df) {
        
        # Set/standardizing fixed-effect referent levels for SITE_RV_VAX and URBAN
        df <- df %>%
          mutate(
            SITE_RV_VAX = factor(SITE_RV_VAX, levels = unique(combined_condns4$SITE_RV_VAX)),
            URBAN = factor(URBAN, levels = unique(combined_condns4$URBAN))
          )
        
        # Fitting Model1 ~ Hierarchical mixed-effects model with SYNDROME/AGERANGE/SITE_RV_VAX/MIDPOINT/URBAN as fixed-effects 
                            #and SITE_ID/EST_ID + SITE_WHO_REGION/SITE_COUNTRY as random-effects
        
        fit <- rma.mv(yi = df$yi,
                      V = df$vi,
                      data   = df,
                      method = "REML",
                      verbose = TRUE,
                      mods = ~ SYNDROME + AGERANGE + SITE_RV_VAX + MIDPOINT + URBAN,
                      random = list(~ 1 | SITE_ID/EST_ID))
        
        pred_grid <- df %>%
          distinct(SYNDROME, AGERANGE) %>%
          mutate(
            SITE_RV_VAX = factor("NO", levels = levels(df$SITE_RV_VAX)),
            MIDPOINT  = mean(df$MIDPOINT, na.rm = TRUE),
            URBAN  = factor("NO", levels = levels(df$URBAN)))
        
        # Drop the intercept column — predict.rma.mv adds it internally so keeping it would make it redundant
        newmods <- model.matrix(fit$formula.mods, data = pred_grid)
        newmods <- newmods[, colnames(newmods) != "(Intercept)", drop = FALSE]
        
        # Using model to generate predictions by CONDITION, SYNDROME, AND AGERANGE
        preds <- predict(fit, newmods = newmods)
        
        pred_grid %>%
          select(SYNDROME, AGERANGE) %>%
          mutate(
            MEPrev = plogis(preds$pred),
            prev_cilb = plogis(preds$ci.lb),
            prev_ciub = plogis(preds$ci.ub)
          )
        
      }, .id = "CONDITION")
    
    
### Cleaning up code for output -------------------------------------------------------------------

#Combining raw prevalence and hierarchical mixed-effects modeled prevalence
derivedPrev <- rawPrev %>% 
  left_join(Model1, by=c('CONDITION', 'SYNDROME', 'AGERANGE'))
derivedPrev <- select(derivedPrev, CONDITION, SYNDROME, AGERANGE, RawPrev, MEPrev)

derivedPrev <- derivedPrev %>% 
  mutate(PrevPercentageChange = (((RawPrev - MEPrev)/RawPrev)*100))


#Export to CSV
write.csv(derivedPrev, "derivedPrev.csv")
