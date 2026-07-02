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

### Prevalence from random-effects meta-analytical model by CONDITION & AGERANGE & SYNDROME (MODEL 4) ----------------------

#Checking number of groups per cell
cell_count <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  summarise(
    k = n_distinct(SITE_ID))

write.csv(cell_count, "cell_count.csv")

### BYPASS CODE -----------------------------------------------------------------------------------

#IMPORTANT! - BYPASS FOR (K <= 1) IN ORDER TO RUN THE RANDOM-EFFECTS LOGIT MODEL! DO NOT FORGET TO REMOVE WHEN CELL SIZE INCREASES 
combined_condns4 <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  filter(n() > 1) %>%
  ungroup()

### Model 1 

#Model 1 - Random-effects meta-analysis by CONDITION, AGERANGE, & SYNDROME with SITE_ID/EST_ID as random-effects
Model1 <- combined_condns4 %>%
  group_by(CONDITION, AGERANGE, SYNDROME) %>%
  group_modify(~ {
    fit <- rma.mv(yi = .x$yi,
                  V = .x$vi,
                  data = .x,
                  verbose = TRUE,
                  method = "REML",
                  control = list(iter.max = 10000, eval.max = 1000),
                  random = ~ 1 | SITE_ID/EST_ID)
    tibble(prev_pop = plogis(fit$b[1]))
  }) %>%
  ungroup()


#Comparison of Raw Prevalence and Random-Effects Model Prevalence
derivedPrev <- rawPrev %>% left_join(Model1, by=c('CONDITION', 'AGERANGE', 'SYNDROME'))
derivedPrev <- select(derivedPrev, CONDITION, AGERANGE, SYNDROME, RawPrev, prev_pop)
