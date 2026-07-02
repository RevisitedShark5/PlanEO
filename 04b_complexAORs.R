#(Re)loading Relevant Libraries
library(tidyverse)

### Deriving logit-prevalence and SE for each individual study ------------------------------------ 

#'escalc' function using 'PLO' (i.e., log-odds) for effect size for each study
combined_condns5 <- escalc(measure = "PLO", 
                 xi = CASES,
                 ni = SAMPLES,
                 data = combined_condns3)

#setting referent level for 'SYNDROME' as '00Asymptomatic'
combined_condns5 <- combined_condns5 %>% mutate(SYNDROME = relevel(factor(SYNDROME), ref = '00Asymptomatic'))

#setting referent level for 'URBAN' as 'YES'
combined_condns5 <- combined_condns5 %>% mutate(URBAN = relevel(factor(URBAN), ref = 'YES'))

#setting referent level for 'SITE_RV_VAX' as 'YES'
combined_condns5 <- combined_condns5 %>% mutate(SITE_RV_VAX = relevel(factor(SITE_RV_VAX), ref = 'YES'))

### aORs for Model3 -------------------------------------------------------------------------------
aORs3 <- combined_condns5 %>%
  group_by(CONDITION, AGERANGE) %>%
  group_modify(function(sub, grp) {
    
    # Standardizing factor levels -> similar to what was done for prev log-odds
    sub <- sub %>%
      mutate(SITE_RV_VAX = factor(SITE_RV_VAX, levels = unique(combined_condns5$SITE_RV_VAX)), 
             URBAN = factor(URBAN, levels = unique(combined_condns5$URBAN)))
    
    # Fit aOR model
    fit <- rma.mv(
      yi = yi, V = vi,
      mods   = ~ SYNDROME + SITE_RV_VAX + URBAN,
      random = ~ 1 | SITE_ID/EST_ID,
      data = sub,
      method = "REML",
      control = list(iter.max = 10000, eval.max = 1000),
      verbose = TRUE)
    
    # Extract 'SYNDROME' coefficients only
    coef_names  <- names(coef(fit))
    syndrome_idx <- grepl("^SYNDROME", coef_names)
    log_aOR <- coef(fit)[syndrome_idx]
    se_log_aOR <- sqrt(diag(vcov(fit))[syndrome_idx])
    
    # Return results in df
    tibble(SYNDROME  = gsub("SYNDROME", "", coef_names[syndrome_idx]),
           aOR = exp(log_aOR),
           aOR_CI_lo = exp(log_aOR - 1.96 * se_log_aOR),
           aOR_CI_hi = exp(log_aOR + 1.96 * se_log_aOR)
    )
    
  }) %>%
  ungroup() %>%
  select(CONDITION, AGERANGE, SYNDROME, aOR, aOR_CI_lo, aOR_CI_hi)



### aORs for Model4 -------------------------------------------------------------------------------
aORs4 <- combined_condns5 %>%
  group_by(CONDITION, AGERANGE) %>%
  group_modify(function(sub, grp) {
    
    # Standardizing factor levels -> similar to what was done for prev log-odds
    sub <- sub %>%
      mutate(SITE_RV_VAX = factor(SITE_RV_VAX, levels = unique(combined_condns5$SITE_RV_VAX)), 
             SITE_INCOME = factor(SITE_INCOME, levels = unique(combined_condns5$SITE_INCOME)),
             URBAN = factor(URBAN, levels = unique(combined_condns5$URBAN)))
    
    # Fit aOR model
    fit <- rma.mv(
      yi = yi, V = vi,
      mods   = ~ SYNDROME + URBAN + SITE_RV_VAX + SITE_INCOME,
      random = ~ 1 | SITE_ID/EST_ID,
      data = sub,
      method = "REML",
      control = list(iter.max = 10000, eval.max = 1000),
      verbose = TRUE)
    
    # Extract 'SYNDROME' coefficients only
    coef_names  <- names(coef(fit))
    syndrome_idx <- grepl("^SYNDROME", coef_names)
    log_aOR <- coef(fit)[syndrome_idx]
    se_log_aOR <- sqrt(diag(vcov(fit))[syndrome_idx])
    
    # Return results in df
    tibble(SYNDROME  = gsub("SYNDROME", "", coef_names[syndrome_idx]),
      aOR = exp(log_aOR),
      aOR_CI_lo = exp(log_aOR - 1.96 * se_log_aOR),
      aOR_CI_hi = exp(log_aOR + 1.96 * se_log_aOR)
    )
    
  }) %>%
  ungroup() %>%
  select(CONDITION, AGERANGE, SYNDROME, aOR, aOR_CI_lo, aOR_CI_hi)




