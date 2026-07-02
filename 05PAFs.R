#Reloading necessary libraries 
library(tidyverse)

#Joining together prevalence and aORs
combinedPrevOR <- derivedPrev %>% left_join(aORs3, by = c('CONDITION', 'AGERANGE', 'SYNDROME'))


#Filtering out 'Asymptomatic' because that was the referent level for the aORs
combinedPrevOR <- combinedPrevOR %>% filter(SYNDROME != '00Asymptomatic')

#Dropping '99MixedAges' category
combinedPrevOR <- combinedPrevOR %>%
  filter(AGERANGE != '99MixedAges')

#Dropping '04Unspecified' category
combinedPrevOR <- combinedPrevOR %>%
  filter(SYNDROME != '04Unspecified')

#Filtering out those with missing prevalences - see bypass code on '03prevalencederivation.r'
combinedPrevOR <- combinedPrevOR %>% filter(!is.na(prev_pop3))



### Calculating PAF -------------------------------------------------------------------------------

#Manual calculation of PAF
combinedPrevOR <- combinedPrevOR %>%
  mutate(PAF = prev_pop3 * (1-1/aOR))

#Recoding negative PAFs as zero 
combinedPrevOR$PAF <- if_else(combinedPrevOR$PAF < 0, 0, combinedPrevOR$PAF)



### Data Quality Assurance and Fidelity Checks ----------------------------------------------------

assert_that(noNA(combinedPrevOR$PAF)) #no NAs
assert_that(all(combinedPrevOR$PAF >= 0)) #No negative PAFs 
assert_that(all(combinedPrevOR$PAF < 1))



