#Reloading necessary libraries 
library(tidyverse)


#Joining together prevalence and aORs
combinedPrevOR <- derivedPrev %>% left_join(aORs, by = c('CONDITION', 'AGERANGE', 'SYNDROME'))

#Filtering out 'Asymptomatic' because that was the referent level for the aORs
combinedPrevOR <- combinedPrevOR %>% filter(SYNDROME != '00Asymptomatic')

#Filtering out those with missing prevalences - see bypass code on '03prevalencederivation.r'
combinedPrevOR <- combinedPrevOR %>% filter(!is.na(MEPrev))

### Calculating PAF -------------------------------------------------------------------------------

#Manual calculation of PAF
combinedPrevOR <- combinedPrevOR %>%
  mutate(PAF = (MEPrev * (aOR - 1)) / (MEPrev * (aOR - 1) + 1))

#Recoding negative PAFs as zero 
combinedPrevOR$PAF <- if_else(combinedPrevOR$PAF < 0, 0, combinedPrevOR$PAF)

#Dropping '99MixedAges' category
combinedPrevOR <- combinedPrevOR %>%
  filter(AGERANGE != '99MixedAges')

#Exporting as CSV
write.csv(combinedPrevOR, "PAFS.csv")

