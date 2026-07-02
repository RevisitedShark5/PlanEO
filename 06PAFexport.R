#Only keeping PAF df
remove(list = setdiff(ls(), 'combinedPrevOR'))

#Keeping only relevant variables 
PAF <- select(combinedPrevOR, CONDITION, SYNDROME, AGERANGE, PAF)

### Data Quality Assurance and Fidelity Checks ----------------------------------------------------

  #Assuring that each combination of 'SYNDROME' & 'AGERANGE' for all conditions sums to less than 1
a <- PAF
a <- a %>%
  group_by(SYNDROME, AGERANGE) %>%
  summarize(PAF = sum(PAF, na.rm = TRUE))

assert_that(all(a$PAF < 1))

#Exporting PAF Files
write.csv(combinedPrevOR, 'combinedPrevOR.csv')
write.csv(PAF, 'PAF.csv')
