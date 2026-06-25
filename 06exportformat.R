#Only keeping PAF df
remove(list = setdiff(ls(), 'combinedPrevOR'))

#Keeping only relevant variables 
PAF <- select(combinedPrevOR, CONDITION, SYNDROME, AGERANGE, PAF)
write.csv(PAF, 'PAF.csv')
