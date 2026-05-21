#Age Cutoffs - Crude

combined_condns2$AGERANGE <- ifelse(combined_condns2$AgeLBMon >= 0 & combined_condns2$AgeUBMon < 12, "0-1yr",
                                    ifelse(combined_condns2$AgeLBMon >= 12 & combined_condns2$AgeUBMon < 24, "1-2yr",
                                           ifelse(combined_condns2$AgeLBMon >= 24 & combined_condns2$AgeUBMon < 36, "2-3yr",
                                                  ifelse(combined_condns2$AgeLBMon >= 36 & combined_condns2$AgeUBMon < 60, "3-5yr",
                                                         ifelse(combined_condns2$AgeLBMon >= 60 & combined_condns2$AgeUBMon < 108, "5-9yr",
                                                                ifelse(combined_condns2$AgeLBMon >= 108 & combined_condns2$AgeUBMon < 144, "9-12yr",
                                                                       ifelse(combined_condns2$AgeLBMon >= 144 & combined_condns2$AgeUBMon < 216, "12-18yr",
                                                                              ifelse(combined_condns2$AgeLBMon >= 216, "18+yr", NA))))))))


combined_condns2$AGERANGE <- ifelse(combined_condns2$AgeLBMon >= 0 & combined_condns2$AgeUBMon < 36, "0-3yr",
                                                  ifelse(combined_condns2$AgeLBMon >= 36 & combined_condns2$AgeUBMon < 108, "3-9yr",
                                                                ifelse(combined_condns2$AgeLBMon >= 108 & combined_condns2$AgeUBMon < 216, "9-18yr",
                                                                              ifelse(combined_condns2$AgeLBMon >= 216, "18+yr", NA))))

