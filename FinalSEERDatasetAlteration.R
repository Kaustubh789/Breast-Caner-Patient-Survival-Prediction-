df <- read.csv("SEERDatasetMinorFinalFileToBeUsed.csv",stringsAsFactors = FALSE)
df$Survivability <- NULL

write.csv(df,"FinalSEERDatasetForMinor.csv",row.names = FALSE)

df2 <- read.csv("FinalSEERDatasetForMinor.csv",stringsAsFactors = FALSE)
