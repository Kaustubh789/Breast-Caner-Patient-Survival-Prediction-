library(ggplot2)
library(caTools)
library(randomForest)
library(dplyr)
df <- read.csv("FinalSEERDatasetForMinor.csv",stringsAsFactors = FALSE)
df$Status <- as.factor(df$Status)

race.plot <- ggplot(df,aes(x=factor(Race))) + geom_bar(aes(fill=Status),color='black',
                                                         position='dodge') + theme_bw() +
  scale_fill_manual(values = c("green","red"))

#TRAIN-TEST SPLIT
set.seed(101)
sample <- sample.split(df$Status,SplitRatio = 0.95)

train <- subset(df,sample == T)
test <- subset(df,sample == F)

rf.model <- randomForest(Status ~ T.Stage+N.Stage+Stage.Cancer+Grade+Tumor.Size,data = train,importance=TRUE)

rf.preds <- predict(rf.model,test)

table(rf.preds,test$Status)

