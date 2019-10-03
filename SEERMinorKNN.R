library(caTools)
library(dplyr)
library(ggplot2)
library(class)

df.knn <- read.csv("FinalSEERDatasetForMinor.csv",stringsAsFactors = FALSE)

standardised.features.knn <- scale(df.knn[1:13])

#print(var(standardised.features.knn))

final.df.knn <- cbind(standardised.features.knn,df.knn[14])
final.df.knn1 <- select(final.df.knn,Race,T.Stage,N.Stage,Stage.Cancer,Grade,Tumor.Size,Status)

#TRAIN-TEST SPLIT
set.seed(101)
sample.knn <- sample.split(final.df.knn1$Status,SplitRatio = 0.95)

train.knn <- subset(final.df.knn1,sample.knn == T)
test.knn <- subset(final.df.knn1,sample.knn == F)

#BUILDING KNN MODEL
knn.predicted.status <- knn(train.knn[1:6],test.knn[1:6],train.knn$Status,k=1)
#print(knn.predicted.status)

#print(mean(test.knn$Status != knn.predicted.status))

#SELECTING OPTIMUM K VALUE
knn.predicted.status.trial <- NULL
error.rate.trial <- NULL

for(i in 1:50){
  set.seed(101)
  knn.predicted.status.trial <- knn(train.knn[1:6],test.knn[1:6],train.knn$Status,k=i)
  error.rate.trial[i] <- mean(test.knn$Status != knn.predicted.status.trial)
}


#VISUALISING ERROR FOR DIFFERENT K VALUES

k.values <- 1:50
knn.error.df <- data.frame(error.rate.trial,k.values)

knn.plot <- ggplot(knn.error.df,aes(x=k.values,y=error.rate.trial)) + geom_point(size=2) + theme_classic()

knn.plot <- knn.plot + geom_line(lty = "solid",alpha=0.2,color="red",size=1) + xlab("K-Values") + ylab("Error rate")

knn.plot <- knn.plot + ggtitle("Plot of K-Value vs Error rate")
print(knn.plot)

final.knn.model.predictions <- knn(train.knn[1:6],test.knn[1:6],train.knn$Status,k=16)
knn.misClassError <- mean(test.knn$Status != final.knn.model.predictions)

knn.acc <- 1-knn.misClassError

print(knn.acc)

