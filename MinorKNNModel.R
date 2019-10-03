library(caTools)
library(ggplot2)
library(class)

df.knn <- read.csv("BCancerMinorFileFinal.csv",stringsAsFactors = FALSE)

standardised.features.knn <- scale(df.knn[2:31])

#print(var(standardised.features.knn))

final.df.knn <- cbind(standardised.features.knn,df.knn[1])

#TRAIN-TEST SPLIT
set.seed(101)
sample.knn <- sample.split(final.df.knn$diagnosis,SplitRatio = 0.7)

train.knn <- subset(final.df.knn,sample.knn == T)
test.knn <- subset(final.df.knn,sample.knn == F)

#BUILDING KNN MODEL
knn.predicted.diagnosis <- knn(train.knn[1:30],test.knn[1:30],train.knn$diagnosis,k=1)
#print(knn.predicted.diagnosis)

print(mean(test.knn$diagnosis != knn.predicted.diagnosis))

#SELECTING OPTIMUM K VALUE
knn.predicted.diagnosis.trial <- NULL
error.rate.trial <- NULL

for(i in 1:20){
  set.seed(101)
  knn.predicted.diagnosis.trial <- knn(train.knn[1:30],test.knn[1:30],train.knn$diagnosis,k=i)
  error.rate.trial[i] <- mean(test.knn$diagnosis != knn.predicted.diagnosis.trial)
}

#VISUALISING ERROR FOR DIFFERENT K VALUES

k.values <- 1:20
knn.error.df <- data.frame(error.rate.trial,k.values)

knn.plot <- ggplot(knn.error.df,aes(x=k.values,y=error.rate.trial)) + geom_point(size=2) + theme_classic()

knn.plot <- knn.plot + geom_line(lty = "solid",alpha=0.2,color="red",size=1) + xlab("K-Values") + ylab("Error rate")

knn.plot <- knn.plot + ggtitle("Plot of K-Value vs Error rate")
print(knn.plot)

final.knn.model.predictions <- knn(train.knn[1:30],test.knn[1:30],train.knn$diagnosis,k=6)
knn.misClassError <- mean(test.knn$diagnosis != final.knn.model.predictions)

knn.acc <- 1-knn.misClassError

print(knn.acc)
