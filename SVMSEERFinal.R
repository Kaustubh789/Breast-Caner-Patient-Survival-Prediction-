library(e1071)
library(caTools)

df <- read.csv("FinalSEERDatasetForMinor.csv",stringsAsFactors = FALSE)

df$Status <- factor(df$Status)

#TRAIN-TEST SPLIT
set.seed(101)
sample <- sample.split(df$Status,0.9)

train <- subset(df,sample==TRUE)
test <- subset(df,sample==FALSE)

#BUILDING SVM MODEL
s.model <- svm(Status ~ .,data = train)
print(summary(s.model))
predicted.values <- predict(s.model,test[1:13])
table(predicted.values,test$Status) #UNTUNED RESULTS

tuned.res <- tune(svm,train.x = Status ~ .,data = train,kernel = "radial",
                  ranges = list(cost=c(1.01,0.99),gamma=c(0.077,0.07691)))

print(summary(tuned.res))

#cost gamma
#1.01 0.077

tuned.s.model <- svm(Status ~ .,data = train,cost = 1.01,gamma=0.077)
tuned.preds <- predict(tuned.s.model,test[1:13])
table(tuned.preds,test$Status)
