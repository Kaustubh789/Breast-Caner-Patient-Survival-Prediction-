library(ggplot2)
library(caTools)
library(neuralnet)
library(dplyr)
df <- read.csv("FinalSEERDatasetForMinor.csv",stringsAsFactors = FALSE)

impute_status <- function(v){
  out <- v
  for(i in 1: length(v)){
    if(v[i]=="Alive"){
      out[i] <- 1
    }else{
      out[i] <- 0
    }
  }
  return(out)
}

df$Status <- as.numeric(impute_status(df$Status))

maxs <- apply(df,2,max)
mins <- apply(df,2,min)

scaled.data <- scale(df,center = mins,scale = maxs-mins)

scaled <- as.data.frame(scaled.data)

set.seed(101)
#TRAIN-TEST SPLIT

split <- sample.split(df$Status,SplitRatio = 0.9)

train <- subset(scaled,split == T)
test <- subset(scaled,split == F)

#n <- names(df)

#f <- as.formula(paste("Status ~",paste(n[!n %in% "Status"],collapse = "+")))

nn.seer <- neuralnet(Status ~ T.Stage+N.Stage+Stage.Cancer+Grade+A.Stage+
                       Tumor.Size,
                     data = train,hidden = c(3,2),linear.output = FALSE,
                     stepmax = 1e08)

plot(nn.seer)

predicted.nn.seer.values <- compute(nn.seer,test[,4:9])

predictions <- sapply(predicted.nn.seer.values$net.result,round)

table(predictions,test$Status)
