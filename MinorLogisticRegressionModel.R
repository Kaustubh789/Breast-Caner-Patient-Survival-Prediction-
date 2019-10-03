library(corrgram)
library(ggplot2)
library(dplyr)
library(plotly)
library(caTools)
df <- read.csv("BCancerMinorFileFinal.csv")
df <- select(df,-S.no,-id)

corrgram(df,order = TRUE,lower.panel = panel.density,upper.panel = panel.pie,text.panel = 
           panel.txt)

pl <- ggplot(df,aes(x=diagnosis,y=radius_mean)) + geom_jitter(size=2,alpha=0.5,color="black",
                                                        fill="yellow") + theme_classic() + xlab("Diagnosis") + ylab("Mean Radius Of Tumor") + ggtitle("Mean Radius vs Diagnosis")

pl <- ggplotly(pl)

set.seed(101)

sample <- sample.split(df$diagnosis,SplitRatio = 0.7)

train <- subset(df,sample == TRUE)
test <- subset(df,sample == FALSE)

#CREATING MODEL
log.reg.model <- glm(diagnosis ~ .,family = binomial(link = 'logit'),data = train)

summary(log.reg.model)

new.log.reg.model <- step(log.reg.model)

summary(new.log.reg.model)


fitted.probs <- predict(log.reg.model,test,type = 'response')

fitted.res <- ifelse(fitted.probs>0.5,"M","B")

log.reg.misClassError <- mean(fitted.res != test$diagnosis)

print(1-log.reg.misClassError)

#CONFUSION MATRIX
table(test$diagnosis,fitted.probs>0.5)

log.reg.acc <- (100+58)/(100+7+6+58)
print(log.reg.acc)
