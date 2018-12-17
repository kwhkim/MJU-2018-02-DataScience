data <- read.csv("00_Instructor/W11_MultipleRegression/BaseballHitters.csv")
head(data)
library(rpart) 
library(rpart.plot) 
library(dplyr) 
library(caret) 
library(tidyr) 
library(ipred) 
library(randomForest) 
library(termstrc)
install.packages("Metrics")
library(Metrics)

# Data Preprocessing
traindata <- data[-c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7 ,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153 ,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15),-c(1:3,25:26)]
head(traindata,10)
Traindata <- traindata %>% 
  filter(!is.na(salary87))

testdata <- data[c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7 ,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153 ,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15),-c(25:26)]

Testdata <- testdata %>% 
  filter(!is.na(salary87))

# Bagging
fit1 <- list(NA, NA, NA, NA, NA)
for (i in 1:5) {
  hitTrain <- Traindata %>% slice(iTrains[[i]])
  fit1[[i]] <- bagging(salary87 ~ ., data = hitTrain)
}
summary(fit1)

nsamp=10
hitTrainTest <- Traindata %>% slice(1:nsamp)
iTrains <- createFolds(y = Testdata$salary87, k = 5, list = TRUE, returnTrain = TRUE)

pred <- data.frame(i=factor(1:nsamp, levels=1:nsamp),
                   pred1 = predict(fit1[[1]], hitTrainTest, type='prob'), 
                   pred2 = predict(fit1[[2]], hitTrainTest, type='prob'),
                   pred3 = predict(fit1[[3]], hitTrainTest, type='prob'),
                   pred4 = predict(fit1[[4]], hitTrainTest, type='prob'),
                   pred5 = predict(fit1[[5]], hitTrainTest, type='prob'))

mse(Testdata$salary87, hitTrainTest$salary87 )

# Random Forest
fit2 <- list(NA, NA, NA, NA, NA)
for (i in 1:5) {
  hitTrain <- Traindata %>% slice(iTrains[[i]])
  fit2[[i]] <- randomForest(salary87 ~ ., data = hitTrain)
}

summary(fit2)

pred2 <- data.frame(i=factor(1:nsamp, levels=1:nsamp),
                   pred1 = predict(fit2[[1]], hitTrainTest, type='response'), 
                   pred2 = predict(fit2[[2]], hitTrainTest, type='response'),
                   pred3 = predict(fit2[[3]], hitTrainTest, type='response'),
                   pred4 = predict(fit2[[4]], hitTrainTest, type='response'),
                   pred5 = predict(fit2[[5]], hitTrainTest, type='response'))

mse(Testdata$salary87, hitTrainTest$salary87)
# bagging과 randomforest의 MSE는 같다.
