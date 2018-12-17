library(dplyr)
library(readxl)
install.packages("rpart")
library(rpart)
library(rpart.plot)
library(caret)
library(tidyr)
library(ipred)
library(randomForest)
data <- read.csv("C:/Users/Jang/Desktop/Coding Program/r/project/MJU-2018-02-DataScience/00_instructor/W11_MultipleRegression/BaseballHitters.csv")

summary(data)
df <- data

boxplot(df)
str(df)
boxplot(df$careerAB)
boxplot(df$careerH)
boxplot(df$salary87)
boxplot(df$years)

df$careerAB <- ifelse(df$careerAB>10000 ,NA,df$careerAB)
df$careerH <- ifelse(df$careerH>3000 ,NA,df$careerH)
df$salary87 <- ifelse(df$salary87>1500 ,NA,df$salary87)

df <- na.omit(df)
table(is.na(df))




ggplot(df, aes(x=careerH, y=salary87)) + geom_point() + labs(title="a11")

fitLm1 <- lm(careerH ~ salary87, df)
print(fitLm1)
print(summary(fitLm1))

plot(careerH ~ salary87, df, main='a22'); abline(fitLm1)

ggplot(df, aes(x=careerH, y=salary87)) + geom_point() + 
  geom_smooth(method="lm") + labs(title='a33')

ggplot(df, aes(x=careerAB, y=salary87)) + geom_point() + labs(title="b11")

fitLm2 <- lm(careerAB ~ salary87, df)
print(fitLm2)
print(summary(fitLm2))

plot(careerAB ~ salary87, df, main='b22'); abline(fitLm2)

ggplot(df, aes(x=careerAB, y=salary87)) + geom_point() + 
  geom_smooth(method="lm") + labs(title='b33')


ggplot(df, aes(x=years, y=salary87)) + geom_point() + labs(title="c11")

fitLm3 <- lm(years ~ salary87, df)
print(fitLm3)
print(summary(fitLm3))

plot(years ~ salary87, df, main='c22'); abline(fitLm3)

ggplot(df, aes(x=years, y=salary87)) + geom_point() + 
  geom_smooth(method="lm") + labs(title='c33')

mean(fitLm1$residuals^2)
mean(fitLm2$residuals^2)
mean(fitLm3$residuals^2)

#years는 salary87과  양의 상관관계를 가진다.



df <- read.csv("C:/Users/Jang/Desktop/Coding Program/r/project/MJU-2018-02-DataScience/00_instructor/W11_MultipleRegression/BaseballHitters.csv", sep=',', header=FALSE)
head(df)

table(is.na(df))
df <- na.omit(df)
table(is.na(df))
head(df)


iTrains <- createFolds(y = df$V26, k = 5, list = TRUE, returnTrain = TRUE)

#datSpamTest <- datSpam %>% slice(1811:1820)
nsamp = 10
datSpamTest <- df %>% slice(1:nsamp)
#datSpamTest <- datSpam %>% sample_n(nsamp)

fit <- list(NA, NA, NA, NA, NA)
for (i in 1:5) {
  datSpamTrain <- df %>% slice(iTrains[[i]])
  fit[[i]] <- rpart(V26 ~ ., data = datSpamTrain)
  rpart.plot(fit[[i]])
}
