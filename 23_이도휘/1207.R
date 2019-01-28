#preview#
#-과적합: 기존의 데이터는 잘 설명하지만 새로운 데이터는 설명하지 못함
#-모형검증의 필요성: 과적합을 줄이기 위해 기존 데이터의 일부를 떼어내 반복검증함
#-홀드아웃 데이터: 보통 20%의 데이터를 떼어냄

#14장
data(Wells, package='carData')
library(rpart) ## rpart
library(rpart.plot) ## rpart.plot
library(dplyr) ## %>%
library(caret) ## createFolds
library(tidyr) ## gather
library(ipred) ## bagging
library(randomForest) 

fit <- rpart(switch ~ ., Wells)
rpart.plot(fit)

dat <- Wells %>% sample_n(2800)
nrow(Wells)

rpart.plot(rpart(switch ~ ., dat))

#•모형 분산(model variance) : 동일한 모집단에서 추출된 자료에 대한 적합된 모형의 변동성. 
#-◦모형 분산은 모집단의 실제 모형, 표본 크기, 그리고 적합하는 모형의 함수이다.
#- 모형 분산은 데이터가 많을 수록 작아진다.

#배깅
library(ipred)
sample(10, replace=TRUE) 
hist(sample(10, replace=TRUE), breaks = seq(0.5, 10.5, 1))

datSpam <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data', sep=',', header=FALSE)
summary(datSpam)

datSpam <- datSpam %>% mutate(V58 = factor(V58))

iTrains <- createFolds(y = datSpam$V58, k = 5, list = TRUE, returnTrain = TRUE)

iTrains[[1]]
iTrains[[2]]

datSpamTrain <- datSpam %>% slice(iTrains[[1]])

nrow(datSpam)
nrow(datSpamTrain)

fit1 <- rpart(V58 ~ ., datSpamTrain)
rpart.plot(fit1)
bagging1 <- bagging(V58 ~ ., datSpamTrain)

datSpamTrain <- datSpam %>% slice(iTrains[[2]])
rpart.plot(rpart(V58 ~ ., datSpamTrain))
fit2 <- rpart(V58 ~ ., datSpamTrain)
rpart.plot(fit2)
bagging2 <- bagging(V58 ~ ., datSpamTrain)

datSpamTrain <- datSpam %>% slice(iTrains[[3]])
rpart.plot(rpart(V58 ~ ., datSpamTrain))
fit3 <- rpart(V58 ~ ., datSpamTrain)
rpart.plot(fit3)
bagging3 <- bagging(V58 ~ ., datSpamTrain)


test <- datSpam %>% slice(1:10)

print(predict(fit1, newdata= test)[,2], digits=2)
print(predict(fit2, newdata= test)[,2], digits=2)
print(predict(fit3, newdata= test)[,2], digits=2)

print(predict(bagging1, newdata= test, type='prob')[,2], digits=2)
print(predict(bagging2, newdata= test, type='prob')[,2], digits=2)
print(predict(bagging3, newdata= test, type='prob')[,2], digits=2)

library(randomForest)
fit <- randomForest(v58 ~ ., datSpam)
predict(fit, newdata=datSpam)

#14-2장 인공신경망
#rclass 14_03_DT_vs_NN.R 파일 이용
library(rpart)
library(ggplot2)

aa <- rnorm(100, 10, 1)
ab <- rnorm(100, 0, 1)
ba <- rnorm(100, 0, 1)
bb <- rnorm(100, 10, 1)

dat0 <- data.frame(x1 = rep(c('a', 'a', 'b', 'b'), each=100),
                   x2 = rep(c('a', 'b', 'a', 'b'), each=100),
                   y = c(aa, ab, ba, bb))

ggplot(data=dat0, aes(x=x1, y=y)) + 
  facet_grid(x2 ~ .) + 
  geom_boxplot(width=0.2)

rpart(y ~ x1 + x2, data = dat0)

aa <- rnorm(100, 20,1)
ab <- rnorm(100, 10, 1)
ba <- rnorm(100, 0, 1)
bb <- rnorm(100, 10, 1)

dat1 <- data.frame(x1 = rep(c('a', 'a', 'b', 'b'), each=100),
                   x2 = rep(c('a', 'b', 'a', 'b'), each=100),
                   y = c(aa, ab, ba, bb))

ggplot(data=dat1, aes(x=x1, y=y)) + 
  facet_grid(x2 ~ .) + 
  geom_boxplot(width=0.2)

rpart(y ~ x1 + x2, data = dat1)

library(nnet)
fit <- nnet(y ~ x1 + x2, linout=TRUE, size =10, data = dat0)
predict(fit)

fit <- nnet(y ~ x1 + x2, linout=TRUE, size =2, data = dat0)
predict(fit)
table(predict(fit))

fit2 <- nnet(y ~ x1 + x2, linout=TRUE, size=10, data = dat1)
predict(fit2)
table(predict(fit2))

#추정해야 할 모수가 늘어나면(or 모형복잡도가 커지면) 데이터가 작을 때 분산은 커지는 단점이 있고, 데이터가 많아지면 편향이 작아지는 장점이 있다.
#회귀나무 설정 값 조절

library(rpart.plot)
library(dplyr)

x1 <- rnorm(1000, 0, 1)
x2 <- rnorm(1000, 0, 1)

y <- x1 - x2 + rnorm(1000, 0, 1)


dat = data.frame(x1=x1, x2=x2, y=y)

fitDT <- rpart(y ~ x1 + x2, data = dat %>% slice(1:800))

rpart.plot(fitDT)

fitNN <- nnet(y ~ x1 + x2, linout=TRUE, size=10, data=dat %>% slice(1:800))
#plot(fitNN)

mean((dat$y[1:800] - predict(fitNN))^2)
mean((dat$y[1:800] - predict(fitDT))^2)

mean((dat$y[801:1000] - predict(fitNN, newdata=dat %>% slice(801:1000)))^2)
mean((dat$y[801:1000] - predict(fitDT, newdata=dat %>% slice(801:1000)))^2)

##nnet
data(Wells, package='carData')
summary(Wells)
nnet(switch ~ ., maxit=1000, data=Wells, size=20)
