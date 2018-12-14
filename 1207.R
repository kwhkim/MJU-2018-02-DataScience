#교차검증 - 다섯조각을 내서 20퍼센트로 검증 각각조각당 5번??

install.packages("carData")
library(carData)
data(wells, package='carData')
install.packages("dplyr")
library(dplyr)
install.packages("caret")
library(rpart)
library(rpart.plot)
install.packages("caret")
rpart.plot(rpart(switch ~ .,dat))
install.packages("randomForest")

iTrains <- createFolds(y = datSpam$V58, k = 5, list = TRUE, returnTrain = TRUE)
install.packages("dplyr")
library(dplyr)
install.packages("caret")
library(caret)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
datSpam <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data', sep=',', header=FALSE)
head(datSpam)

iTrains <- createFolds(y = datSpam$V58, k = 5, list = TRUE, returnTrain = TRUE)
#datSpamTest <- datSpam %>% slice(1811:1820)
nsamp = 10
datSpamTest <- datSpam %>% slice(1:nsamp)
#datSpamTest <- datSpam %>% sample_n(nsamp)
fit <- list(NA, NA, NA, NA, NA)
for (i in 1:5) {
  datSpamTrain <- datSpam %>% slice(iTrains[[i]])
  fit[[i]] <- rpart(V58 ~ ., data = datSpamTrain)
  rpart.plot(fit[[i]])
}
pred <- data.frame(i=factor(1:10, levels=1:10),
                   pred1 = predict(fit[[1]], newdata = datSpamTest, type='prob')[,2], 
                   pred2 = predict(fit[[2]], newdata = datSpamTest, type='prob')[,2],
                   pred3 = predict(fit[[3]], newdata = datSpamTest, type='prob')[,2],
                   pred4 = predict(fit[[4]], newdata = datSpamTest, type='prob')[,2],
                   pred5 = predict(fit[[5]], newdata = datSpamTest, type='prob')[,2])
predG <- pred %>% gather("key", "value", 2:5) 
predG %>% ggplot(aes(x=i, y=value, col=key, group=key)) +
  geom_point() + 
  geom_line()
#predG %>% ggplot(aes(x=i, y=value, col=key, group=key)) +
#  facet_wrap(~key) + 
#  geom_point() + 
#  geom_line()
```





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



#(p+1)xh+(h+1)=모수의 수
#10개일 경우 (2+1)ㅌ10+11=41



bagging 마지막부분에 데이터 준비되어 있음
11폴더 베이스볼 데이터 파일
87년도를 제외한 86년도 커리어 성적을 예측하기위한 데이터
87년도 셀러리를 예측하는 모델을 만드는데 베링 뉴런 등등 두가지 선택해서 비교하기 80%트레이닝 20%검증 모형평가 검
랜덤