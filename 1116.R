git fetch upstream

x <- rnorm(100,0,1)
x <- rnorm(10,0,1)
curve(dnorm(x,0,1),xlim=c(-3,3))



n=1000
x <- rnorm(n,0,1)
e <- rnorm(n,0,0.5)
y <- 2*x+e
plot(y~x)

dat <- data.frame(x=x,y=y)
fit <- lm(y~x,dat)
coef(fit)
confint(fit)


n=100
x <- rnorm(n,0,1)
e <- rnorm(n,0,0.5)
y <- 1+0*x+e
plot(y~x)
dat <- data.frame(x=x,y=y)
fit <- lm(y~x,dat)
coef(fit)


newdat <- data.frame(x=2)
predict(fit,newdat)
#신뢰구간 95%
predict(fit,newdat,interval="confidence",level=0.95)
predict(fit,newdat,interval="prediction",level=0.95)


#잔차 = 실제 y와 예측된 y의 차이
# 오차 = e 실제 평균값과 데이터의 차이


#표준편차 분산을 나타내는것
#표준오차 추정값의 

getwd()


library(ggplot2)
library(dplyr)


##두 단순 선형 모형을 평가하기

#제곱근을 해주면 보기좋다??
#잔차를 모두 더하면 0이되기에 이것을 방지하기 위해 제곱근

dat <- read.csv(file='./LR_weight_n100.csv', header=T, row.names=1)

dat%>%
  group_by(gender)%>%
  summarise(mean(weight))
  
fitA <- lm(weight ~ height,data=dat)
fitB <- lm(weight ~ gender,data=dat)

plot(weight ~ height, data=dat)
mean((dat$weight - predict(fitA))^2)
## [1] 152.8368
mean((dat$weight - predict(fitB))^2)
## [1] 195.8862

summary(fitB)$r.squared
cor(dat$weight,predict(fitB))^2

fitA <- lm(weight~height,data= dat)
fitB <- lm(weight ~gender, data = dat)
fitC <- lm(weight ~ gender+height,data=dat)



