#random sampling from normal distribution
x <- rnorm(100, 0, 1) 
curve(dnorm(x, 0, 1), xlim=c(-3, 5))
 
n=100
x <- rnorm(n, 0, 1)
e <- rnorm(n, 0, 1)
y <- -1+0*x + e
plot(y ~ x)
dat <- data.frame(x=x, y=y)
fit <- lm(y ~ x, dat)
coef(fit)

confint(fit) #1. 데이터가 많을 때 2. 오차의 표준편차가 작을 때 정규분포 그래프는 얇아진다.(평균을 향해)

#예측오차
newdat <- data.frame(x=2)
predict(fit, newdat)
predict(fit, newdat, interval='confidence', level=0.95)
predict(fit, newdat, interval='prediction', level=0.95)

plot(fit) 
#잔차(residual): r=y-y_hat(실제 값에서 예측 값을 뺀 것)
#오차: 모형에서 평균을 뺀 것

#예2 성별로 회귀분석
library(ggplot2)
getwd()
dat <- read.csv(file='C://Users//gfdss//Desktop//datascience//homework1//MJU-2018-02-DataScience//00_Instructor//W11_MultipleRegression//LR_weight_n100.csv', header=T, row.names=1)

# 01. 데이터 살펴보기
ggplot(dat, aes(x= gender, y=weight)) + geom_point() + labs(title="01. 데이터 살펴보기")

# 02. 회귀분석
fitLm <- lm(weight ~ gender, dat)
print(fitLm)

head(model.matrix(~gender, dat))
print(summary(fitLm))

lm(weight ~ 1, data=dat) #여기서 1은 intercept를 의미함

fitA <- lm(weight ~ 1, data=dat)
fitB <- lm(weight ~ gender + 1, data=dat)

coef(fitA)
coef(fitB)
mean(dat$weight)
library(dplyr)
dat %>% group_by(gender) %>% summarise(mean(weight))

#두 단순 선형 모형 평가하기
fitA <- lm(weight ~ height, data=dat)
summary(fitA)
plot(weight ~ height, data=dat)

mean((dat$weight - predict(fitA))^2)
mean((dat$weight - predict(fitB))^2)

library(caret)
RMSE(dat$weight, predict(fitA)) #sqrt(mean((dat$weight - predict(fitA))^2))
RMSE(dat$weight, predict(fitB)) 

MAE(dat$weight, predict(fitA)) 
MAE(dat$weight, predict(fitA))

summary(fitA)$r.squared # 결정계수: 클수록 좋음 cor(dat$weight, predict(fitA))^2
summary(fitB)$r.squared

#다중회귀
fitA <- lm(weight ~ height, data=dat)
fitB <- lm(weight ~ gender, data=dat)
fitC <- lm(weight ~ gender + height + gender:height, data=dat)

coef(fitC)
summary(fitC)

ls()
rm(ifelse)
lm(weight ~ ., dat)

dat$genderM <- ifelse(dat$gender == "M", 1, 0)
lm(weight ~ 1 + I(genderM + height), data=dat)

update(fit, . ~ . -gender)
update(fit, weight ~ height)

fitA = lm(weight ~ height + gender, dat)
fitB = lm(weight ~ height + gender, dat)

coef(fitA)
coef(fitB)

RMSE(dat$weight, predict(fitA))
RMSE(dat$weight, predict(fitB))

library(car)
AIC(fitA) #작을수록 좋음
AIC(fitB)
