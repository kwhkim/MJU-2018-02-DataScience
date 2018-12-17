library(ggplot2)
library(dplyr)
library(rpart)
library(ipred)
library(rpart.plot)

#데이터 불러오기
raw <- read.csv('00_Instructor/W11_MultipleRegression/BaseballHitters.csv')
dat <- subset(raw, select= -c(league87,team87))
dat <- dat[-c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15),]

#결측치 확인 : salary87에만 결측치가 존재.
colSums(is.na(dat))  

#결측치 제외
dat <- dat %>% filter(!is.na(salary87))
colSums(is.na(dat))

#검증 데이터
dat_a <- raw[c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7 ,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153 ,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15),]

dat_a <- dat_a %>% filter(!is.na(salary87))


##선형회귀분석
#선형회귀 모형A
fitA <- lm(salary87~.,data=dat %>% select(-c('X','firstName','lastName')))
predict(fitA, newdata=dat_a, se.fit=T)
mean((dat$salary87 - predict(fitA))^2)       #MSE=51262.76

#선형회귀 모형b
fitB <- lm(salary87~H86,data=dat)
predict(fitB, newdata=dat_a, se.fit=T)
mean((dat$salary87 - predict(fitB))^2)     #MSE=144303.3

#선형회귀 모형C
fitC <- lm(salary87~AB86+H86+W86+careerR+careerW+team86+position86+PO86,data=dat)
predict(fitC, newdata=dat_a, se.fit=T)
mean((dat$salary87 - predict(fitC))^2)     #MSE=59597.94

#선형회귀 모형D
fitD <- lm(salary87~AB86+H86+W86+careerR+careerW+PO86+team86*position86,data=dat)
predict(fitD, newdata=dat_a, se.fit=T)
mean((dat$salary87 - predict(fitD))^2)     #MSE=4528.309

#선형회귀 모형E
fitE <- lm(salary87~AB86+H86*W86+careerR*careerW+PO86+team86*position86,data=dat)
predict(fitE, newdata=dat_a, se.fit=T)
mean((dat$salary87 - predict(fitE))^2)     #MSE=3680.111


###MSE값이 가장 작은 fitE가 가장 적합.



##배깅
Bag_a <- bagging(salary87 ~ ., data = dat)
predict(Bag_a, newdata = dat_a, type='prob')
mean((dat_a$salary87 - predict(Bag_a, newdata = dat_a, type='prob'))^2) #MSE=161375.5

Bag_b <- bagging(salary87 ~ years, data = dat)
predict(Bag_b, newdata = dat_a, type='prob')
mean((dat_a$salary87 - predict(Bag_b, newdata = dat_a, type='prob'))^2) #MSE=149605

Bag_c <- bagging(salary87 ~ years + careerHR, data = dat)
predict(Bag_c, newdata = dat_a, type='prob')
mean((dat_a$salary87 - predict(Bag_c, newdata = dat_a, type='prob'))^2) #MSE=119315.7

###MSE값이 가장 작은 Bag_c가 가장 적합.
