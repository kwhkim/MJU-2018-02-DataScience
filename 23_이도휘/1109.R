if (a==1) {
  
  
} else {
  
  
  
  
}

ifelse(a==1, {
  print("a=1")
  "right"}, "wrong")

ifelse <- function(a) a+1
if <- function(a) a+1

a=1

if(a==1) {
  "up"
} else {
  if(a==2) {
    "down"
  } else {
      if(a==3) {
        "stay"
      }
    }
}


a=50
switch(as.character(a),
       "1"="up"
       "2"="down"
       "3"="stay",
       "not defined")

x=c(1,5,10)
s=rep(NA, 3)
for{i in seg_along(x)} {
  #akd
  #jdk
  s[1]= x[i]^2
}

# 10 % %7 : 10 %% 7

n=s
sum_one_to_n = function(n) {
  s=0 
for (i in 1:n) { 
 s = s + i 
} 
  return(s)
}

a <- sum_one_to_n(n=5)
b <- sum_one_to_n(n=10)
c <- sum_one_to_n(n=15)

sum_a_to_b(a=3, b=7)

n=10
s=0
for(i in 1:n) {
  s=s+i
}
print(s)

#연습문제
sumToN = function(n) { 
  s=0 
  for (i in 1:n) { 
    s = s + i 
  } 
  print(s) 
  return(s)
} 

#함수와 인자의 클래스
*
methods(print)
print(a) #class(a)
print.data.frame(a)

*
sum_one_to_n = function(n) {
  paste("one to", n)
}
sum_one_to_n("lalala")

# w10 linear regression 선형회귀
print(getwd())
dat <- read.csv(file='C://Users//gfdss//Desktop//datascience//homework1//MJU-2018-02-DataScience//00_Instructor//W10_LinearRegression//LR_weight_n100.csv', header=T, row.names=1)
t.test(dat$height)
t.test(dat$weight)

hist(dat$weight)
stem(dat$weight, scale=1)

t.test(dat$weight, conf.level = 0.99)

*모평균 차이

library(dplyr)
datM <- dat %>% filter(gender == "M")
datF <- dat %>% filter(gender == "F")
t.test(datM$height) #168 (161, 175)
t.test(datF$height) #165 (163, 168)

t.test(height ~ gender, dat) #(-10, 4) => 신뢰구간이 둘다 -이거나 둘다 +이어야 함

x <- rnorm(10, 160, 10)
y <- rnorm(10, 150, 10)

x
y

# 데이터 살펴보기
library(ggplot2)
ggplot(dat, aes(x=height, y=weight)) + geom_point() + labs(title="01. 데이터 살펴보기")

# 회귀분석
fitLm <- lm(weight ~ I(height^2), dat)
plot(fitLm)
print(fitLm)
print(summary(fitLm))
coef(fitLm)
library(car)
library(lmtest)
confint(fitLm) #신뢰구간 구하기

ggplot(dat, aes(x=height, y=weight)) + geom_point() + 
  geom_smooth(method="lm") + labs(title='03. 회귀분석 결과 시각화 b')

newdata <- data.frame(height=c(160,170,180))
predict(fitLm, newdata)
predict(fitLm, newdata, se=T)

read.csv("http://data.princeton.edu/wws509/datasets/salary.dat", sep='')
salary <- read.csv("http://data.princeton.edu/wws509/datasets/salary.dat")

sl ~ yd
