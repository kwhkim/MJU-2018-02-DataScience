



a <- 1


a=1
switch(a,
       "1"="up",
       "2"="down")

x=c(1,5,10)
s=rep(NA,3)
for (i in seq_along(x)){
  s[i]=x[i]>5}
  
n=10
s=0
for(i in 1:n){s=s+i}
print(s)




print(getwd())
## [1] "G:/git/MJU-2018-02-DataScience/00_Instructor/W10_LinearRegression"
dat <- read.csv(file='./00_Instructor/W10_LinearRegression/LR_weight_n100.csv', header=T, row.names=1)
dat <- read.csv(file='./W10_LinearRegression/LR_weight_n100.csv', header=T, row.names=1)
dat <- read.csv(file='/00instructor/w10_LinerRegression.csv', header=T, row.names=1)
t.test(dat$height)




## data:  dat$height
## t = 204.3, df = 99, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  166.2817 169.5433
## sample estimates:
## mean of x 
##  167.9125
t.test(dat$weight)


#salary
#sl ~ yd
#E[sl|yd]=베타0+베타1+yd
#계수,95%신뢰구간,시각화,진단
#http://data.princetion.edu/wws509/datasets/salary.dat

library(ggplot2)
## [1] "G:/git/MJU-2018-02-DataScience/00_Instructor/W10_LinearRegression"
#dat <- read.csv(file='./00_Instructor/W10_LinearRegression/LR_weight_n100.csv', header=T, row.names=1)
#dat <- read.csv(file='./W10_LinearRegression/LR_weight_n100.csv', header=T, row.names=1)
dat <- read.csv(file='./LR_weight_n100.csv', header=T, row.names=1)
t.test(dat$height)
dat <- read.csv(file='./LR_weight_n100.csv', header=T, row.names=1)
ggplot(dat, aes(x=height, y=weight)) + geom_point() + labs(title="01. 데이터 살펴보기")

# 02. 회귀분석
fitLm <- lm(weight ~ height, dat)
print(fitLm)
plot(weight ~ height, dat, main='03. 회귀분석 결과 시각화 a'); abline(fitLm)

## b
ggplot(dat, aes(x=height, y=weight)) + geom_point() + 
  geom_smooth(method="lm") + labs(title='03. 회귀분석 결과 시각화 b')

## c
ggplot(dat, aes(x=height, y=weight)) + geom_point() +
  geom_abline(intercept = fitLm$coefficients["(Intercept)"],
              slope = fitLm$coefficients["height"]) + 
  labs(title='03. 회귀분석 결과 시각화 c')

# 04. 회귀분석 가정 검토
#par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
plot(fitLm, main='04. 회귀분석 가정 검토') # plot(fitLm, which=1:6)


install.packages("foreign")
library(foreign)
write.dta()