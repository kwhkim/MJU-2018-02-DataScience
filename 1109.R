



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