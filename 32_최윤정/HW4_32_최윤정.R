##HW4_32_최윤정
#http://data.princeton.edu/wws509/datasets/#salary의 자료에서 예측변수 yd와 결과변수 sl로 선형회귀분석을 하세요.
#1. 계수를 추정하세요.
#2. 계수의 95% 신뢰구간을 구하세요.
#3. 데이터와 적합한 회귀선을 시각화하세요.
#4. 선형회귀분석 결과에 대해 진단(diagnostics)하세요.

#데이터 불러오기

#http://data.princeton.edu/wws509/datasets/salary.dat

library(readr)
data <- read_table2("http://data.princeton.edu/wws509/datasets/salary.dat")


head(data)
str(data)
View(data)

#예측변수 yd와 결과변수 sl로 선형회귀분석하기 
dat_lm <- lm(sl~yd, data)
dat_lm

summary(dat_lm)

#1. 계수를 추정하세요.
coef(dat_lm)  
#sl = 17502.2574 + 390.6451*yd



#2. 계수의 95% 신뢰구간을 구하세요.
confint(dat_lm, level=0.95)
#yd:  269.3063~511.9839


#3. 데이터와 적합한 회귀선을 시각화하세요.
ggplot(data, aes(x = yd, y = sl)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.95) + ggtitle('Salary 회귀분석 시각화') 



#4. 선형회귀분석 결과에 대해 진단(diagnostics)하세요.
#summary(dat_lm) 의 결과에서 p-value: 4.102e-08 이므로, 0.05보다 작기때문에 유의미하다. 
#summary(dat_lm) 의 결과에서 Multiple R-squared:  0.4554 이므로 yd가 sl을 약 45% 정도 예측할 수 있는 설명력을 가진다고 할 수 있다. 

