# 01. 데이터 살펴보기
library(readr)
salary <- read_table2("http://data.princeton.edu/wws509/datasets/salary.dat")
View(salary)

library(ggplot2)
ggplot(salary, aes(x=sl, y=yd)) + geom_point() + labs(title="01. 데이터 살펴보기")

# 02. 95% 신뢰구간 분석
t.test(salary$sl) # 23797.65 (22150.27, 25445.04)
t.test(salary$yd) # 16.11538 (13.26947, 18.96130)


# 03. 회귀분석
fitLm <- lm(yd ~ sl, salary)
print(fitLm)

print(summary(fitLm))

# 04. 회귀분석 결과를 시각화하기
## a
plot(yd ~ sl, salary, main='04. 회귀분석 결과 시각화 a'); abline(fitLm)

## b
ggplot(salary, aes(x=sl, y=yd)) + geom_point() + 
  geom_smooth(method="lm") + labs(title='04. 회귀분석 결과 시각화 b')

## c
ggplot(salary, aes(x=sl, y=yd)) + geom_point() +
  geom_abline(intercept = fitLm$coefficients["(Intercept)"],
              slope = fitLm$coefficients["sl"]) + 
  labs(title='04. 회귀분석 결과 시각화 c')

# 05. 회귀분석 가정 검토
#par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
plot(fitLm, main='05. 회귀분석 가정 검토') # plot(fitLm, which=1:6)

## a. x와 y의 관계가 선형적인가? 선형적이다.
## b. 잔차가 정규성을 띄는가? 정규분포선을 따르는 그래프이므로 거의 정규성을 띈다.
## c. 잔차가 등분산인가? 아니다.
## d. 특이값(계수 추정에 큰 영향을 미치는 값)이 존재하는가? 존재한다. 22, 34, 24 등이 중심으로부터 극단적으로 치우쳤다.