data <- salary
library(ggplot2)
# 1. 데이터 살펴보기
ggplot(data= data, aes(x= yd, y= sl))+ 
  geom_point()+
  labs(title= '데이터 살펴보기')
cor(data$yd, data$sl)     # 두 변수 간 양의 상관관계가 어느정도 있음

# 2. 회귀분석
fitLm <- lm(sl ~ yd, data)
print(fitLm)
print(summary(fitLm))

# 2-1. 계수
coef(fitLm)

# 2-2. 95% 신뢰도로 추정
confint(fitLm)  # 두 계수 모두 신뢰구간 내에 존재하므로 채택할 수 있다.


# 3. 회귀분석 결과 시각화
plot(sl ~ yd, data, main= '회귀분석 결과 시각화'); abline(fitLm)

ggplot(data= data, aes(x= yd, y= sl))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(title= '회귀분석 결과 시각화')

ggplot(data=data, aes(x= yd, y= sl))+
  geom_point()+
  geom_abline(intercept = fitLm$coefficients["(Intercept)"],
              slope = fitLm$coefficients["yd"])+
  labs(title= '회귀분석 결과 시각화')

# 4. 회귀분석 가정 검토
plot(fitLm, main='04. 회귀분석 가정 검토')

# a. x와 y의 관계가 선형적인가? -잔차가 점점 커지다가 작아지므로 선형적이라고 할 수 없다
# b. 잔차가 정규성을 띄는가? - 대체적으로 직선위에 점들이 존재하므로 어느정도 정규성을 띄고 있음을 알 수 있다.
# c. 잔차가 등분산인가? - 점들이 무작위로 분포되어 있으므로 등분산을 만족하는 것으로 보인다.
# d. 특이값(계수 추정에 큰 영향을 미치는 값)이 존재하는가?- 쿡의 거리는 레버리지와 잔차에 비례하므로 두 값이 큰 오른쪽 상단과 오른쪽 하단에 이상치가 존재하게 된다. 여기서는 24, 22, 34 등 숫자가 쓰여져 있는 것이 이상치라고 볼 수 있다.