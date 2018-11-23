library(ggplot2)
library(readr)

#데이터 불러오기
df <- read.table("salary.dat.txt", header = TRUE, sep = "")

#데이터 살펴보기
head(df)
str(df)

#1. 계수를 추정하세요.
cor(df$yd, df$sl)

df_lm <- lm(sl~yd, df) #계수추정
coef(df_lm)
summary(df_lm)

#2. 계수의 95% 신뢰구간을 구하세요.
confint(df_lm, level=0.95)

#3. 데이터와 적합한 회귀선을 시각화하세요.
ggplot(df, aes(x= df$yd, y= df$sl)) +
  geom_point(size=3, colour='red') +
  xlab("Number of years in current rank") +
  ylab("Academic year salary") +
  geom_smooth(method='lm', level = 0.95) +
  ggtitle("회귀선 시각화")

#4. 선형회귀분석 결과에 대해 진단하세요.
##두 변수간 상관계수는 0.67이다. 두 변수간 선형관계가 보이기는 하지만 선형관계의 강도는 강하지 않다.
##총변동에서 차지하는 설명되는 변동분의 비율인 결정계수를 통해서 우리는 yd 변수에 대해서 sl변수가 약 44% 설명한다고 볼 수 있다.
##sl = 17502 + 391*yd, 회귀분석을 통해서 yd 변수가 1 단위 증가했을 때 sl 변수는 390 달러 증가하는 것으로 나타났다. 그리고 t값은 6.5를 보유하고 있기 때문에 통계적으로 유의미하다.

summary(mfrow = c(2, 2))
par(mfrow=c(1,2))
