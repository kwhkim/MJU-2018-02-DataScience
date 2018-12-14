library(readxl)
library(ggplot2)

dat <- salary


ggplot(dat,aes(x=yd, y=sl))+geom_point()+labs(title="데이터 살펴보기")
dat_a <- lm(yd~sl,dat)
print(dat_a)
print(summary(dat_a))

coef(dat_a)
confint(dat_a)


plot(yd~sl,dat,main='시각화 a');abline(dat_a)
ggplot(dat, aes(x=yd, y=sl))+geom_point()+
  geom_smooth(method = "lm")+labs(title='시각화b')

#yd와 sl은 양의 상관관계가 있다.