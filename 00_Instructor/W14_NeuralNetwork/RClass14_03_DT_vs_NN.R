# 의사결정나무(decision tree)가 그리디 알고리즘(greedy algorithm)으로
# 변수와 분기점을 결정하기 때문에 다음과 같이 명백하게 드러나는
# 데이터의 구조를 찾아내지 못한다.

library(rpart)
library(ggplot2)

aa <- rnorm(100, 10, 1)
ab <- rnorm(100, 0, 1)
ba <- rnorm(100, 0, 1)
bb <- rnorm(100, 10, 1)

dat0 <- data.frame(x1 = rep(c('a', 'a', 'b', 'b'), each=100),
                  x2 = rep(c('a', 'b', 'a', 'b'), each=100),
                  y = c(aa, ab, ba, bb))

ggplot(data=dat0, aes(x=x1, y=y)) + 
  facet_grid(x2 ~ .) + 
  geom_boxplot(width=0.2)

rpart(y ~ x1 + x2, data = dat0)

aa <- rnorm(100, 20,1)
ab <- rnorm(100, 10, 1)
ba <- rnorm(100, 0, 1)
bb <- rnorm(100, 10, 1)

dat1 <- data.frame(x1 = rep(c('a', 'a', 'b', 'b'), each=100),
                  x2 = rep(c('a', 'b', 'a', 'b'), each=100),
                  y = c(aa, ab, ba, bb))

ggplot(data=dat1, aes(x=x1, y=y)) + 
  facet_grid(x2 ~ .) + 
  geom_boxplot(width=0.2)

rpart(y ~ x1 + x2, data = dat1)

library(nnet)
fit <- nnet(y ~ x1 + x2, linout=TRUE, size =10, data = dat0)

predict(fit)
table(predict(fit))

#참고 : 회귀나무의 설정 값을 조절하기
#rpart(y ~ x1 + x2, data = dat0, 
#      control=rpart.control(cp=0.00))

library(rpart.plot)
library(dplyr)

x1 <- rnorm(100, 0, 1)
x2 <- rnorm(100, 0, 1)

y <- x1 - x2 + rnorm(100, 0, 1)

dat = data.frame(x1=x1, x2=x2, y=y)

fitDT <- rpart(y ~ x1 + x2, data = dat %>% slice(1:80))

rpart.plot(fitDT)

fitNN <- nnet(y ~ x1 + x2, linout=TRUE, size=10, data=dat %>% slice(1:80))
#plot(fitNN)

mean((dat$y[1:80] - predict(fitNN))^2)
mean((dat$y[1:80] - predict(fitDT))^2)

mean((dat$y[81:100] - predict(fitNN, newdata=dat %>% slice(81:100)))^2)
mean((dat$y[81:100] - predict(fitDT, newdata=dat %>% slice(81:100)))^2)

