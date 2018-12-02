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

# 물론 회귀나무로도 할 수 있다.
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

# Try new NN with the number of hidden nodes being 2.

n = 7 
# 여기서 sample size가 증가함에 따라 rmse가 갑자기 커지는 순간이 있다!
# 그렇다면 그 전까지는 overfitting이 일어나고 있었다는 의미가 된다?
# model compatibility를 생각해보면 어떨까?
# model A, model B를 비교해 본다고 할 때,
# model A에서 생성된 데이터를 model B로 적합시킬 때, 
# 샘플의 크기가 늘어남에 따라 training data 또는 test data에 대한
# rmse가 어떻게 변하는지 살펴보는 것이다!
x1 = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)
x4 = rnorm(n)
x5 = rnorm(n)
y = (x1+0.3)^2 + 3 + rnorm(n)
dat = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, y=y)

fit0 <- lm(y ~ x1+x2+x3+x4+x5, data=dat)
library(caret)
RMSE(dat$y, predict(fit0))

fit1 <- lm(y ~ I(x1^2), data=dat)
RMSE(dat$y, predict(fit1))


# 