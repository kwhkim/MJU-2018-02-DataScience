# Variance of Decision Tree

library(rpart)
library(rpart.plot)
library(dplyr)
datSpam1 <- datSpam %>% slice(-sample(1:nrow(.), 10))
fit1 <- rpart(V58 ~ ., data = datSpam1)
rpart.plot(fit1)

datSpam2 <- datSpam %>% slice(-sample(1:nrow(.), 10))
fit2 <- rpart(V58 ~ ., data = datSpam2)
rpart.plot(fit2)

datSpam3 <- datSpam %>% slice(-sample(1:nrow(.), 10))
fit3 <- rpart(V58 ~ ., data = datSpam3)
rpart.plot(fit3)


datSpam1 <- datSpam %>% slice(-sample(1:nrow(datSpam), 10))
fit1 <- rpart(V58 ~ ., data = datSpam1, control=rpart.control(cp=0.005))
rpart.plot(fit1)

datSpam2 <- datSpam %>% slice(-sample(1:nrow(datSpam), 10))
fit2 <- rpart(V58 ~ ., data = datSpam2, control=rpart.control(cp=0.005))
rpart.plot(fit2)

datSpam3 <- datSpam %>% slice(-sample(1:nrow(datSpam), 10))
fit3 <- rpart(V58 ~ ., data = datSpam3, control=rpart.control(cp=0.005))
rpart.plot(fit3)
