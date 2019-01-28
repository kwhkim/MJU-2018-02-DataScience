data(Wells, package='carData')
Wells
head(Wells)
#선형회귀
lm(switch ~ distance, data= Wells)
#로지스틱 회귀(일반화 선형 회귀, GLM)
fit <- glm(switch ~ distance, data=Wells,
    family=binomial(link="logit"))
summary(fit)


#모형평가
fit0 <- glm(switch ~ distance, family=binomial(link="logit"), 
            data=Wells)
pred <- predict(fit0, type='link')
pred <- ifelse(pred>0, "yes", "no")
pred <- factor(pred, levels = c('no', 'yes'))
table(pred, Wells$switch)


library(caret)
confusionMatrix(pred, Wells$switch, positive='yes')


#13장
data(Wells, package='carData')
library(rpart)
tmode12 <- rpart(switch ~ distance + arsenic, data=Wells)
summary(tmode12)
plot(tmode12)
text(tmode12)
library(rpart.plot)
rpart.plot(tmode12)

fit0 <- rpart(switch ~ ., data=Wells,
              control=rpart.control(maxdepth = 2))
rpart.plot(fit0)

fit1 <- rpart(switch ~ ., data=Wells,
              control=rpart.control(cp=0.005, minsplit=20, minbucket=7))
pdf('DT.pdf', height=6, width=10)
rpart.plot(fit1)
dev.off()

iTrains <- createDataPartition(y = Wells$switch, p = 0.8)
iTrains[[1]]
iTrains[[2]]