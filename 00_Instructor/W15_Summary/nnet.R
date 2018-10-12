library(dplyr)

iTest <- c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,
           321,151,7 ,19,242,123,221,230,45,277,54,228,156,298,92,121,181,
           243,191,68,118,55,264,153 ,125,102,75,32,303,317,106,252,149,70,
           316,293,40,310,90,100,258,15)

dat <- read.csv('00_Instructor/W11_MultipleRegression/BaseballHitters.csv', row.names=1)
#sum(is.na(datTrain$salary87))
#sum(is.na(datTest$salary87))
datTrain <- dat %>% slice(-iTest) %>% filter(!is.na(salary87))
datTest <- dat %>% slice(iTest) %>% filter(!is.na(salary87))

nrow(datTrain); sum(complete.cases(datTrain))
nrow(datTest); sum(complete.cases(datTest))

summary(datTrain)

datTrain0 <- datTrain 
datTest0 <- datTest
# %>% select(-firstName, -lastName, -league87, -team87)

datTrain <- datTrain0 %>% 
  mutate(avAB = careerAB/years,
         avH = careerH/years,
         avHR = careerHR/years,
         avR = careerR/years,
         avRBI = careerRBI/years,
         avW = careerRBI/years) 

datTest <- datTest0 %>% 
  mutate(avAB = careerAB/years,
         avH = careerH/years,
         avHR = careerHR/years,
         avR = careerR/years,
         avRBI = careerRBI/years,
         avW = careerRBI/years) 

colnames(datTrain)

library(nnet); library(caret)
fit <- nnet(salary87 ~ . - firstName - lastName - league87 - team87,
            data = datTrain,
            size=100, linout=TRUE,
            MaxNWts = 100000,
            maxit = 100000)

RMSE(predict(fit)[,1], datTrain$salary87)
RMSE(predict(fit, newdata=datTest)[,1], datTest$salary87)

pred1 = data.frame(pred=predict(fit)[,1], sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest)[,1], sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')



library(nnet); library(caret)
fit <- nnet(salary87 ~ AB86 + H86 + HR86 + R86 + RBI86 + W86 + years + 
              careerAB + careerH + careerHR + careerR + careerRBI + careerW + 
              avAB + avH + avHR + avR + avRBI + avW + 
              league86 + division86 + PO86 + A86 + E86,
            data = datTrain,
            size=100, linout=TRUE,
            MaxNWts = 100000,
            maxit = 100000)

RMSE(predict(fit)[,1], datTrain$salary87)
RMSE(predict(fit, newdata=datTest)[,1], datTest$salary87)

pred1 = data.frame(pred=predict(fit)[,1], sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest)[,1], sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')

# ====

library(nnet); library(caret)
fit <- nnet(salary87 ~ AB86 + H86 + HR86 + R86 + RBI86 + W86 + years + 
              careerAB + careerH + careerHR + careerR + careerRBI + careerW + 
              avAB + avH + avHR + avR + avRBI + avW + 
              league86 + division86 + PO86 + A86 + E86,
            data = datTrain,
            size=100, linout=TRUE,
            MaxNWts = 100000,
            maxit = 100000)

RMSE(predict(fit)[,1], datTrain$salary87)
RMSE(predict(fit, newdata=datTest)[,1], datTest$salary87)

pred1 = data.frame(pred=predict(fit)[,1], sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest)[,1], sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')



# ====

library(nnet)
fit <- nnet(salary87 ~ . - firstName - lastName - league87 - team87 - 
               careerAB - careerH - careerHR - careerR - careerRBI - careerRBI,
             data = datTrain,
             size=100, linout=TRUE,
             MaxNWts = 10000,
             maxit = 100000)

RMSE(predict(fit), datTrain$salary87)
RMSE(predict(fit, newdata=datTest), datTest$salary87)

length(predict(fit, newdata=datTest)); length(datTest$salary87)

predict(fit, newdata=datTest, type='raw')


pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)





#install.packages('doMC')
#library(doMC) 
#registerDoMC(cores = 5) 


#https://stackoverflow.com/questions/44774516/parallel-processing-in-r-in-caret
library(doParallel)
cls = makeCluster(3)
registerDoParallel(cls)

if (file.exists('avNNet_s100_Sal87.RData')) {
  load('avNNet_s100_Sal87.RData')
} else {
  fit <- avNNet(salary87 ~ . - firstName - lastName - league87 - team87,
                data = datTrain,
                size=100, linout=TRUE,
                MaxNWts = 20000,
                maxit = 10000)  # allowParallel = TRUE by default
  save(fit, file='avNNet_s100_Sal87.RData')
}
                
RMSE(predict(fit), datTrain$salary87)
#predict(fit, newdata=datTest, type='raw')
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)

#save(fit, file='avNNet_s200_Sal87.RData')

####

fit <- avNNet(log(salary87) ~ . - firstName - lastName - league87 - team87,
              data = datTrain,
              size=200, linout=TRUE,
              MaxNWts = 20000,
              maxit = 10000)  # allowParallel = TRUE by default
#load('avNNet_s200_logSal87.RData')

RMSE(exp(predict(fit)), datTrain$salary87)
terms(fit)
#predict(fit, newdata=datTest, type='raw')
RMSE(exp(predict(fit, newdata=datTest)), datTest$salary87)

pred1 = data.frame(pred=exp(predict(fit)), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=exp(predict(fit, newdata=datTest)), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)

#save(fit, file='avNNet_s200_logSal87.RData')

###

fit <- avNNet(log(salary87) ~ . - firstName - lastName - league87 - team87,
              data = datTrain,
              size=100, linout=TRUE,
              MaxNWts = 20000,
              maxit = 10000)  # allowParallel = TRUE by default
load('avNNet_logSal87.RData')

RMSE(exp(predict(fit)), datTrain$salary87)
#predict(fit, newdata=datTest, type='raw')
RMSE(exp(predict(fit, newdata=datTest)), datTest$salary87)

pred1 = data.frame(pred=exp(predict(fit)), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=exp(predict(fit, newdata=datTest)), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)


####

getModelInfo('avNNet')



datTrain2 <- droplevelsDF(datTrain)
#scale(datTrain)

fit <- avNNet(salary87 ~ . - firstName - lastName - league87 - team87,
              data = datTrain,
              size=5, linout=TRUE,
              decay=0.01,
              bag=TRUE,
              MaxNWts = 20000,
              maxit = 10000#,allowParallel=FALSE
              )  # allowParallel = TRUE by default

Sys.time()



droplevelsDF= function(df) {
  as.data.frame(lapply(df, function(x) 
    if (class(x) == 'factor') droplevels(x) else x))
}

datTrain <- droplevelsDF(datTrain)
datTest <- droplevelsDF(datTest)

fit <- train(log(salary87) ~ .- firstName - lastName - league87 - team87 -
               careerAB - careerH - careerHR - careerR - careerRBI - careerRBI,
             data = datTrain,
             method = 'avNNet', 
             tuenGrid = expand.grid(size=c(50, 100),
                                    decay = c(0),
                                    bag=c(TRUE)),
             linout=TRUE,
             MaxNWts = 20000,
             maxit = 10000,
             preProcess = c('center', 'scale'),#allowParallel=FALSE
             allowParallel=FALSE
             )

fit <- train(log(salary87) ~ .- firstName - lastName - league87 - team87 -
               careerAB - careerH - careerHR - careerR - careerRBI - careerRBI,
             data = datTrain,
             method = 'avNNet', 
             tuenGrid = expand.grid(size=c(50),
                                    decay = c(0),
                                    bag=c(FALSE)),
             linout=TRUE,
             MaxNWts = 20000,
             maxit = 10000,
             preProcess = c('center', 'scale'),#allowParallel=FALSE
             allowParallel=TRUE
)

save(fit, file='train_avNNet_logSal87.RData')
Sys.time()

RMSE(predict(fit), datTrain$salary87)
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)

####

library(randomForest)
fit <- randomForest(salary87 ~ . - firstName - lastName - league87 - team87,
                    data = datTrain)
RMSE(predict(fit), datTrain$salary87)
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)


####

fit <- randomForest(log(salary87) ~ . - firstName - lastName - league87 - team87,
                    data = datTrain)
RMSE(exp(predict(fit)), datTrain$salary87)
RMSE(exp(predict(fit, newdata=datTest)), datTest$salary87)

pred1 = data.frame(pred=exp(predict(fit)), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=exp(predict(fit, newdata=datTest)), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)



library(ipred)
fit <- bagging(salary87 ~ . - firstName - lastName - league87 - team87,
               data = datTrain)
RMSE(predict(fit), datTrain$salary87)
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)

####












#datTrainX <- data.matrix(datTrain %>% select(-firstName, -lastName, -league87, -team87, -salary87))
datTrainX <- model.matrix(~., datTrain %>% select(-firstName, -lastName, -league87, -team87, -salary87))

datTrainY <- datTrain$salary87


fit <- train(x=datTrainX, y=datTrainY,
             method = 'avNNet',
             tuneGrid = expand.grid(data.frame(size=c(10,100), decay=0, bag=TRUE)),
             MaxNWts = 10000,
             preprocessing = c('center', 'scale'))


fit <- train(salary87 ~ . - firstName - lastName - league87 - team87,
             data = datTrain2,
             method = 'avNNet', 
             preprocessing = c('center', 'scale'))

str(fit)
class(fit)
# look at xlelves...???
# xlevels seems to show factor levels...
# 

library(caret)
datTrain3 <- datTrain2 %>% select(-firstName, -lastName, -league87, -team87)
fit <- train(salary87 ~ .,
             data = datTrain3,
             method = 'avNNet',
             preProcess = c('center', 'scale'))

predict(fit, newdata = datTrain3)

  
library(randomForest)
fit <- randomForest(salary87 ~ . - firstName - lastName - league87 - team87,
                    data = datTrain2)
RMSE(predict(fit), datTrain$salary87)
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)


fit <- randomForest(log(salary87) ~ . - firstName - lastName - league87 - team87,
                    data = datTrain2)
RMSE(exp(predict(fit)), datTrain$salary87)
RMSE(exp(predict(fit, newdata=datTest)), datTest$salary87)

pred1 = data.frame(pred=exp(predict(fit)), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=exp(predict(fit, newdata=datTest)), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)



library(ipred)
fit <- bagging(salary87 ~ . - firstName - lastName - league87 - team87,
               data = datTrain2)
RMSE(predict(fit), datTrain$salary87)
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)





library(ipred)
fit <- bagging(salary87 ~ . - firstName - lastName - league87 - team87 -
                 careerAB - careerH - careerHR - careerR - careerRBI - careerRBI,
               data = datTrain2)
RMSE(predict(fit), datTrain$salary87)
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)



droplevelsDF= function(df) {
  as.data.frame(lapply(df, function(x) 
    if (class(x) == 'factor') droplevels(x) else x))
}

datTrain <- droplevelsDF(datTrain)
datTest <- droplevelsDF(datTest)

if (file.exists('00_Instructor/W15_Summary/train_avNNet_Sal87.RData')) {
  load('00_Instructor/W15_Summary/train_avNNet_Sal87.RData')
} else {
  fit <- train(salary87 ~ . - firstName - lastName - league87 - team87 -
                 careerAB - careerH - careerHR - careerR - careerRBI - careerRBI,
               data = datTrain,
               method = 'avNNet', 
               tuenGrid = expand.grid(size=c(50, 100),
                                      decay = c(0),
                                      bag=c(TRUE)),
               linout=TRUE,
               MaxNWts = 20000,
               maxit = 10000,
               preProcess = c('center', 'scale'),#allowParallel=FALSE
               allowParallel=FALSE
  )
  save(fit, file='train_avNNet_Sal87.RData')
}

RMSE(predict(fit), datTrain$salary87)
#predict(fit, newdata=datTest, type='raw')
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)



#

if (file.exists('train_avNNet_Sal87.RData')) {
  load('train_avNNet_Sal87.RData')
  } else {
  fit <- train(salary87 ~ . - firstName - lastName - league87 - team87 -
                 careerAB - careerH - careerHR - careerR - careerRBI - careerRBI,
               data = datTrain,
               method = 'avNNet', 
               tuneGrid = expand.grid(size=c(50, 100),
                                      decay = c(0),
                                      bag=c(TRUE)),
               linout=TRUE,
               MaxNWts = 20000,
               maxit = 10000,
               preProcess = c('center', 'scale'),#allowParallel=FALSE
               allowParallel=FALSE
  )
  save(fit, file='train_avNNet_Sal87.RData')
  }




RMSE(predict(fit), datTrain$salary87)
#predict(fit, newdata=datTest, type='raw')
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + 
  geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted') + 
  facet_wrap(~ data)


#====

fit <- train(salary87 ~ . - firstName - lastName - league87 - team87 -
               careerAB - careerH - careerHR - careerR - careerRBI - careerRBI,
             data = datTrain,
             method = 'rf', 
             tuneLength = 10,
             #preProcess = c('center', 'scale'),
             #tuneLength = 3, 
             trControl = trainControl(method='cv')
)

fit

predict(fit)

RMSE(predict(fit), datTrain$salary87)
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')




#====

#====

fit <- train(salary87 ~ . - firstName - lastName - league87 - team87 -
               team86 - PO86,
             data = datTrain,
             method = 'rf', 
             #preProcess = c('center', 'scale'),
             tuneLength = 10, 
             trControl = trainControl(method='cv')
)


fit <- train(salary87 ~ AB86 + H86 + HR86 + R86 + RBI86 + W86 + years,
             data = datTrain,
             method = 'rf', 
             #preProcess = c('center', 'scale'),
             tuneLength = 10, 
             trControl = trainControl(method='boot')
)

fit

predict(fit)

RMSE(predict(fit, newdata=datTrain), datTrain$salary87)
RMSE(predict(fit, newdata=datTest), datTest$salary87)

pred1 = data.frame(pred=predict(fit), sal87=datTrain$salary87, data='train')
pred2 = data.frame(pred=predict(fit, newdata=datTest), sal87=datTest$salary87, data='test')
datPlot = rbind(pred1, pred2)

ggplot(datPlot, aes(x=sal87, y=pred, col=data)) + geom_point() + 
  geom_abline(intercept= 0, slope=1, linetype='dotted')



