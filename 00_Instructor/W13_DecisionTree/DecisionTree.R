library(dplyr)
Wells_cut <- 
  Wells %>% summarise(min = min(distance), max = max(distance), by = (max-min)/20)  
Wells <- 
  Wells %>% mutate(distCut = cut(distance, 
                                 breaks=seq(Wells_cut$min, Wells_cut$max, Wells_cut$by))) 
ggplot(data=Wells, aes(x=distCut, col=switch, fill=switch)) + geom_bar()



ggplot(data=Wells, aes(x=distCut, col=switch, fill=switch)) + geom_bar(position = "fill" ) + 
  geom_rug(aes(x=distCut, y=0.5), position=position_jitter(width=0.2),    alpha=0.02, sides='b', col='black')


ggplot(data=Wells, aes(x=distCut, col=switch, fill=switch)) + geom_bar(position = "fill" ) +
  facet_grid(association~.) + 
  theme(axis.text.x=element_text(angle=60)) +
  geom_abline(intercept=0.5, slope=0, linetype='dotted')



Wells_cut <- 
  Wells %>% summarise(min = min(education), max = max(education), by = (max-min)/5)  
Wells <- 
  Wells %>% mutate(eduCut = cut(education, 
                                breaks=seq(Wells_cut$min, Wells_cut$max, Wells_cut$by))) 

ggplot(data=Wells, aes(x=distCut, col=switch, fill=switch)) + geom_bar(position = "fill" ) +
  facet_grid(eduCut~association) + 
  theme(axis.text.x=element_text(angle=60)) +
  geom_abline(intercept=0.5, slope=0, linetype='dotted')

library(rpart)
tmodel2 <- rpart(switch ~ distance+ association, data=Wells)
summary(tmodel2)
plot(tmodel2)
text(tmodel2)

library(rpart)
tmodel3 <- rpart(switch ~ distance+ association + education, data=Wells)
summary(tmodel3)
plot(tmodel3)
text(tmodel3)

library(rpart)
tmodel4 <- rpart(switch ~ ., data=Wells %>% select(switch, arsenic, distance, education, association))
summary(tmodel4)
plot(tmodel4)
text(tmodel4)

lmodel4 <- glm(switch ~ ., data=Wells %>% select(switch, arsenic, distance, education, association),
               family=binomial(link='logit'))
summary(lmodel4)
plot(allEffects(lmodel4))

lmodel5 <- glm(switch ~ .^2, data=Wells %>% select(switch, arsenic, distance, education, association),
               family=binomial(link='logit'))
summary(lmodel5)
plot(allEffects(lmodel5), multiline=TRUE)


predT4 <- predict(tmodel4, type='class')
predL4 <- predict(lmodel4, type='response') > 0.5
predL5 <- predict(lmodel5, type='response') > 0.5

table(predT4, predL4)
table(predT4, predL5)


`control = rpart.control(cp=0.001, minsplit=1000, minbucket=1000, maxdepth=5)`
`cp`: complexity parameter
`maxdepth`: maximum depth

`party::ctree`

train(x, y, method='rpart', tuneLength =, 
      trControl = trainControl())

```{r}
library(randomForest)
#library(caret)
rfModel <- randomForest(Wells %>% select(arsenic, distance, education, association), Wells$switch,
                        importance = TRUE,
                        ntrees = 1000)

rfModel <- randomForest(switchyes ~ . , 
                        data=model.matrix(~., Wells %>% select(switch, arsenic, distance, education, association)))

rfModel <- randomForest(switch ~ . , 
                        data=Wells %>% select(switch, arsenic, distance, education, association))

plot(getTree(rfModel, k=1, labelVar =TRUE))

importance(rfModel)
varImpPlot(rfModel) 

```





