# :은 상호작용이라는 의미
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    -141.5100    45.7177  -3.095  0.00258 ** 
## height            1.2152     0.2795   4.348 3.42e-05 ***
## genderM         -34.5312    52.5081  -0.658  0.51235    
## height:genderM    0.3223     0.3174   1.016  0.31238    
## ---
# 키가 0일때 여자는 남자에 비해 -34.5312
# height:genderM = 기울기 차이값
# 07. 계수 시각화#
install.packages("coefplot")
library(coefplot)



#g 계수비교
#표준편차는 분산을 제곱근한것
library(car)

dat <- read.csv(file="C:/Users/Jang/Desktop/Coding Program/r/project/MJU-2018-02-DataScience/LR_weight_n100.csv", header=T, row.names=1)
fitMLm01 <- lm(weight ~ height + gender, dat)


#b=베타
#체중 = b0+b1x1
#gender='F','M'
#        0   1
#b0=여자의 체중
#b1=남자체중-여자체중
#남자체중 = 여자체중 +b1*1
lm(weight~gender,dat)
#여자의 체중 = 57.17
#genderM = 남자의체중 - 여자의 체중



#gender='F','M''중성'
#genderm 0   1   2

#남자여자 차이와 중성 남자차이가 같다는 가정이 들어간다.
#아래처럼 적어야함
#gender=      'F','M''중성'
#genderm      0   1   2
#gender중성    0  0   1
#  b0+b1x1+b2x2
#b2= 중성에서 여자 체중을 뺸것 
#중성은 b0+b2





library(ggplot2)
Prestige <- carData::Prestige

ggplot(Prestige, aes(x=type, y=income)) + geom_boxplot()

fitPrestige <- lm(income ~ type, data=Prestige)
summary(fitPrestige)

modelmat <- model.matrix(~ type, data=Prestige)
modelmat[c(1,28,31),] 


#gov pfof라 inter에 1
#
print(getwd())

datIQ <- read.csv("C:/Users/Jang/Desktop/Coding Program/r/project/MJU-2018-02-DataScience/00_Instructor/W11_MultipleRegression/data_iq.csv", row.names=1)

#income = 1000+200edu(교육연수)

#누락변수
#ip=100+신발크기 - 신발크기는 iq와 상관관계가 없다
#age가 들어가야함
#예측은 가능하다??????????????????
#원인 결과는 알 수 없다.

#
fit <- lm(income ~ type + education, Prestige)
coef(fit)
#

#문제 
#상호작용:한변수의 효과가 다른변수의 값에 따라 달라진다.
