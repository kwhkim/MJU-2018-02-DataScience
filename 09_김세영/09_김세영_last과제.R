``
#00_Instructor/W11_MultipleRegression/BaseballHitters.csv 자료를 사용하여 salary87을 예측하는 모형을 만드세요. 이때 변수 league87와 team87는 사용할 수 없습니다. 모형 적합에는 c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7 ,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153 ,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15)번째 자료는 제외하세요. 이 자료는 모형을 검증할 때 사용할 것입니다.

data <- read.csv("00_Instructor/W11_MultipleRegression/BaseballHitters.csv")

str(data)

library(dplyr)

data <- data %>% select(-league87, -team87)

#모형적합 데이터 추출 
dat1 <- data[-c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15), ]

#모형검증 데이터 추출 
dat2 <- data[c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15), ]


#결측치 확인 
table(is.na(dat1))
table(is.na(dat2))

#결측치 제거
library(dplyr)

dat1 <- dat1 %>% filter(!is.na(salary87))
dat2 <- dat2 %>% filter(!is.na(salary87))

### 선형회귀분석 

library(rpart) ## rpart
library(rpart.plot) ## rpart.plot

#어떤 변수가 연봉에 영향을 미치는지 파악하기
str(dat1)
colnames(dat1)

lr <- lm(salary87~.,data=dat1 %>% select(-c('X','firstName','lastName')))

lr
coef(lr)
summary(lr)


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   252.32670  411.77937   0.613 0.540943    
# AB86           -2.24003    0.73944  -3.029 0.002881 ** 
#   H86             8.96594    2.62271   3.419 0.000808 ***
#   HR86            2.12103    7.62608   0.278 0.781291    
# R86            -4.20106    3.63827  -1.155 0.250030    
# RBI86           0.98013    3.06869   0.319 0.749865    
# W86             6.84141    2.01486   3.395 0.000874 ***
#   years           4.43437   13.13198   0.338 0.736072    
# careerAB       -0.21726    0.15101  -1.439 0.152281    
# careerH         0.08384    0.75900   0.110 0.912191    
# careerHR        0.22710    1.79501   0.127 0.899488    
# careerR         1.92198    0.83604   2.299 0.022873 *  
#   careerRBI       0.60271    0.74941   0.804 0.422509    
# careerW        -0.80257    0.34332  -2.338 0.020710 *  
#   league86N     -42.74334  147.48676  -0.290 0.772356    
# division86W  -353.54445  213.79589  -1.654 0.100262    
# team86Bal.   -377.93679  361.36352  -1.046 0.297285    
# team86Bos.   -342.48476  380.22249  -0.901 0.369147    
# team86Cal.   -344.16118  222.17942  -1.549 0.123456    
# team86Chi.   -222.99355  216.91232  -1.028 0.305566    
# team86Cin.   -269.80347  151.11572  -1.785 0.076189 .  
# team86Cle.   -487.97893  370.28460  -1.318 0.189537    
# team86Det.   -633.11349  360.47548  -1.756 0.081046 .  
# team86Hou.   -165.71968  147.48913  -1.124 0.262952    
# team86K.C.   -226.49383  212.82916  -1.064 0.288924    
# team86L.A.     13.71079  154.28366   0.089 0.929304    
# team86Mil.   -527.08519  372.87281  -1.414 0.159530    
# team86Min.   -159.56888  206.58852  -0.772 0.441077    
# team86Mon.   -522.59112  266.63945  -1.960 0.051834 .  
# team86N.Y.   -392.23920  302.56285  -1.296 0.196806    
# team86Oak.   -115.30085  225.18359  -0.512 0.609373    
# team86Phi.   -432.84611  270.51164  -1.600 0.111652    
# team86Pit.   -618.58129  268.01721  -2.308 0.022348 *  
#   team86S.D.   -195.23458  140.94365  -1.385 0.168022    
# team86S.F.   -161.32304  141.81473  -1.138 0.257093    
# team86Sea.   -260.40334  204.99687  -1.270 0.205926    
# team86St.L.  -362.06426  271.50085  -1.334 0.184341    
# team86Tex.   -289.61282  211.27491  -1.371 0.172462    
# team86Tor.   -343.99826  373.05514  -0.922 0.357932    
# position861O   74.75694  303.58910   0.246 0.805826    
# position8623  308.70043  357.83020   0.863 0.389661    
# position862B  395.40622  221.49287   1.785 0.076225 .  
# position862S  703.20135  351.82252   1.999 0.047420 *  
#   position8632  332.78871  357.34848   0.931 0.353190    
# position863B  449.37846  217.00221   2.071 0.040064 *  
#   position863O -229.19696  362.61751  -0.632 0.528296    
# position863S  355.93385  258.12630   1.379 0.169947    
# position86C   320.69334  120.84284   2.654 0.008805 ** 
#   position86CD  167.56570  345.28872   0.485 0.628168    
# position86CF  280.07024  185.92670   1.506 0.134053    
# position86DH   93.20000  221.85188   0.420 0.675006    
# position86DO  134.39727  267.95867   0.502 0.616703    
# position86LF  278.86654  182.02969   1.532 0.127605    
# position86O1  232.44688  221.32209   1.050 0.295263    
# position86OD  334.16654  328.69813   1.017 0.310942    
# position86OF  315.04633  172.96516   1.821 0.070506 .  
# position86OS  127.14071  267.91564   0.475 0.635785    
# position86RF  287.50220  191.69111   1.500 0.135735    
# position86S3 1153.53624  404.05109   2.855 0.004906 ** 
#   position86SS  469.84996  227.51814   2.065 0.040611 *  
#   position86UT  249.86965  204.43578   1.222 0.223509    
# PO86            0.54075    0.21435   2.523 0.012675 *  
#   A86            -0.07133    0.50473  -0.141 0.887798    
# E86            -7.68273    5.41261  -1.419 0.157826

#위에서 통계적으로 유의한 항목(p-value < 0.05)
# AB86 0.002881 **
# H86 0.000808 ***
# W86 0.000874 ***
# careerR 0.022873 *  
# careerW 0.020710 *
# team86Pit. 0.022348 * 
# position862S 0.047420 *
# position863B 0.040064 *
# position86C 0.008805 **
# position86S3 0.004906 ** 
# position86SS 0.040611 *
# PO86 0.012675 * 

#가장 큰 영향을 미친 것 = H86(86년도의 안타 수)

#ggplot으로 살펴보기
library(ggplot2)
ggplot(dat1, aes(AB86, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(dat1, aes(H86, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(dat1, aes(W86, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(dat1, aes(careerR, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(dat1, aes(careerW, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(dat1, aes(PO86, salary87)) +geom_point()+geom_smooth(method=lm)
summary(lr)

#위에서 통계적으로 유의한 항목(p-value < 0.05)들만을 가지고 모형 만들기
lr1 <- lm(salary87 ~ AB86+H86+W86+careerR+careerW+team86+position86+PO86, data=dat1 %>% select(-c('X','firstName','lastName')))

summary(lr1)

#F-test 실행
anova(lr1,lr)#F-test 결과 유의하지 않은 변수를 제거하지 않은 처음 lr의 p값이 0.02277 으로 0.05보다 작으므로 유의한 관계가 있다고 할 수 있다. 따라서 해당 변수를 제거하지 않고 분석하도록 한다.

#예측에 사용할 모델 살펴보기 
summary(lr)
coef(lr)

#1~10번째 데이터에 대해 예측된 salary 값 
fitted(lr)[1:10]

#1~10번째 데이터에 대한 잔차
residuals(lr)[1:10]

#fitted 값과 잔차의 값의 합은 실제 데이터 값과 같음
fitted(lr)[1:5]+residuals(lr)[1:5]
baseData$salary87[1:5] 

#salary87을 예측하기 
sal <- predict(lr)
head(sal)

#예측한 값과 test 데이터의 실제 종속변수 값의 차이를 구하기(오차생성)
lr_error <- sal - dat2[["salary87"]]
lr_error
sqrt(mean(lr_error^2))# RMSE = 614.3508


### RandomForest

library(randomForest)

set.seed(1000) 

rf<-randomForest(salary87 ~ AB86+H86+HR86+R86+RBI86+W86+years+careerAB+careerH+careerHR+careerR+careerRBI+careerW+PO86+A86+E86, data=dat1, mtry=5, ntree=500)
rf

#예측 
rf1 <- predict(rf, newdata=dat1, type="response")

###MSE를 사용한 검정(RandomForest 결과 검정)
head(p2)

#예측한 값과 test 데이터의 실제 종속변수 값의 차이를 구하기(오차생성)

rf_error <- rf1 - dat2[["salary87"]]
rf_error
sqrt(mean(error2^2)) # RMSE = 607.3151

### 결론
# 다중선형회귀 모델의 RMSE는 614.3508 이고, RandomForest 모델의 RMSE 는 607.3151 이다.
# RMSE는 작을수록 정밀도가 높으므로, RandomForest 모델이 RMSE이 더 큰 값인 다중선형회귀 모델보다 예측 설명력이 높다.
