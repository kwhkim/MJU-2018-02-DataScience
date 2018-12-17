#32_최윤정_과제5

###00_Instructor/W11_MultipleRegression/BaseballHitters.csv 자료를 사용하여 salary87을 예측하는 모형을 만드세요. 이때 변수 league87와 team87는 사용할 수 없습니다. 모형 적합에는 c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7 ,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153 ,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15)번째 자료는 제외하세요. 이 자료는 모형을 검증할 때 사용할 것입니다.

###선형회귀모형, 배깅 결정나무, 랜덤 포레스트, 인공신경망 중에서 2개의 모형을 선택하여 적합니다.

###검증 데이터를 사용하여 MSE(Mean Squared Error)가 가장 작은 결과에 최고점이 부여될 것입니다.

###과제에 포함되어야 하는 사항

###모형을 적합하기 전에 데이터의 사전 처리(preprocessing) 과정
###훈련 데이터 셋을 사용하여 모형을 적합하고, 평가한 결과
###만약 여러 개의 모형을 적합했다면 이들 중 하나의 모형을 선택한 과정을 적어주세요.
###검증 데이터 셋을 사용한 MSE 결과




###데이터 파악하기
baseball <- read.csv("00_Instructor/W11_MultipleRegression/BaseballHitters.csv")

head(baseball)
str(baseball)

dim(baseball)


colnames(baseball)
# 컬럼명의 86: in 1986(year)
# AB86 (at-bats) 타수   / H86 (hits) 안타 / HR86 (home-runs) 홈런/  R86 (runs scored) 득점 
# RBI86 (runs batted-in) 타율 / W86 (walks) / years (number of years in the major leagues) 연차 
# careerAB 타수 / careerH 안타  /  careerHR 홈런  /careerR 득점 / careerRBI 타율/  careerW 
# league86  /  division86 / team86 / position86 / PO86     
# A86/ E86 / salary87 / league87 / team87

#PO86: number of put-outs in 1986 
#A86:  number of assists in 1986 
#E86: number of errors in 1986 
#salary87: player’s annual salary in $1000s at the beginning of the 1987 season 
#league87: player’s league at the beginning of the 1987 season 
#team87: player’s team at the beginning of the 1987 season 

#출처: 구글, Source: http://lib.stat.cmu.edu/datasets/baseball.data, with corrections from D. C. Hoaglin and P. F. Velleman, “A critical look at some analyses of Major League Baseball salaries,” American Statistician, 1995, 49: 277-285. 

View(baseball)



###데이터 정리하기


#문자 변수 제거 league86, division86, team86, position86 +사용할 수 없는 변수 league87와 team87 제거
base1 <- baseball[,-c(1,2,3,17,18,19,20,25,26)]

#모형 검증에 사용할 데이터 분리
test <- base1[c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7 ,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153 ,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15),]
test
dim(test)  
is.na(test)

sum(is.na(test)) 

test[!complete.cases(test),] #NA가 있는 행 살펴보기 

#salary 값에 NA가 존재하므로 해당행 삭제하기
testData <- na.omit(test)

sum(is.na(testData)) #NA 제거 완료 

#최종 검증에 사용할 데이터:testData

#모형 검증에 사용할 데이터 제거(분석할 데이터만 남기기)
base2 <- base1[-c(177,294,44,220,215,25,81,111,107,284,216,36,18,56,91,320,113,194,321,151,7 ,19,242,123,221,230,45,277,54,228,156,298,92,121,181,243,191,68,118,55,264,153 ,125,102,75,32,303,317,106,252,149,70,316,293,40,310,90,100,258,15),]

dim(base2)
head(base2)
str(base2)
View(base2)

is.na(base2)
sum(is.na(base2)) # NA 값 확인. 46개의 NA

base2[!complete.cases(base2),] #NA가 있는 행 살펴보기 

#salary 값에 NA가 존재하므로 해당행 삭제하기
baseData <- na.omit(base2)

sum(is.na(baseData)) #NA 제거 완료 

#최종 분석에 사용할 데이터: baseData
head(baseData)
str(baseData)
summary(baseData)
View(baseData)

###1.다중선형회귀분석 사용
# 다중상관계수(multiple correlation coefficient) : 여러 개의 독립변수가 모여 종속변수와 얼마나 강한 선형관계에 있는지를 측정한 수치.
#결정계수(coefficient of determination) : 회귀식의 적합도를 평가 할 수 있는 수치. 0 <= 결정계수 <= 1
#조정된 결정계수(adjusted coefficient of determination) : 결정계수는 독립변수의 수가 증가하면 그 값이 계속 증가하는 성질이 있다.
#조정된 결정계수는 독립변수의 수가 증가하더라도 그 값이 더 이상 증가하지 않는다. 
#표준오차(standard error of estimate, Se) : 표본자료가 추정된 회귀식 주위에 얼마나 오밀조밀하게 분포되어 있는지 그 정도를 측정한 수치. 작을수록 회귀식의 적합도는 높아지게 된다.
#관측수

#제곱평균 : 제곱합을 해당 자유도 나눈 값(분산)
#회귀제곱평균(MSR:mean square regression)
#잔차평균제곱(MSE:mean square error)
#회귀식의 유의성 검정 : F비와 유의한 F

#회귀계수와 관련 정보
#계수, 표준오차, t 통계량, P값, 신뢰구간

#잔차(residuals) : 종속변수의 실제값과 회귀식을 이용하여 추정한 예측치의 차이
#표준잔차 : 해당 잔체에서 잔차들의 평균을 뺀 후 잔차들의 표준편차로 나눈 값.




#어떤 변수가 연봉에 영향을 미치는지 파악하기
colnames(baseData)

model <- lm(salary87 ~ AB86+H86+HR86+R86+RBI86+W86+years+careerAB+careerH+careerHR+careerR+careerRBI+careerW+PO86+A86+E86, data=baseData)
model

coef(model)

summary(model)
#AB86 p-value 0.004946 
#H86  p-value 0.000225
#W86  p-value 0.002433
#careerR p-value 0.045754 
#careerW p-value 0.036287 
#PO86    p-value 0.000664
#위의 항목의 p값이 0.05보다 작기에 통계적으로 유의하다 라고 할수 있다. 
#가장 큰 영향을 미친 것은 H86(86년도의 안타 수) 이다.

#각 항목 그래프로 살펴보기
#plot

plot(baseData$AB86, baseData$salary87)
a <- lm(salary87 ~ AB86, data=baseData)
abline(coef(a))


plot(baseData$H86, baseData$salary87)
b <- lm(salary87 ~ W86, data=baseData)
abline(coef(b))

plot(baseData$W86, baseData$salary87)
c <- lm(salary87 ~ H86, data=baseData)
abline(coef(c))

plot(baseData$careerR, baseData$salary87)
d <- lm(salary87 ~ careerR, data=baseData)
abline(coef(d))

plot(baseData$careerW, baseData$salary87)
e <- lm(salary87 ~ careerW, data=baseData)
abline(coef(e))

plot(baseData$PO86, baseData$salary87)
f <- lm(salary87 ~ PO86, data=baseData)
abline(coef(f))

#ggplot으로 살펴보기
library(ggplot2)
ggplot(baseData, aes(AB86, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(baseData, aes(H86, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(baseData, aes(W86, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(baseData, aes(careerR, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(baseData, aes(careerW, salary87)) +geom_point()+geom_smooth(method=lm)
ggplot(baseData, aes(PO86, salary87)) +geom_point()+geom_smooth(method=lm)


summary(model)
#위 코드 실행 에서 아래의 항목들만 유의하기 때문에(summary의 결과 *표시 있는 항목) 나머지 변수는 제거한 모형을 만든다. 
#AB86 p-value 0.000726 
#H86  p-value 3.72e-05
#W86  p-value 0.000301
#careerR p-value 0.016570
#careerW p-value 0.020006
#PO86    p-value 0.000431



#유의하지 않은 변수 제거한 모형만들기
model2 <- lm(salary87 ~ AB86+H86+W86+careerR+careerW+PO86, data=baseData)
model2


#F-test 실행
anova(model2,model)

#F-test 결과 유의하지 않은 변수를 제거하지 않은 처음 model의 p값이 0.02961 으로 0.05보다 작으므로 유의한 관계가 있다고 할 수 있다. 따라서 해당 변수를 제거하지 않고 분석한다.

#예측에 사용할 모델 살펴보기 
summary(model)

coef(model)

#1~10번째 데이터에 대해 예측된 salary 값 
fitted(model)[1:10]

#1~10번째 데이터에 대한 잔차
residuals(model)[1:10]


#fitted 값과 잔차의 값의 합은 실제 데이터 값과 같다. 
fitted(model)[1:5]+residuals(model)[1:5]
baseData$salary87[1:5] 


#salary87을 예측하기 
p1 <- predict(model)
head(p1)

#예측한 값과 test 데이터의 실제 종속변수 값의 차이를 구하기(오차생성)

error1 <- p1 - testData[["salary87"]]
error1

sqrt(mean(error1^2))
# RMSE 는 596.9284 이다.


###3. MSE를 사용한 검정(다중선형회귀 결과 검정)
# Root Mean Squared Error의 약어로 평균 제곱근 편차를 뜻한다. 총 n개 데이터 중 i번째 데이터에 대한 추정값이^yi , 실제 값이 yi 일 때 RMSE 루트(yi-^yi)^2/n 는 이다.



###2. 랜덤포레스트 사용
#앙상블 기법을 사용한 모델
#여러 모델을 학습 후 예측시 여러모델의 예측 결과를 종합해 사용
#첫번째 방법. 데이터의 일부를 복원 추출로 꺼내고 해당 데이터에 대해 의사결정 나무를 만듦
#-> 일부 데이터만 사용하여 만드는 것
#두번째 방법/ 노트 내 데이터를 자식 노드로 나누는 기준을 일부 변수만 대상으로 하여 나누는 것

head(baseData)

library(randomForest)

#랜덤포레스트 모델 

set.seed(1000) #랜덤포레스트를 여러번 시행해도 동일한 결과가 나오도록 함. 

m<-randomForest(salary87 ~ AB86+H86+HR86+R86+RBI86+W86+years+careerAB+careerH+careerHR+careerR+careerRBI+careerW+PO86+A86+E86, data=baseData, mtry=5, ntree=500)
m

#예측 
p2 <- predict(m, newdata=baseData, type="response")


###3. MSE를 사용한 검정(랜덤포레스트 결과 검정)

head(p2)

#예측한 값과 test 데이터의 실제 종속변수 값의 차이를 구하기(오차생성)

error2 <- p2 - testData[["salary87"]]
error2

sqrt(mean(error2^2))
# RMSE 는 608.176 이다.


###4. 결론
# 다중선형회귀 모델의 RMSE는 596.9284 이고, 랜덤포레스트 모델의 RMSE 는 608.176 이다.
# RMSE는 작을수록 정밀도가 높으므로, 랜덤포레스트 모델보다 RMSE이 더 작은 값인 다중선형회귀 모델의 예측 설명력이 높다고 할 수 있다. 