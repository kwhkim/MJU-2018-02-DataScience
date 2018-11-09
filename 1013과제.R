library(readxl)
library(dplyr)
library(ggplot2)
crime_raw<- read.csv("crime.csv")
cctv_raw <- read.csv("cctv.csv")
cr <- crime_raw
cc <- cctv_raw


summary(cr)
str(cr)
head(cr,10)

summary(cc)
str(cc)
head(cc,10)
tail(cc)


#CCTV는 이미 발생한 범죄자를 추적하는것에는 매우 효과적이라는 것은 모두가 인정하는 부분이지만 범죄를 예방하는데에도 큰 효과가 있는지 알아보려한다. 각 구별 범죄발생 횟수와 CCTV설치 댓수 자료를 사용


cc <- rename(cc,자치구 = 기관명)
cc

df <- left_join(cc,cr, by = "자치구")
df <- df%>%
  select(자치구,합계,소계,방범용)
df <- rename(df,범죄발생 = 합계, 전체cctv = 소계,방범용cctv = 방범용)
df

df%>%
  arrange(범죄발생)%>%
  head(10)


df%>%
  arrange(desc(방범용cctv))%>%
  head(10)
#교집합을 보여주는 intersect함수 사요

intersect(df%>%
            arrange(범죄발생)%>%
            head(10),df%>%
            arrange(desc(방범용cctv))%>%
            head(10))


#방범cctv 많은곳 범죄 발생 적은곳 중 동대문구, 성북구, 은평구, 양천구 4곳 동일 

#방범용 cctv말고도 어린이 보호, 쓰레기 무단투기 감시용등 여러 CCTV가 설치되어 있는데 이러한 CCTV들도 범죄예방에 도움이 되는지 알아보자

df%>%
  arrange(범죄발생)%>%
  head(10)

df%>%
  arrange(desc(전체cctv))%>%
  head(10)

intersect(df%>%
            arrange(범죄발생)%>%
            head(10),df%>%
            arrange(desc(전체cctv))%>%
            head(10))

#cctv많은곳 범죄발생 적은곳 중 동대문구,성북구,은평구,양천구 4곳 동일
#분석결과 방법용 CCTV와 전체 CCTV의 비교 결과가 같다.


#CCTV와 범죄발생이 영향이 있는지 근거를 더하기 위해 다른 경우들도 알아보

df%>%
  arrange(desc(범죄발생))%>%
  head(10)

df%>%
  arrange(전체cctv)%>%
  head(10)

intersect(df%>%
            arrange(desc(범죄발생))%>%
            head(10),df%>%
            arrange(전체cctv)%>%
            head(10))

#cctv적은곳 범죄발생 많은곳 중 중랑구, 광진구, 강서구,마포구,송파구 5곳 동일


#위의 두가지와 반대의 경우
df%>%
  arrange(범죄발생)%>%
  head(10)

df%>%
  arrange(전체cctv)%>%
  head(10)

intersect(df%>%
            arrange(범죄발생)%>%
            head(10),df%>%
            arrange(전체cctv)%>%
            head(10))

#cctv 적은곳 범죄발생 적은곳 중 서대문구, 도봉구,강북구 3곳 동일

df%>%
  arrange(desc(범죄발생))%>%
  head(10)

df%>%
  arrange(desc(전체cctv))%>%
  head(10)

intersect(df%>%
            arrange(desc(범죄발생))%>%
            head(10),df%>%
            arrange(desc(전체cctv))%>%
            head(10))

#CCTV많은곳 범죄발생 많은곳  강남구,관악구,구로구,서초구 4곳
# 강남역은 유동인구가 많고 유흥가가 많은 특성상 범죄발생 빈도와 CCTV댓수 두가지 모두 많은 특수한 지역이라 제외하여 3곳

#


#CCTV가 많은 지역일수록 범죄발생 횟수가 적고 CCTV가 적은 지역일 수록 범죄 발생횟수가 높은 경향을 보였다.



