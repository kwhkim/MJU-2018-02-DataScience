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


cc <- rename(cc,자치구=기관명)
cc



df <- left_join(cc,cr, by = "자치구")
df <- rename(df,범죄발생 = 합계, 전체cctv = 소계,방범용cctv = 방범용,범죄검거 = 합계.1)



#factor형태 , 제거 및 numeric으로 전환
df$범죄발생 <- sub(",","",df$범죄발생)
df$범죄발생<- as.numeric(df$범죄발생)


df$범죄검거 <- sub(",","",df$범죄검거)
df$범죄검거<- as.numeric(df$범죄검거)


df$살인 <- sub(",","",df$살인)
df$살인<- as.numeric(df$살인)

df$살인.1 <- sub(",","",df$살인.1)
df$살인.1<- as.numeric(df$살인.1)

df$강도 <- sub(",","",df$강도)
df$강도<- as.numeric(df$강도)

df$강도.1 <- sub(",","",df$강도.1)
df$강도.1<- as.numeric(df$강도.1)

df$강간강제추행 <- sub(",","",df$강간강제추행)
df$강간강제추행<- as.numeric(df$강간강제추행)


df$강간강제추행.1 <- sub(",","",df$강간강제추행.1)
df$강간강제추행.1<- as.numeric(df$강간강제추행.1)

df$절도 <- sub(",","",df$절도)
df$절도<- as.numeric(df$절도)


df$절도.1 <- sub(",","",df$절도.1)
df$절도.1<- as.numeric(df$절도.1)

df$폭력 <- sub(",","",df$폭력)
df$폭력<- as.numeric(df$폭력)


df$폭력.1 <- sub(",","",df$폭력.1)
df$폭력.1<- as.numeric(df$폭력.1)



summary(df)
#결측치 확ㅇ
table(is.na(df))
df <- df%>%
  filter(!is.na(살인.1))


#이상치 확인
table(df$자치구)
table(df$전체cctv)
table(df$방범용cctv)
table(df$범죄발생)
table(df$범죄검거)
table(df$강도)
table(df$강도.1)
table(df$살인)
table(df$살인.1)
table(df$강간강제추행)
table(df$강간강제추행.1)
table(df$절도)
table(df$절도.1)
table(df$폭력)
table(df$폭력.1)

#2016년등 전년도에 발생한 범죄를 2017년도에 검거하여 검거횟수가 발생횟수를 넘을 수는 있지만 종로구의 강간은 검거횟수가 발생횟수의 5배가까이 되어 정상적인 수치라 보기 어렵다.
#송파구 강도검거 횟수가 발생횟수의 3배에 달하나 발생 1회 검거 3회로 충분히 가능하다 판단되어 수정하지 않았다.


boxplot(df$강간강제추행.1)
df$강간강제추행.1 <- ifelse(df$강간강제추행.1>500,mean(df$강간강제추행.1),df$강간강제추행.1)


#방범용 cctv 많은곳 범죄 많은 곳
dfa1 <- df%>%
  arrange(범죄발생)%>%
  head(10)
dfa1
ggplot(data = dfa1,aes(x=reorder(자치구,-범죄발생),y=범죄발생))+geom_col(fill="#FFCC00")+labs(x="자치구")
dfa2 <- 
df%>%
  arrange(desc(방범용cctv))%>%
  head(10)

ggplot(data = dfa2,aes(x=reorder(자치구,-방범용cctv),y=방범용cctv))+geom_col(fill="#FF3333")+labs(x="자치구")

#교집합을 보여주는 intersect함수 사요

intersect(df%>%
            arrange(범죄발생)%>%
            head(10),df%>%
            arrange(desc(방범용cctv))%>%
            head(10))

dfaa <- ggplot()+
  geom_col(data = dfa1,aes(x=reorder(자치구,-범죄발생),y=범죄발생,fill=자치구))

#빨간 표시 된곳이 교집합을 통해 나온 겹치는 곳
dfaa+scale_fill_manual(values = c("#FFCC00","#FFCC00","#FFCC00","#FF3333","#FFCC00","#FFCC00","#FFCC00","#FF3333","#FF3333","#FF3333"))+labs(x="자치구")



#방범cctv 많은곳 범죄 발생 적은곳 상위 10곳중 동대문구, 성북구, 은평구, 양천구 4곳 동일 

#방범용 cctv말고도 어린이 보호, 쓰레기 무단투기 감시용등 여러 CCTV가 설치되어 있는데 이러한 CCTV들도 범죄예방에 도움이 되는지 알아보자

dfb <- df%>%
  arrange(범죄발생)%>%
  head(10)
dfb
df%>%
  arrange(desc(전체cctv))%>%
  head(10)

df111 <- intersect(df%>%
            arrange(범죄발생)%>%
            head(10),df%>%
            arrange(desc(전체cctv))%>%
            head(10))
df111

dfb1 <- ggplot()+
  geom_col(data = dfb,aes(x=reorder(자치구,-범죄발생),y=범죄발생,fill=자치구))
dfb1+scale_fill_manual(values = c("#FFCC00","#FFCC00","#FFCC00","#FFCC00","#FF3333","#FFCC00","#FFCC00","#FF3333","#FF3333","#FF3333"))+labs(x="자치구")


#cctv많은곳 범죄발생 적은곳 상위 10곳중 동대문구,성북구,은평구,양천구 4곳 동일
#분석결과 방법용 CCTV와 전체 CCTV의 비교 결과가 같다.




dfc <- df%>%
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

dfc1 <- ggplot()+
  geom_col(data = dfc,aes(x=reorder(자치구,-범죄발생),y=범죄발생,fill=자치구))
dfc1+scale_fill_manual(values = c("#FFCC00","#FF3333","#FFCC00","#FF3333","#FFCC00","#FF3333","#FFCC00","#FF3333","#FFCC00","#FF3333"))+labs(x="자치구")


#cctv적은곳 범죄발생 많은곳 중 중랑구, 광진구, 강서구,마포구,송파구 5곳 동일


#cctv가 많고 범죄 발생이 적은곳, cctv가 적고 범죄 발생이 많은 곳은 상위 10개 지역중 4~5지역이 동일 했다. 이 수치가 높은 수치인지 알아보기 위해 반대되는 cctv가 적고 범죄 발생이 적은곳, cctv가 많고 범죄 발생이 많은 곳을 알아보도록 하겠다.
dfd <- df%>%
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

dfd1 <- ggplot()+
  geom_col(data = dfd,aes(x=reorder(자치구,-범죄발생),y=범죄발생,fill=자치구))
dfd1+scale_fill_manual(values = c("#FF3333","#FFCC00","#FF3333","#FFCC00","#FFCC00","#FF3333","#FFCC00","#FFCC00","#FFCC00","#FFCC00"))+labs(x="자치구")


#cctv 적은곳 범죄발생 적은곳 중 서대문구, 도봉구,강북구 3곳 동일

dfe <- df%>%
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


dfe1 <- ggplot()+
  geom_col(data = dfe,aes(x=reorder(자치구,-범죄발생),y=범죄발생,fill=자치구))
dfe1+scale_fill_manual(values = c("#FF3333","#FFCC00","#FF3333","#FFCC00","#FF3333","#FFCC00","#FF3333","#FFCC00","#FFCC00","#FFCC00"))+labs(x="자치구")


#CCTV많은곳 범죄발생 많은곳  강남구,관악구,구로구,서초구 4곳
# 강남역은 유동인구가 많고 유흥가가 많은 특성상 범죄발생 빈도와 CCTV댓수 두가지 모두 많은 특수한 지역이라 제외하여 3곳



#cctv가 적고 범죄 발생 적은곳과 cctv가 많고 범죄 발생이 적은 곳은 상위 10개지역중 3곳이 동일하게 나왔다. 
#결과적으로 범죄발생이 cctv에 영향을 받는다고 할 수 있다..



#CCTV가 많고 범죄율 낮은 지역을 같이 나타낸 그래프 막대가 범죄발생 점이 CCTV
ggplot()+
geom_col(data = dfc,aes(x=reorder(자치구,-범죄발생),y=범죄발생,fill=자치구))+scale_fill_brewer(palette = "Set3")+
  geom_point(data=dfc,aes(x=자치구,y=전체cctv,fill="black",size=6))+labs(x="자치구")



dfb

#검거율
library(RColorBrewer)
library(tidyr)
library(reshape)







df<- df%>%
  mutate(검거율=df$범죄검거/df$범죄발생*100)

df11<- df%>%
  select(자치구,전체cctv,검거율)%>%
  arrange(desc(검거율))
df11



df22 <- intersect(df%>%
            arrange(desc(검거율))%>%
            head(10),df%>%
            arrange(desc(전체cctv))%>%
            head(10))

df22

arrange(df$전체cctv)

#상위 10곳중 관악,은평,동대문,종로 4곳이 동일 

g0 <- ggplot()+
  geom_col(data=df22,aes(x=reorder(자치구,-전체cctv),y=전체cctv*0.035),fill="lightblue",colour="black")
g0+
  geom_point(data=df22,aes(x=자치구,y=검거율),fill="red",size = 4, shape = 21)+labs(x="자치구",y="전체cctv & 검거율")


g1 <- ggplot()+
  geom_col(data=df22,aes(x=reorder(자치구,-전체cctv),y=전체cctv*0.035),fill="lightblue",colour="black")
g1+
  geom_point(data=df,aes(x=자치구,y=검거율),fill="red",size = 4, shape = 21)+labs(x="자치구",y="전체cctv & 검거율")

g2 <- ggplot()+
  geom_boxplot(data=df22,aes(x=reorder(자치구,-전체cctv),y=전체cctv*0.035),fill="lightblue",colour="black")
g2+
  geom_point(data=df11,aes(x=자치구,y=검거율),fill="red",size = 4, shape = 21)+labs(x="자치구",y="전체cctv & 검거율")
#이부분이 그 플롯 비교부분인데 이렇게 하는게 맞는지 잘 모르겠습니다





#cctv가 많은 지역일수록 높은 검거율을 보였다.


df<- df%>%
  mutate(살인검거율=df$살인.1/df$살인*100,
              강도검거율=df$강도.1/df$강도*100,
              강간검거율=df$강간강제추행.1/df$강간강제추행*100,
              절도검거율=df$절도.1/df$절도*100,
              폭력검거율=df$폭력.1/df$폭력*100)
dff <- df%>%
  select(자치구,전체cctv,검거율,살인검거율,강도검거율,강간검거율,절도검거율,폭력검거율)

dff1 <- melt(dff,measure.=c(3:8))
ggplot(dff1,aes(x=자치구,y=value,fill=자치구))+geom_col()+facet_wrap(variable~.)






#결론: cctv는 범죄예방과 검거에 큰 효력을 지닌다.


