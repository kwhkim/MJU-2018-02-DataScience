## 프리미어 리그 경기 결과를 통한 축구경기 득점에 영향을 미치는 요소 분석

### 데이터 출처 및 선정 이유


### 데이터 불러오기 
epl_2000_1 <- read.csv("00_01epl.csv")
epl_2001 <- read.csv("01_02epl.csv")
epl_2002_1 <- read.csv("02_03epl.csv")
epl_2003 <- read.csv("03_04epl.csv")
epl_2004 <- read.csv("04_05epl.csv")
epl_2005 <- read.csv("05_06epl.csv")
epl_2006 <- read.csv("06_07epl.csv")
epl_2007 <- read.csv("07_08epl.csv")
epl_2008 <- read.csv("08_09epl.csv")
epl_2009 <- read.csv("09_10epl.csv")
epl_2010 <- read.csv("10_11epl.csv")
epl_2011 <- read.csv("11_12epl.csv")
epl_2012 <- read.csv("12_13epl.csv")
epl_2013 <- read.csv("13_14epl.csv")
epl_2014 <- read.csv("14_15epl.csv")
epl_2015 <- read.csv("15_16epl.csv")
epl_2016 <- read.csv("16_17epl.csv")
epl_2017 <- read.csv("17_18epl.csv")


### 필요한 데이터만 추출하기
library(dplyr)
epl_2000 <- epl_2000 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF,  HY, AY, HR, AR)

epl_2001 <- epl_2001 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2002 <- epl_2002 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2003 <- epl_2003 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2004 <- epl_2004 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2005 <- epl_2005 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2006 <- epl_2006 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2007 <- epl_2007 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2008 <- epl_2008 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2009 <- epl_2009 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2010 <- epl_2010 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2011 <- epl_2011 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2012 <- epl_2012 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2013 <- epl_2013 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2014 <- epl_2014 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2015 <- epl_2015 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2016 <- epl_2016 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

epl_2017 <- epl_2017 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

### 결측치 확인하기
table(is.na(epl_2000))
table(is.na(epl_2001))
table(is.na(epl_2002))
table(is.na(epl_2003))
table(is.na(epl_2004))
table(is.na(epl_2005))
table(is.na(epl_2006))
table(is.na(epl_2007))
table(is.na(epl_2008))
table(is.na(epl_2009))
table(is.na(epl_2010))
table(is.na(epl_2011))
table(is.na(epl_2012))
table(is.na(epl_2013))
table(is.na(epl_2014))
table(is.na(epl_2015))
table(is.na(epl_2016))
table(is.na(epl_2017))

### 결측치 제거하기 
epl_2002 <- epl_2002 %>%
  filter(!is.na(FTHG))

epl_2003 <- epl_2003 %>% 
  filter(!is.na(FTHG))

epl_2004 <- epl_2004 %>% 
  filter(!is.na(FTHG))

epl_2014 <- epl_2014 %>% 
  filter(!is.na(FTHG))

### 데이터 분석하기

#### 1. 홈 경기 vs 원정 경기?

##### 1-1. 홈경기 결과

top10_score_team_H <- epl_2000 %>% 
  filter(HomeTeam %in% c("Man United", "Arsenal", "Chelsea", "Liverpool", "Leeds", "Charlton", "Ipswich", "Tottenham", "Everton", "Leicester"))

top10_score_team_H <- top10_score_team_H %>% 
  group_by(HomeTeam, FTR) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

Home_Result <- ggplot(data = top10_score_team_H, aes(x = HomeTeam, y = count, fill = FTR)) + geom_col(position = "dodge") + scale_x_discrete(limits = c("Man United", "Arsenal", "Chelsea", "Liverpool", "Leeds", "Charlton", "Ipswich", "Tottenham", "Everton", "Leicester"))

##### 1-2. 원정경기 결과

top10_score_team_A <- epl_2000 %>%
  filter(AwayTeam %in% c("Liverpool", "Man United", "Leeds", "Ipswich", "Middlesbrough", "Chelsea", "Coventry", "Sunderland", "Man City", "West Ham"))

top10_score_team_A <- top10_score_team_A %>%
  group_by(AwayTeam, FTR) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

Away_Result <- ggplot(data = top10_score_team_A, aes(x = AwayTeam, y = count, fill = FTR)) + geom_col(position = "dodge") + scale_x_discrete(limits = c("Liverpool", "Man United", "Leeds", "Ipswich", "Middlesbrough", "Chelsea", "Coventry", "Sunderland", "Man City", "West Ham")) 

grid.arrange(Home_Result, Away_Result)

##### 결론: 홈경기가 원정경기보다 훨씬 경기에 유리한 결과를 가져온다는 사실을 알 수 있다.


#### 2. 전반 우세 vs 후반 우세? 

##### 2-1. Home 경기 전 후반 득점 

epl_2000_h <- epl_2000 %>%  
  select(HomeTeam, FTHG, HTHG) %>% 
  mutate(ATHG = FTHG - HTHG) %>% 
  group_by(HomeTeam) %>% 
  summarise(tot_HTHG = sum(HTHG),
            tot_ATHG = sum(ATHG),
            tot_FTHG = sum(FTHG)) %>% 
  arrange(desc(tot_FTHG)) %>% 
  head(10)

epl_2000_h

library(ggplot2)

epl_2000_g1 <- ggplot(data = epl_2000_h, aes(x = reorder(HomeTeam, -tot_FTHG), y = tot_HTHG)) + geom_col()

epl_2000_g2 <- ggplot(data = epl_2000_h, aes(x = reorder(HomeTeam, -tot_FTHG), y = tot_ATHG)) + geom_col()

library(gridExtra)

grid.arrange(epl_2000_g1, epl_2000_g2)

##### 2-2. Away 경기 전 후반 득점 

epl_2000_a <- epl_2000 %>%  
  select(AwayTeam, FTAG, HTAG) %>% 
  mutate(ATAG = FTAG - HTAG) %>% 
  group_by(AwayTeam) %>% 
  summarise(tot_HTAG = sum(HTAG),
            tot_ATAG = sum(ATAG),
            tot_FTAG = sum(FTAG)) %>% 
  arrange(desc(tot_FTAG)) %>% 
  head(10)

epl_2000_a

epl_2000_g3 <- ggplot(data = epl_2000_a, aes(x = reorder(AwayTeam, -tot_FTAG), y = tot_HTAG)) + geom_col()

epl_2000_g3

epl_2000_g4 <- ggplot(data = epl_2000_a, aes(x = reorder(AwayTeam, -tot_FTAG), y = tot_ATAG)) + geom_col()

grid.arrange(epl_2000_g3, epl_2000_g4)

##### 2-3. Home 경기, Away 경기 전후반 득점수 비교

grid.arrange(epl_2000_g1, epl_2000_g2, epl_2000_g3, epl_2000_g4)


#### 3. 유효슈팅이 득점으로 이어진다?

##### 3-1. Home 경기 유효슈팅 

epl_2000_h2 <- epl_2000 %>%  
  select(HomeTeam, HST) %>% 
  group_by(HomeTeam) %>% 
  summarise(tot_HST = sum(HST)) %>% 
  arrange(desc(tot_HST)) %>% 
  head(10)

epl_2000_h2

##### 3-2. Home 경기 유효슈팅 대비 득점률 

epl_2000_h_score_per_shoot <- epl_2000 %>% 
  select(HomeTeam, FTHG, HST) %>% 
  group_by(HomeTeam) %>% 
  summarise(tot_FTHG = sum(FTHG),
            tot_HST = sum(HST)) %>% 
  mutate(SPS = ((tot_FTHG/tot_HST)*100)) %>%
  arrange(desc(tot_FTHG)) %>% 
  head(10)

epl_2000_g5 <- ggplot(data = epl_2000_h_score_per_shoot, aes(x = reorder(HomeTeam, -tot_FTHG), y = SPS)) + geom_col()

##### 3-3. Away 경기 유효슈팅

epl_2000_h2 <- epl_2000 %>%  
  select(AwayTeam, AST) %>% 
  group_by(AwayTeam) %>% 
  summarise(tot_AST = sum(AST)) %>% 
  arrange(desc(tot_AST)) %>% 
  head(10)

epl_2000_h2

##### 3-4. Away 경기 유효슈팅 대비 득점률

epl_2000_a_score_per_shoot <- epl_2000 %>% 
  select(AwayTeam, FTAG, AST) %>% 
  group_by(AwayTeam) %>% 
  summarise(tot_FTAG = sum(FTAG),
            tot_AST = sum(AST)) %>% 
  mutate(SPS = ((tot_FTAG/tot_AST)*100)) %>%
  arrange(desc(tot_FTAG)) %>% 
  head(10)

epl_2000_g6 <- ggplot(data = epl_2000_a_score_per_shoot, aes(x = reorder(AwayTeam, -tot_FTAG), y = SPS)) + geom_col()

##### 3-5. Home 경기, Away 경기 유효슈팅 대비 득점률

grid.arrange(epl_2000_g5, epl_2000_g6)
