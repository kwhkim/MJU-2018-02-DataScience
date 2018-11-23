library(dplyr)
library(ggplot2)

#### 00-01
epl_2000_tot <- epl_2000 %>% 
  mutate(Result = ifelse(FTR == "H", "W",
                        ifelse(FTR == "A", "L", "D")),
         Point = ifelse(Result == "W", 3, 
                        ifelse(Result == "D", 1, 0))) %>% 
  select(HomeTeam, Point)

epl_2000_tot <- rename(epl_2000_tot, Team = HomeTeam)

#### 01-02
epl_2001_tot <- epl_2001 %>% 
  mutate(Result = ifelse(FTR == "H", "W",
                         ifelse(FTR == "A", "L", "D")),
         Point = ifelse(Result == "W", 3, 
                        ifelse(Result == "D", 1, 0))) %>% 
  select(HomeTeam, Point) %>% 
  group_by(HomeTeam) %>% 
  summarise(Sum = sum(Point))

epl_2001_tot <- rename(epl_2001_tot, Team = HomeTeam)

epl_2001_tot_a <- epl_2001 %>%
  mutate(Result = ifelse(FTR == "A", 3,
                         ifelse(FTR == "H", 0, 1))) %>% 
  select(AwayTeam, Result) %>% 
  group_by(AwayTeam) %>% 
  summarise(sum = sum(Result))

epl_2001_tot_a
epl_2001_tot_a <- rename(epl_2001_tot_a, Team = AwayTeam)
epl_2001_tot_a <- rename(epl_2001_tot_a, Sum = sum)

total_pts <- left_join(epl_2001_tot, epl_2001_tot_a, by = "Team")

total_pts <- total_pts %>% 
  mutate(total_pts = Sum.x + Sum.y) %>% 
  arrange(desc(total_pts))

#### 02-03
epl_2002_tot <- epl_2002 %>% 
  mutate(Result = ifelse(FTR == "H", "W",
                         ifelse(FTR == "A", "L", "D")),
         Point = ifelse(Result == "W", 3, 
                        ifelse(Result == "D", 1, 0))) %>% 
  select(HomeTeam, Point) %>% 
  group_by(HomeTeam) %>% 
  summarise(Sum = sum(Point))

epl_2002_tot <- rename(epl_2002_tot, Team = HomeTeam)

total <- left_join(epl_2001_tot, epl_2002_tot, by = "Team")

### 분석
#### 1. 홈팀, 원정팀 
##### 1-1. 홈팀
epl_2000_h <- epl_2000 %>%
  filter(HomeTeam %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
  mutate(Result_h = ifelse(FTR == "H", 3,
                           ifelse(FTR == "A", 0, 1))) %>%
  select(HomeTeam, Result_h) %>% 
  group_by(HomeTeam) %>% 
  summarise(Pts_h = sum(Result_h))

epl_2000_h <- rename(epl_2000_h, Team = HomeTeam)

##### 1-2. 원정팀
epl_2000_a <- epl_2000 %>%
  filter(AwayTeam %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
  mutate(Result_a = ifelse(FTR == "A", 3,
                           ifelse(FTR == "H", 0, 1))) %>%
  select(AwayTeam, Result_a) %>% 
  group_by(AwayTeam) %>% 
  summarise(Pts_a = sum(Result_a))

epl_2000_a <- rename(epl_2000_a, Team = AwayTeam)

##### 1-3. 홈팀, 원정팀 테이블 합치기
epl_2000_tot <- left_join(epl_2000_h, epl_2000_a, by = "Team")

epl_2000_tot <- epl_2000_tot %>% 
  mutate(tot_pts = Pts_h + Pts_a) %>% 
  arrange(desc(tot_pts))

##### 2-1. 홈 경기 결과 경우의 수 - win win
epl_2000_h_s <- epl_2000_h %>% 
  filter(Team %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
  mutate(Result = ifelse(FTR == "H", 3,
                         ifelse(FTR == "A", 0, 1))) %>%
  filter(HTR == "H" & FTR == "H") %>% 
  group_by(Team) %>% 
  summarise(Sum_h = sum(Result)) %>% 
  arrange(desc(Sum_h)) %>% 
  select(Team, Sum_h) 

epl_2000_a_s <- epl_2000_a %>% 
    filter(Team %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
    mutate(Result = ifelse(FTR == "A", 3,
                         ifelse(FTR == "H", 0, 1))) %>%
    filter(HTR == "A" & FTR == "A") %>% 
    group_by(Team) %>% 
    summarise(Sum_a = sum(Result)) %>% 
    arrange(desc(Sum_a)) %>% 
    select(Team, Sum_a)
  
epl_2000_ww <- left_join(epl_2000_h_s,epl_2000_a_s, by = "Team")
epl_2000_ww
##### 2-2. 홈 경기 결과 경우의 수 - lose draw
epl_2000_h_s2 <- epl_2000_h %>% 
  filter(Team %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
  mutate(Result = ifelse(FTR == "H", 3,
                         ifelse(FTR == "A", 0, 1))) %>%
  filter(HTR == "A" & FTR == "D") %>% 
  group_by(Team) %>% 
  summarise(Sum_h = sum(Result)) %>% 
  arrange(desc(Sum_h)) %>% 
  select(Team, Sum_h) 

epl_2000_a_s2 <- epl_2000_a %>% 
  filter(Team %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
  mutate(Result = ifelse(FTR == "A", 3,
                         ifelse(FTR == "H", 0, 1))) %>%
  filter(HTR == "H" & FTR == "D") %>% 
  group_by(Team) %>% 
  summarise(Sum_a = sum(Result)) %>% 
  arrange(desc(Sum_a)) %>% 
  select(Team, Sum_a)

epl_2000_ld <- left_join(epl_2000_h_s2,epl_2000_a_s2, by = "Team")
epl_2000_ld

##### 2-3. 홈 경기 결과 경우의 수 - lose win
epl_2000_h_s3 <- epl_2000_h %>% 
  filter(Team %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
  mutate(Result = ifelse(FTR == "H", 3,
                         ifelse(FTR == "A", 0, 1))) %>%
  filter(HTR == "A" & FTR == "H") %>% 
  group_by(Team) %>% 
  summarise(Sum_h = sum(Result)) %>% 
  arrange(desc(Sum_h)) %>% 
  select(Team, Sum_h) 

epl_2000_a_s3 <- epl_2000_a %>% 
  filter(Team %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
  mutate(Result = ifelse(FTR == "A", 3,
                         ifelse(FTR == "H", 0, 1))) %>%
  filter(HTR == "H" & FTR == "A") %>% 
  group_by(Team) %>% 
  summarise(Sum_a = sum(Result)) %>% 
  arrange(desc(Sum_a)) %>% 
  select(Team, Sum_a)

epl_2000_lw <- left_join(epl_2000_h_s3,epl_2000_a_s3, by = "Team")
epl_2000_lw

##### 2-4. 홈 경기 결과 경우의 수 - draw win
epl_2000_h_s4 <- epl_2000_h %>% 
  filter(Team %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
  mutate(Result = ifelse(FTR == "H", 3,
                         ifelse(FTR == "A", 0, 1))) %>%
  filter(HTR == "A" & FTR == "H") %>% 
  group_by(Team) %>% 
  summarise(Sum_h = sum(Result)) %>% 
  arrange(desc(Sum_h)) %>% 
  select(Team, Sum_h) 

epl_2000_a_s4 <- epl_2000_a %>% 
  filter(Team %in% c("Man United", "Arsenal", "Chelsea", "Liverpool")) %>% 
  mutate(Result = ifelse(FTR == "A", 3,
                         ifelse(FTR == "H", 0, 1))) %>%
  filter(HTR == "H" & FTR == "A") %>% 
  group_by(Team) %>% 
  summarise(Sum_a = sum(Result)) %>% 
  arrange(desc(Sum_a)) %>% 
  select(Team, Sum_a)

epl_2000_dw <- left_join(epl_2000_h_s4,epl_2000_a_s4, by = "Team")
epl_2000_dw

