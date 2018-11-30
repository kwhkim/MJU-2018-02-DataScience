library(dplyr)
library(readxl)
library(ggplot2)
df_exam <- read_excel("00_Instructor/W07_ggplot2/excel_exam.xlsx")
df_exam


ggplot(df_exam, aes(x=as.factor(class), y=math)) +
  geom_boxplot(col='red') +
  geom_boxplot(aes(y=english), col='blue') +
  geom_boxplot(aes(y=science), col='yellow') 

library(tidyr)

df_exam_long <- df_exam %>% gather("key", "value", math:science)

ggplot(df_exam_long, aes(x=as.factor(class), y=value, col=key)) + 
  geom_boxplot()
  
ggplot(df_exam_long, aes(x=as.factor(class), y=value, col=key)) + 
  geom_boxplot(position='identity') 

ggplot(df_exam_long, aes(x=as.factor(class), y=value, col=key)) + 
  geom_boxplot(position='dodge') 

# position = 'identity', 'jitter', 'dodge', 'stack', 'fill'

ggplot(df_exam, aes(x=as.factor(as.character(class)), y=math)) +
  geom_boxplot(col='red') +
  geom_boxplot(aes(y=english), col='blue') +
  geom_boxplot(aes(y=science), col='yellow')

df_exam_long <- df_exam %>% gather("key", "value", math:science)

ggplot(df_exam_long, aes(x=as.factor(class), y=value, col=key)) + 
  geom_boxplot()


