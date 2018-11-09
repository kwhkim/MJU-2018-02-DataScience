library(ggplot2)
library(dplyr)
ggplot(data = mpg, mapping=aes(x=displ, y= cty,col=hwy))+geom_point()
ggplot(data = mpg, mapping=aes(x=displ, y= cty,size=hwy))+geom_point()
ggplot(data = mpg, mapping=aes(x=displ, y= cty,shape=fl))+geom_point()
ggplot(data = mpg, mapping=aes(x=displ, y= cty,shape=trans))+geom_point()

ggplot(data = mpg,aes(x=displ, y= cty))+geom_line()+geom_point()

ggplot(data = mpg,aes(x=displ, y= cty))+geom_jitter()+geom_point(alpha=0.2)

ggplot(data = mpg,aes(x=manufacturer, y= cty))+geom_boxplot()
ggplot(data = mpg,aes(x=manufacturer, y= cty))+geom_violin()
ggplot(data = mpg,aes(x=manufacturer, y= cty))+geom_
#가로세로바꾸기
ggplot(data = mpg,aes(x=manufacturer, y= cty))+geom_violin()+coord_flip()

ggplot(data = mpg, aes(x=cty))+geom_freqpoly(bins=10)
ggplot(data = mpg, aes(x=cty,col=fl))+geom_freqpoly(bins=10)

ggplot(data = mpg,mapping = aes(x=manufacturer))+geom_bar()+coord_flip()

ggplot(data = mpg,mapping = aes(x=manufacturer))+geom_bar(col='red',fill='blue')+coord_flip()


ggplot(data = mpg,mapping = aes(x=manufacturer))+geom_bar(aes(col=fl,fill=fl))

ggplot()+
  geom_jitter(data= mpg, mapping=aes(x=displ,y=cty))+
  geom_jitter(data= mpg, mapping=aes(x=displ,y=hwy,col='red'))

ggplot()+
  geom_jitter(data= mpg, mapping=aes(x=displ,y=cty))+
  facet_grid(fl~.)
#(x~y)x방향 y방향 나누고 싶은 곳에 따라 각 위치에 넣는다.없으면 .을 찍는다.
ggplot()+
  geom_jitter(data= mpg, mapping=aes(x=displ,y=cty))+
  facet_grid(.~fl)

ggplot()+
  geom_jitter(data= mpg, mapping=aes(x=displ,y=cty))+
  facet_wrap(~fl)

ggplot()+
  geom_jitter(data= mpg, mapping=aes(x=displ,y=cty,col=fl))+
  facet_grid(trans2~fl)#???
  
  


  
ggplot(data= mpg, mapping=aes(x=displ,y=cty,col=fl))+
  geom_jitter()+
  scale_y_log10()+
  coord_cartesian(ylim=c(1,40))

#범위의 차이가 클때 로그를 취해준다.

ggplot(data= mpg, mapping=aes(x=displ,y=cty,col=fl))+
  geom_jitter()+scale_color_brewer(palette = "BuPu")


ggplot(data= mpg, mapping=aes(x=displ,y=cty,col=fl))+
  geom_jitter()+scale_color_brewer(palette = "blues")
  


ggplot(data= mpg, mapping=aes(x=displ,y=cty,col=hwy))+
  geom_jitter()+scale_color_gradient(low='red',high ='blue')

ggplot(data= mpg, mapping=aes(x=displ,y=cty,col=hwy))+
  geom_jitter()+scale_fill_gradient(low='red',high ='blue')



geom_hline(y)