#Line Graph
library(ggplot2)
library(gcookbook)
library(plyr)

#basic line graph
BOD1 <- BOD
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line() # if no group=1, there is a fault

#add point to library
ggplot(worldpop,aes(x=Year,y=Population))+
  geom_line()+
  geom_point()+
  scale_y_log10() #设置坐标轴标尺
ggplot(worldpop,aes(x=Year,y=Population))+
  geom_line()+
  geom_point()

#multiple lines graph
#If any discrete variables are mapped to aes‐ thetics like colour or linetype or shape or fill, 
#they are automatically used as grouping variables
tg <- ddply(ToothGrowth,c('supp','dose'),summarise,length=mean(len))
ggplot(tg,aes(x=dose,y=length,color=supp))+
  geom_line()
ggplot(tg,aes(x=dose,y=length,linetype=supp))+
  geom_line()
ggplot(tg,aes(x=factor(dose),y=length,color=supp,group=supp))+
  geom_line() #把dose变成factor之后，一定要有group，要不然R不知道该如何分组
ggplot(tg,aes(x=dose,y=length,shape=supp))+
  geom_line()+
  geom_point(size=4)
ggplot(tg,aes(x=dose,y=length,fill=supp))+
  geom_line()+
  geom_point(size=4)
#somethimes points will overlap, so use position_dodge() to dodge them
ggplot(tg,aes(x=dose,y=length,shape=supp))+
  geom_line(position = position_dodge(.2))+
  geom_point(position = position_dodge(.2),size=4)

#change appearance of lines
#using linetype, colour and size in geom_line() or scale_fill_brewer(palette=...) or scale_fill_manual(values=c(...))
ggplot(tg,aes(x=dose,y=length,shape=supp))+
  geom_line(linetype='dashed',size=1,colour='darkgreen')+
  geom_point(size=3,colour='darkgreen')
ggplot(tg,aes(x=dose,y=length,shape=supp,color=supp))+
  geom_line()+
  geom_point()+
  scale_fill_brewer(palette = 'Set1') #但是这行删掉之后还是一样的图？？？？？？？？？？？
ggplot(tg,aes(x=dose,y=length,fill=supp))+
  geom_line()+
  geom_point(shape=21,size=3)+
  scale_fill_manual(values = c('black','white')) #只有geom_point()有设置参数时，scale_fill_manual才会对point生效

#graph with shaded area
sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)
ggplot(sunspotyear,aes(x=Year,y=Sunspots))+
  geom_area()
#using color,fill,alpha and size in geom_area. alpha=0.2 means 80% transparent
ggplot(sunspotyear,aes(x=Year,y=Sunspots))+
  geom_area(color='black',fill='blue',alpha=.2)

#Stacked Area Graph
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))+
  geom_area()
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))+
  geom_area(size=.2,alpha=.8,color='black')+
  scale_fill_brewer(palette = 'Blues',breaks=rev(levels(uspopage$AgeGroup)))  #rev也有问题？？？？？？？？
#using order=desc() to reverse stacked order
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup,order=desc(uspopage$AgeGroup)))+
  geom_area(alpha=.8,color='black')+
  scale_fill_brewer(palette = 'Blues') 
#上面代码的图，不仅有隔层之间的线条还有两面的线条，这样可能会让人误解，所以用下面的：
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup,order=desc(uspopage$AgeGroup)))+
  geom_area(alpha=.8,colour=NA)+
  geom_line(position = 'stack',size=.2)+
  scale_fill_brewer(palette = 'Blues')
