#Bar Graph
library(ggplot2)
library(gcookbook)

# what is mapping ? 
# aes() is a mapping function, 所谓的映射即为数据集中的数据关联到相应的图形属性过程中一种对应关系。

###################### basic bar plot #####################################

ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat = 'identity')
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat = 'identity',fill='lightblue',colour='black') 
#change bar color with fill='',add line color with color=''
# and American spelling 'color'  works as well as British splling 'colour'


############### Group bar together to compare different types #############

cabbage_exp #this is a data set
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat = 'identity', position = 'dodge') 
#add a second variable with fill=... in aes(), dodge each ther horizontally with position='dodge' in geom_bar()
#to set color,use scale_fill_brewer() or scale_fill_manual(), as following:
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',position = 'dodge',color='black')+
  scale_fill_brewer(palette = 'Pastel1') 
#如果某个只有一个组，它就会占两个的面积，不美观，如下：
ca <- cabbage_exp[1:5,]
ggplot(ca,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',position = 'dodge',color='black')+
  scale_fill_brewer(palette = 'Pastel1') 


######################### bar graph of counts #############################

ggplot(diamonds,aes(x=cut))+geom_bar() #or geom_bar(stat='count')
ggplot(diamonds,aes(x=carat))+
  geom_bar() # if x is a continue variable, it will be histogram
ggplot(diamonds,aes(x=carat))+
  geom_histogram(binwidth = 0.25) # ??? 书上说这两图是一样的，但是我做的不一样.


######################### use color for bar plot ##########################

#use use the uspopchange data set for this example. It contains the percentage change in population for the US states from 2000 to 2010. We’ll take the top 10 fastest-growing states and graph their percentage change. We’ll also color the bars by region (Northeast, South, North Central, or West).
upc <- subset(uspopchange,rank(Change)>40) # rank(x)将x按照从小到大排序，这个data set共50个样本，所以rank(Change)>40就是取最大的十个
ggplot(upc,aes(x=State,y=Change,fill=Region))+
  geom_bar(stat = 'identity',color='black')+
  scale_fill_manual(values = c('#669933','#FFCC66'))+
  xlab('State')
ggplot(upc,aes(x=reorder(Abb,Change),y=Change,fill=Region))+ #use reorder() function
  geom_bar(stat = 'identity',color='black')+
  scale_fill_manual(values = c('#669933','#FFCC66'))+
  xlab('State')

################# color negative and positive bar different ################

cusb <- subset(climate,Source=='Berkeley' & Year >= 1900) #use climate data
cusb$pos <- cusb$Anomaly10y >= 0 
ggplot(cusb,aes(x=Year,y=Anomaly10y,fill=pos))+
  geom_bar(stat = 'identity',position = 'identity') #
ggplot(cusb,aes(x=Year,y=Anomaly10y,fill=pos))+
  geom_bar(stat = 'identity',position = 'identity',color='black',size = 0.25)+
  scale_fill_manual(values=c('#CCEEFF','#FFDDDD'),guide=FALSE) #注意，scale_fill_manual()才可以values=c(...),scale_fill_brewer不可以


########################### adjust width and space #########################

#using width=... in geom_bar() to adjust width
ggplot(pg_mean,aes(x=group,y=weight))+
  geom_bar(stat = 'identity',width = 0.5)
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity', position = 'dodge',width = 0.5)
#using position=position_dodge() to change sapce
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',width = 0.5,position = position_dodge(0.7))


############################## stacked bar graph ###########################

#without position='dodge', it is a stacked bar plot, as following:
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',width = 0.5)+
  guides(fill=guide_legend(reverse = TRUE)) #using guides() to reverse and specify which legend( fill in this case)
#adjust color and outline
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',width = 0.5, color='black')+
  guides(fill=guide_legend(reverse = TRUE))+
  scale_fill_brewer(palette = 'Pastel1')


######################### proportional stacked bar graph ###################

library(plyr)
ce <- ddply(cabbage_exp,.(Date),transform,
            precent_weight=Weight/sum(Weight)*100)
ggplot(ce,aes(x=Date,y=precent_weight,fill=Cultivar))+
  geom_bar(stat = 'identity')

################################### add labels ###############################

#using geom_text()
#vjust(vertical justification)/hjust(horizontal justification) for alignment
#label is what you want to add.
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=Weight),vjust=1.5,color='white') # or...
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=Weight),vjust=-0.5)

#当label在bar上方的时候，他有可能会到达y轴最大值，label就会被遮住，为了解决这个问题，我们手动设置ylim
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=Weight),vjust=-0.5)+
  ylim(0,max(cabbage_exp$Weight)*1.05)
#or...
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat = 'identity')+
  geom_text(aes(y=Weight+0.1,label=Weight))

#for grouped bar plot，we also can use position in geom_text()
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',position = position_dodge(1))+
  geom_text(aes(label=Weight),vjust=1.5,color='white',position = position_dodge(1),size=3)

#add labels to stacked bar
#using arrange(df,...),ddply() in plyr package
ce <- arrange(cabbage_exp,Date,disc(Cultivar)) # reorder by Date,then by Cultivar
ce <- ddply(ce,.(Date),transform,label_y=cumsum(Weight))
ggplot(ce,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity')+
  guides(fill=guide_legend(reverse = TRUE))+
  geom_text(aes(y=label_y,label=Weight),vjust=1.5,color='white')  
#??????????画出来的不对 <- 解决：arrange(... desc(Cultivar)),因为ggplot2的图例回根据一定的顺序排序，比如这里就是先c39后c52

# to add label in the medien place, add 'KG' behind number using paste() and conserve 2 digits after the decimal point by using format()
ce <- arrange(cabbage_exp,Date,Cultivar)
ce <- ddply(ce,'Date',transform,label_y=cumsum(Weight)-0.5*Weight)
ggplot(ce,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',color='black')+
  geom_text(aes(y=label_y,label=paste(format(Weight,nsmall = 2),'KG')),color='white',size = 4)+
  scale_fill_brewer(palette = 'Pastel1')+
  guides(fill=guide_legend(reverse=TRUE))  
#?????????????还是颠倒 同样的问题，desc(Cultivar)就可以了


########################## Claveland Dot Point ##############################

library(gcookbook)
tophip=tophitters2001[1:25,] # a data set for top 144 hitters, avg=batting average棒球平均击球率（成功率）
ggplot(tophip,aes(x=avg,y=name))+geom_point()

# order name by avg <-- reorder(name,avg)
ggplot(tophip,aes(x=avg,y=reorder(name,avg)))+
  geom_point(size=3)+                                   #larger dot
  theme_bw()+                                           #use theme() to change grid
  theme(panel.grid.major.x = element_blank(),           #make vertical grid line disappear
        panel.grid.minor.x = element_blank(),           #make vertical grid line disappear
        panel.grid.major.y = element_line(color = 'grey60',linetype = 'dashed'))     
                                                        #make horizontal grid dashed

#order name by lg and avg <-- reorder() cannot deal with 2 or more factor order levels,so...
nameorder <- tophip$name[order(tophip$lg,tophip$avg)]
tophip$name <- factor(tophip$name,levels = nameorder)
#seperate to AL(American League) and NL(National League)
#make the line go only to the point by using geom_segment()
#facet_grid 根据lg分成了两个图
#aes(color-lg) in geom_point() 给了lg不同颜色
ggplot(tophip,aes(x=avg,y=name))+
  geom_segment(aes(yend=name),xend=0,color='grey50')+
  geom_point(size=3,aes(color=lg))+
  scale_fill_brewer(palette = 'Pastel1',limits=c('NL','AL'),guide=FALSE)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank())+
  facet_grid(lg~.,scales='free_y',space='free_y')
