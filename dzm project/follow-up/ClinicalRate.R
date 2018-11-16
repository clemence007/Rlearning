library(plyr)
library(ggplot2)
library(data.table)
library(dplyr)

source('/Users/xuchaoqun/statistics/tumour/东直门/基线描述0805/数据预处理.R')
source('/Users/xuchaoqun/statistics/tumour/东直门/基线描述0805/Function.R')

#zylc随访分析
dzm_zylc <- read.csv('/Users/xuchaoqun/statistics/tumour/东直门/东直门181024导出-1026修改确认/随访-中医临床症状评分表.csv',header = TRUE, fileEncoding = 'GBK',na.strings = "")
zylc_sum <- ddply(dzm_zylc, .(患者ID,随访次数),summarise,sum=sum(分值))
zylc_sum <- na.omit(zylc_sum)
adsldzm_g <- data.table(adsldzm[c('PatientID','组别')])
zylc_sum_dt <- data.table(zylc_sum)
names(zylc_sum_dt) <- c('PatientID','times','zylc_sum')
zylc_sg <- left_join(adsldzm_g,zylc_sum_dt,by='PatientID',all = T)
zylc_sg$zylc_sum <- ifelse(zylc_sg$zylc_sum==0,NA,zylc_sg$zylc_sum)

#绘制散点图+loess曲线
l <- ggplot(zylc_sg,aes(x=times,y=zylc_sum,color=组别))+
  geom_point()+
  stat_smooth(method = loess)+
  theme_bw(base_family='STKaiti')+
  labs(x='随访次数(次)',y='中医临床症状评分(分)')

#平均数表述
zylc_mean <- ddply(na.omit(zylc_sg),.(PatientID),summarise,mean=mean(zylc_sum))
adsldzm_dt <- data.table(adsldzm)
zylc_mean_dt <- data.table(zylc_mean)
names(zylc_mean_dt) <- c('PatientID','zylc_mean')
zylc <- left_join(adsldzm_dt,zylc_mean_dt,by='PatientID')
continue(zylc$zylc_mean)
write.table(continue(zylc$zylc_mean),file = 'zylc_conti.csv',col.names = TRUE, row.names = TRUE , sep = ',',fileEncoding = 'GBK')


zylc_count <- ftable(xtabs(~组别+times,data = na.omit(zylc_sg)))
zylc_mean2 <- ddply(zylc_sg,.(组别,times),summarise,mean=mean(zylc_sum,na.rm = TRUE))
write.table(zylc_count,file = 'zylc_count.csv',col.names = TRUE, row.names = TRUE , sep = ',',fileEncoding = 'GBK')
write.table(t(zylc_mean2),file = 'zylc_mean2.csv',col.names = TRUE, row.names = TRUE , sep = ',',fileEncoding = 'GBK')


#残差图
zylc_mean3 <- ddply(zylc_sg,.(times),summarise,mean=mean(zylc_sum,na.rm = TRUE))
zylc_var <- ddply(na.omit(zylc_sg),.(times),summarise,std = var(zylc_sum))
zylc_sg1 <- left_join(zylc_mean3,zylc_sg,by='times')
zylc_sg1 <- left_join(zylc_var,zylc_sg1,by='times')
zylc_sg1$residual <- (zylc_sg1$zylc_sum - zylc_sg1$mean) / zylc_sg1$std
r <- ggplot(zylc_sg1,aes(x=times,y=residual,color=PatientID))+
  geom_line()+
  theme_bw(base_family='STKaiti')+
  theme(legend.position = 'None')+
  labs(x='随访次数(次)',y='zylc评分残差')
