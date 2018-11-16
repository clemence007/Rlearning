library(plyr)
library(ggplot2)
library(data.table)

source('/Users/xuchaoqun/statistics/tumour/东直门/基线描述0805/数据预处理.R')
source('/Users/xuchaoqun/statistics/tumour/东直门/基线描述0805/Function.R')

setwd('/Users/xuchaoqun/statistics/tumour/东直门/随访数据分析')
#nhxb随访分析
dzm_nhxb <- read.csv('/Users/xuchaoqun/statistics/tumour/东直门/东直门181024导出-1026修改确认/随访-辅助检查-尿常规.csv',header = TRUE, fileEncoding = 'GBK',na.strings = "")
dzm_nhxb$结果 <- as.numeric(as.character(dzm_nhxb$结果))
nhxb_sum <- subset(dzm_nhxb,项目=='红细胞')
nhxb_sum <- subset(nhxb_sum,!is.na(nhxb_sum$结果) & !is.nan(nhxb_sum$结果) & !is.infinite(nhxb_sum$结果))
adsldzm_g <- data.table(adsldzm[c('PatientID','组别')])
nhxb_sum_dt <- data.table(nhxb_sum)
names(nhxb_sum_dt) <- c('序号','PatientID','times','采样日期','项目','nhxb_sum','参考下限','参考上限','单位','是否异常')
nhxb_sum_dt$PatientID <- as.character(nhxb_sum_dt$PatientID)
nhxb_sg <- left_join(adsldzm_g,nhxb_sum_dt,by='PatientID',all = T)
nhxb_sg <- subset(nhxb_sg,!is.na(nhxb_sg$nhxb_sum)&!is.nan(nhxb_sg$nhxb_sum)&!is.infinite(nhxb_sg$nhxb_sum))

#绘制箱线图
l <- ggplot(nhxb_sg,aes(x=factor(times),y=nhxb_sum,color=组别))+
  geom_boxplot()+
  theme_bw(base_family='STKaiti')+
  labs(x='随访次数(次)',y='尿红细胞')

#平均数表述
nhxb_mean <- ddply(na.omit(nhxb_sg),.(PatientID),summarise,mean=mean(nhxb_sum))
adsldzm_dt <- data.table(adsldzm)
nhxb_mean_dt <- data.table(nhxb_mean)
names(nhxb_mean_dt) <- c('PatientID','nhxb_mean')
nhxb <- left_join(adsldzm_dt,nhxb_mean_dt,by='PatientID')
continue(nhxb$nhxb_mean)
write.table(continue(nhxb$nhxb_mean),file = 'nhxb_conti.csv',col.names = TRUE, row.names = TRUE , sep = ',',fileEncoding = 'GBK')


nhxb_count <- ftable(xtabs(~组别+times,data = na.omit(nhxb_sg)))
nhxb_mean2 <- ddply(nhxb_sg,.(组别,times),summarise,mean=mean(nhxb_sum,na.rm = TRUE))
write.table(nhxb_count,file = 'nhxb_count.csv',col.names = TRUE, row.names = TRUE , sep = ',',fileEncoding = 'GBK')
write.table(t(nhxb_mean2),file = 'nhxb_mean2.csv',col.names = TRUE, row.names = TRUE , sep = ',',fileEncoding = 'GBK')

#三维图
nhxb_table_3df <- ftable(xtabs(~times+是否异常+组别,data = nhxb_sg),row.vars = c(3,2))
write.table(nhxb_table_3df,'nhxb_table_3df.csv',col.names = T, row.names = T , sep = ',')


#残差图
nhxb_mean3 <- ddply(nhxb_sg,.(times),summarise,mean=mean(nhxb_sum,na.rm = TRUE))
nhxb_var <- ddply(na.omit(nhxb_sg),.(times),summarise,std = var(nhxb_sum))
nhxb_sg1 <- left_join(nhxb_mean3,nhxb_sg,by='times')
nhxb_sg1 <- left_join(nhxb_var,nhxb_sg1,by='times')
nhxb_sg1$residual <- (nhxb_sg1$nhxb_sum - nhxb_sg1$mean) / nhxb_sg1$std
r <- ggplot(nhxb_sg1,aes(x=times,y=residual,color=PatientID))+
  geom_point()+
  geom_line()+
  theme_bw(base_family='STKaiti')+
  theme(legend.position = 'None')+
  labs(x='随访次数(次)',y='尿红细胞残差')
