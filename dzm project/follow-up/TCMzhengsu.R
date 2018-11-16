library(plyr)
library(ggplot2)
library(data.table)

source('/Users/xuchaoqun/statistics/tumour/东直门/基线描述0805/数据预处理.R')
source('/Users/xuchaoqun/statistics/tumour/东直门/基线描述0805/Function.R')

setwd('/Users/xuchaoqun/statistics/tumour/东直门/随访数据分析')
#zs随访分析
dzm_nx <- read.csv('/Users/xuchaoqun/statistics/tumour/东直门/东直门181024导出-1026修改确认/随访-中医证型-内虚.csv',header = TRUE, fileEncoding = 'GBK',na.strings = "")
dzm_xs <- read.csv('/Users/xuchaoqun/statistics/tumour/东直门/东直门181024导出-1026修改确认/随访-中医证型-邪实.csv',header = TRUE, fileEncoding = 'GBK',na.strings = "")
names(dzm_nx) <- c('序号','患者ID','随访次数','部位','症状')
dzm_nx$症状 <- as.character(dzm_nx$症状)
dzm_xs$症状 <- as.character(dzm_xs$症状)
dzm_zs <- rbind(dzm_xs[c('患者ID','随访次数','症状')],dzm_nx[c('患者ID','随访次数','症状')])
names(dzm_zs) <- c('PatientID','随访次数','证素')
adsldzm_g <- data.table(adsldzm[c('PatientID','组别')])
zs_zs_dt <- data.table(dzm_zs)
zs_zs_dt$PatientID <- as.character(zs_zs_dt$PatientID)
zs_sg <- left_join(adsldzm_g,zs_zs_dt,by='PatientID',all = T)
zs_sg <- subset(zs_sg,!is.na(zs_sg$证素)&!is.nan(zs_sg$证素)&!is.infinite(zs_sg$证素))
zs_sg$证素归类 <- ifelse(zs_sg$证素=='气虚','气虚证',
                     ifelse(zs_sg$证素=='血虚','血虚证',
                            ifelse(zs_sg$证素=='阳虚','阳虚证',
                                   ifelse(zs_sg$证素=='阴虚','阴虚证',
                                          ifelse(zs_sg$证素=='湿'|zs_sg$证素=='痰','痰湿证',
                                                 ifelse(zs_sg$证素=='瘀','血瘀证','其他'))))))

#离散型表述
write.table(discrete(zs_sg$证素归类),file = 'zs_discr.csv',col.names = TRUE, row.names = TRUE , sep = ',',fileEncoding = 'GBK')

#绘制箱线图
m <- plyr::count(zs_sg[c('组别','随访次数','证素归类')])
names(m) <- c('组别','随访次数','证素归类','频数')
m$随访次数 <- as.factor(m$随访次数)
ggplot(m,aes(x=频数,y=随访次数))+
  geom_segment(aes(yend=随访次数),xend=0,color='grey50')+
  geom_point(size = 3, aes(color = 证素归类))+
  scale_fill_brewer(palette = 'Pastel1',limits=c('NL','AL'),guide=FALSE)+
  theme_bw(base_family='STKaiti')+
  theme(panel.grid.major.y = element_blank())+
  facet_grid(组别~.,scales='free_y',space='free_y')+
  xlab('频数')+
  ylab('随访次数（次）')

ggplot(m[which(m$随访次数==6),],aes(x=频数,y=证素归类))+
  geom_segment(aes(yend=证素归类),xend=0,color='grey50')+
  geom_point(size = 3, aes(color = 证素归类))+
  scale_fill_brewer(palette = 'Pastel1',limits=c('NL','AL'),guide=FALSE)+
  theme_bw(base_family='STKaiti')+
  theme(panel.grid.major.y = element_blank())+
  facet_grid(组别~.,scales='free_y',space='free_y')+
  xlab('频数')+
  ylab('随访次数（次）')

#三维图
zs_table_3df <- ftable(xtabs(~随访次数+证素归类+组别,data = zs_sg),row.vars = c(3,2))
write.table(zs_table_3df,'zs_table_3df.csv',col.names = T, row.names = T , sep = ',')

