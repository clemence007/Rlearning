
##############################前期数据处理#######################

library(xlsx)

#载入数据
adsldzm <- read.csv("/Users/xuchaoqun/statistics/tumour/东直门/东直门0809的副本.csv",header = TRUE,fileEncoding = "GBK",na.strings="")
#载入分析数据说明（变量中英文名称）
namedata <- read.xlsx('/Users/xuchaoqun/statistics/tumour/东直门/introduction_dzm.xlsx',1)


#根据医院要求修改变量chem_yn和Target_yn，使其与Trtcat一致
adsldzm$chem_yn <- ifelse(grepl('.*化疗.*',adsldzm$Trtcat),'是','否')
adsldzm$Tartget_yn <- ifelse(grepl('.*靶向.*',adsldzm$Trtcat),'是','否')

#bmi分组。
adsldzm$bmiG <- ifelse(adsldzm$bmi<18.5,"低于正常值",ifelse(adsldzm$bmi<=23.9,"正常值","高于正常值"))
adsldzm$bmiG <- factor(adsldzm$bmiG,levels = c('低于正常值','正常值','高于正常值'))

#年龄分组修正（原基线数据错误）
adsldzm$age0 <- ifelse(adsldzm$age<=49,'<=49',ifelse(adsldzm$age<=69,'50-69','>=70'))
adsldzm$age0 <- factor(adsldzm$age0,levels = c('<=49','50-69','>=70'))


#将变量分类：adsldzm1是continue，adsldzm2是discrete
#筛出连续性变量
adsldzm1 <- adsldzm[c('OS0','age','kps','height','weight','bmi','Anderson0','life_score0','TCM_Score0')]

#筛出离散变量
adsldzm2 <- adsldzm[c("Censor","组别",'last_visit','survival_status','age0','kps0',
                      'sex','Grade','typea','B_HER','B_Lauren','sugery_yn','trans_yn',
                      'Pos','Pos1','TRTEAT','SYMPTOM','MOOD','COST','TCM_Pat','neixu_sub',
                      'xieshi1','xieshi2','xieshi3','neixu','xieshi','trans','chem_yn',
                      'Tartget_yn','Radio_yn','Trtcat','bmiG','中医主症1评分','中医主症2评分')]

#将确诊IV期后的治疗方法根据分析计划分为三组：
#  化疗+靶向；单独化疗；单独中药
adsldzm$Trtcat1 <- ifelse(grepl('.*化.*靶.*',adsldzm$Trtcat),'化疗+靶向',
                          ifelse(grepl('.*化',adsldzm$Trtcat),'单独化疗','单独中药'))
adsldzm2$Trtcat <- adsldzm$Trtcat1

#将确诊IV期后的治疗方法根据分析计划分为几线化疗四组：
# 一线化疗；二线化疗；三线化疗；单纯中药
adsldzm$Trtcat2 <- ifelse(grepl('一线化疗.*',adsldzm$Trtcat),'一线化疗',
                          ifelse(grepl('二线化疗.*',adsldzm$Trtcat),'二线化疗',
                                 ifelse(grepl('三线化疗.*',adsldzm$Trtcat),'三线化疗','单纯中药')))


#根据分析计划，将內虚(无缺失)分为：
#  按脏腑归类：脾胃虚证、心虚证、肺虚证、肝虚证、肾虚证、脾胃虚证+心/肺虚证、脾胃虚证+肝/肾虚证、脾胃虚证+心/肺虚证+肝/肾虚证
adsldzm2$neixu <- ifelse(grepl('脾',adsldzm$neixu)|grepl('胃',adsldzm$neixu),
                         ifelse((grepl('心',adsldzm$neixu)|grepl('肺',adsldzm$neixu))&(grepl('肾',adsldzm$neixu)|grepl('肝',adsldzm$neixu)),'脾胃虚证+心/肺虚证+肝/肾虚证',
                                ifelse(grepl('心',adsldzm$neixu)|grepl('肺',adsldzm$neixu),'脾胃虚证+心/肺虚症',
                                       ifelse(grepl('肾',adsldzm$neixu)|grepl('肝',adsldzm$neixu),'脾胃虚证+肾/肝虚症','脾胃虚证'))),
                         ifelse(grepl('心',adsldzm$neixu),'心虚症',
                                ifelse(grepl('肺',adsldzm$neixu),'肺虚证',
                                       ifelse(grepl('肾',adsldzm$neixu),'肾虚症',
                                              ifelse(grepl('肝',adsldzm$neixu),'肝虚证',
                                                     ifelse(adsldzm$neixu=='其他','其他',NA))))))



#根据分析计划，将邪实(一个缺失)分为：
#  湿痰症；血瘀证；其他；NA
adsldzm2$xieshi <- ifelse((grepl('湿',adsldzm$xieshi)|grepl('痰',adsldzm$xieshi))&grepl('瘀',adsldzm$xieshi),'痰湿证+血瘀证',
                          ifelse(grepl('湿',adsldzm$xieshi)|grepl('痰',adsldzm$xieshi),'痰湿证',
                                 ifelse(grepl('瘀',adsldzm$xieshi),'血瘀证',
                                        ifelse(adsldzm$xieshi=='其他','其他',NA))))


#中医主症归类
zhuzheng <- c(adsldzm$中医主症1评分,adsldzm$中医主症2评分)
zhuzheng <- factor(zhuzheng)
zzclass <- ifelse(zhuzheng==7,'纳差',
                  ifelse(zhuzheng==9,'乏力',
                         ifelse(zhuzheng==1|zhuzheng==2,'腹部症状',
                                ifelse(zhuzheng==3|zhuzheng==4|zhuzheng==5|zhuzheng==6,'上消化道症状',
                                       ifelse(zhuzheng==10|zhuzheng==11,'大便',
                                              ifelse(zhuzheng==8|zhuzheng==12|zhuzheng==13|zhuzheng==14|zhuzheng==15,'其他',NA))))))
m <- as.data.frame(zzclass)

adsldzm2$B_Lauren <- ifelse(adsldzm$B_Lauren=='未查','未检测',as.character(adsldzm$B_Lauren))

#将字符串变量改为因子变量：
adsldzm2$Trtcat <- factor(adsldzm2$Trtcat)
adsldzm2$neixu <- factor(adsldzm2$neixu)
adsldzm2$xieshi <- factor(adsldzm2$xieshi)
adsldzm2$Censor <- factor(adsldzm2$Censor)
adsldzm2$last_visit <- factor(adsldzm2$last_visit)
adsldzm2$B_Lauren <- factor(adsldzm2$B_Lauren)
