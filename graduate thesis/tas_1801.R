options(scipen = 200)
options(digits.secs=10)

source('/Users/xuchaoqun/大四/毕业论文选题/R_HFT/funxtion.R')

con <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/taq_1801.db')
tablenames <- as.data.frame(dbListTables(con))


#_______________________barc_taq_1801_______________________________________

barc_taq_1801 <- dbReadTable(con,'barc_taq_1801')

#第一步，清理
barc_taq_1801 <- clean(barc_taq_1801)

#第二，定时
barc_taq_1801 <- fixtime(barc_taq_1801,barc_taq_1801$`Date-Time`,'2018-01-01','2018-01-31')

#第三，转换格式
barc_taq_1801[,5:10] <- sapply(select(barc_taq_1801,Price:Ask_Size), as.numeric)

#第四,创建bvsv列
barc_taq_1801 <- bvsv(barc_taq_1801)

#第五，计算moib
barc_taq_1801$hm <- format(barc_taq_1801$`Date-Time`,'%Y%m%d%H%M')
barc_moib <- moib(barc_taq_1801)

#_____________________________bats_taq_1801____________
bats_taq_1801 <- dbReadTable(con,'bats_taq_1801')
bats_taq_1801 <- clean(bats_taq_1801)
bats_taq_1801 <- fixtime(bats_taq_1801,bats_taq_1801$`Date-Time`,'2018-01-01','2018-01-31')
bats_taq_1801[,5:10] <- sapply(select(bats_taq_1801,Price:Ask_Size), as.numeric)
bats_taq_1801 <- bvsv(bats_taq_1801)
bats_taq_1801$`Date-Time` <- ymd_hms(bats_taq_1801$`Date-Time`,tz='GB')
bats_taq_1801$hm <- format(bats_taq_1801$`Date-Time`,'%Y%m%d%H%M')
bats_moib <- moib(bats_taq_1801)


#___________________
for (i in dbListTables(con)[5:10]) {
  print(i)
  assign(i,dbReadTable(con,i))
  print('check raw data')
  print(head(get(i)))
  assign(i,clean(get(i)))
  print('after cleaning')
  print(head(get(i)))
  assign(i,fixtime(get(i),get(i)[,3],'2018-01-01','2018-01-31'))
  print('after fixing the time')
  print(head(get(i)))
  m <- get(i)
  m[,5:10] <- sapply(select(m,Price:Ask_Size),as.numeric)
  m <- bvsv(m)
  print('calculate bv sv')
  print(head(m))
  m[,3] <- ymd_hms(m[,3],tz= 'GB')
  m <- m %>% mutate(hm = format(m[,3],'%Y%m%d%H%M'))
  print('interval one minute')
  print(head(m))
  assign(paste(unlist(strsplit(i,'_'))[1],'moib',sep = '_'),moib(m))
  print('calculate moib')
  print(head(get(paste(unlist(strsplit(i,'_'))[1],'moib',sep = '_'))))
  
}



nrow(barc_moib)+nrow(bats_moib)+nrow(bp_moib)+nrow(crh_moib)+nrow(dge_moib)+
  nrow(glen_moib)+nrow(gsk_moib)+nrow(hsba_moib)+nrow(rio_moib)+nrow(vod_moib)
#合并十个moib
moib_1801 <- rbind(barc_moib, bats_moib,bp_moib,crh_moib,dge_moib,glen_moib,
       gsk_moib,hsba_moib,rio_moib,vod_moib)

dbWriteTable(con,'moib_1801',moib_1801)
dbDisconnect(con)
#描述性统计
su <- cbind(continue(barc_moib$moib),continue(bats_moib$moib),continue(bp_moib$moib),continue(crh_moib$moib),
      continue(dge_moib$moib),continue(glen_moib$moib),continue(gsk_moib$moib),continue(hsba_moib$moib),
      continue(rio_moib$moib),continue(vod_moib$moib))
write.csv(file = '/Users/xuchaoqun/大四/毕业论文选题/moib_stat.csv',su,fileEncoding = "GBK")

#分别保存
c <- c('barc_moib', 'bats_moib','bp_moib','crh_moib','dge_moib','glen_moib',
       'gsk_moib','hsba_moib','rio_moib','vod_moib')
for (i in c) {
  dbWriteTable(con,i,get(i))
}
dbDisconnect(con)

