options(scipen = 200)
options(digits.secs=10)
library(car)

source('/Users/xuchaoqun/大四/毕业论文选题/R_HFT/funxtion.R')
con <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/taq_1801.db')
conn <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/depth_1801.db')
mq_ret_loib_1801.db <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/mq_ret_loib_1801.db')

# moib_1801 <- dbReadTable(con,'moib_1801')
barc_moib <- dbReadTable(con,'barc_moib')
barc_return <- dbReadTable(mq_ret_loib_1801.db,'barc_return')
barc_inner <- dbReadTable(mq_ret_loib_1801.db,'barc_inner')
barc_middle <- dbReadTable(mq_ret_loib_1801.db,'barc_middle')
barc_outer <- dbReadTable(mq_ret_loib_1801.db,'barc_outer')

barc_equ <- full_join(barc_moib,barc_inner,by=c('RIC','hm'))
barc_equ <- full_join(barc_equ,barc_middle,by=c('RIC','hm'))
barc_equ <- full_join(barc_equ,barc_outer,by=c('RIC','hm'))
barc_equ <- full_join(barc_equ,barc_return,by=c('RIC','hm'))

barc_equ <- barc_equ %>% mutate_all(funs(na.locf(.)))
barc_lm <- lm(lead(return)~return+moib+middle+inner+outer,data = barc_equ)

# m <- as.data.frame(seq(ymd_hms('2018-01-02 08:00:00',tz = 'GB'),ymd_hms('2018-01-31 16:29:59',tz = 'GB'),by = '1 min'))
# m <- format(m,'%Y%m%d%H%M')
# colnames(m) <- 'hm'
# m <- left_join(m,barc_equ,by='hm')
# m <- na.locf(m)
# m[,3:8] <- sapply(select(m,moib:return),as.numeric)
# lm <- lm(lead(return)~return+moib+inner+outer,data = m)

date <- c('2018-01-02','2018-01-03','2018-01-04','2018-01-05','2018-01-08',
          '2018-01-09','2018-01-10','2018-01-11','2018-01-12','2018-01-15',
          '2018-01-16','2018-01-17','2018-01-18','2018-01-19','2018-01-22',
          '2018-01-23','2018-01-24','2018-01-25','2018-01-26','2018-01-29',
          '2018-01-30','2018-01-31')

barc_rsq <- vector()
for (i in date) {
  m <- as.data.frame(seq(ymd_hms(paste(i,'08:00:00',sep = ' '),tz = 'GB'),ymd_hms(paste(i,'16:29:59',sep = ' '),tz = 'GB'),by = '1 min'))
  m <- format(m,'%Y%m%d%H%M')
  colnames(m) <- 'hm'
  m <- left_join(m,barc_equ,by='hm')
  m <- na.locf(m)
  m[,3:8] <- sapply(select(m,moib:return),as.numeric)
  lm <- lm(lead(return)~return+moib+middle+inner+outer,data = m)
  assign(paste('lm',i,sep = '.'),summary(lm)$coefficients)
  barc_rsq <- c(barc_rsq,summary(lm)$r.squared)

}

write(as.list(barc_rsq),'/Users/xuchaoqun/大四/毕业论文选题/rsq/barc_rsq.csv')

# assign(paste('lm',i,sep = '.'),summary(lm)$coefficients)

lm <- summary(lm(lead(return)~return+moib+inner+middle+outer,data = barc_equ))$coefficients
coe <- rbind(`lm.2018-01-02`,`lm.2018-01-03`,`lm.2018-01-04`,`lm.2018-01-05`,`lm.2018-01-08`,
             `lm.2018-01-09`,`lm.2018-01-10`,`lm.2018-01-11`,`lm.2018-01-12`,`lm.2018-01-15`,
             `lm.2018-01-16`,`lm.2018-01-17`,`lm.2018-01-18`,`lm.2018-01-19`,`lm.2018-01-22`,
             `lm.2018-01-23`,`lm.2018-01-24`,`lm.2018-01-25`,`lm.2018-01-26`,`lm.2018-01-29`,
             `lm.2018-01-30`,`lm.2018-01-31`,lm)
write.csv(coe,'/Users/xuchaoqun/大四/毕业论文选题/barc_equ_coe1.csv')



euq <- rbind(barc_equ,bats_equ,bp_equ,crh_equ,dge_equ,glen_equ,gsk_equ,hsba_equ,rio_equ,barc_equ)
lm <- lm(lead(return)~return+moib+inner+middle+outer,data = equ)
