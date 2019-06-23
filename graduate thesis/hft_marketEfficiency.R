

#回归
con <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/taq_1801.db')
dge_taq_1801 <- dbReadTable(con,'dge_taq_1801')
dge_taq_1801 <- clean(dge_taq_1801)
dge_taq_1801 <- fixtime(dge_taq_1801,dge_taq_1801$`Date-Time`,'2018-01-01','2018-01-31')
dge_taq_1801[,5:10] <- sapply(select(dge_taq_1801,Price:Ask_Size), as.numeric)
dge_taq_1801$`Date-Time` <- ymd_hms(dge_taq_1801$`Date-Time`,tz='GB')
dge_taq_1801$hm <- format(dge_taq_1801$`Date-Time`,'%Y%m%d%H%M')
dge_tradevolume <- dge_taq_1801 %>%
  select(RIC,hm,Volume) %>%
  group_by(hm) %>% 
  summarise(SumVolume = sum(Volume,na.rm = T))

equation <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/equation.db')
dge_equ <- dbReadTable(equation,'dge_equ')

dge_ihv <- full_join(dge_tradevolume,dge_hft,by = 'hm')
dge_ihv <- left_join(dge_equ,dge_ihv,by = 'hm')
dge_ihv$hft_act <- na.locf(dge_ihv$hft_act)

dge_effi_moib <- lm(moib~hft_act+SumVolume,data = dge_ihv)
dge_effi_inner <- lm(inner~hft_act+SumVolume,data = dge_ihv)
dge_effi_middle <- lm(middle~hft_act+SumVolume,data = dge_ihv)
dge_effi_outer <- lm(outer~hft_act+SumVolume,data = dge_ihv)

dge_eff_coe <- rbind(summary(dge_effi_moib)$coefficients,summary(dge_effi_inner)$coefficients,
                      summary(dge_effi_middle)$coefficients,summary(dge_effi_outer)$coefficients)

write.csv(dge_eff_coe,'/Users/xuchaoqun/大四/毕业论文选题/hft与市场效率/dge_eff_coe.csv')

#计算spread，回归
con <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/taq_1801.db')
barc_taq_1801 <- dbReadTable(con,'barc_taq_1801')
barc_taq_1801 <- clean_taq(barc_taq_1801)
barc_taq_1801 <- fixtime(barc_taq_1801,barc_taq_1801$`Date-Time`,'2018-01-01','2018-01-31')
barc_taq_1801[,7] <- as.numeric(barc_taq_1801[,7])
barc_taq_1801[,9] <- as.numeric(barc_taq_1801[,9])
barc_taq_1801 <- barc_taq_1801 %>% mutate(spread = Ask_Price - Bid_Price)
barc_taq_1801 <- barc_taq_1801 %>% mutate(hm=format(Date_Time,'%Y%m%d%H%M'))
barc_taq_1801 <- barc_taq_1801 %>% group_by(hm) %>% summary(spread = mean(spread, na.rm = T))


