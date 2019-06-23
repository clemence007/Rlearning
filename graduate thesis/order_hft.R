options(scipen = 200)
op <- options(digits.secs=14)
library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyverse)
source('/Users/xuchaoqun/大四/毕业论文选题/R_HFT/funxtion.R')

order_1801 <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/order_1801.db')
hft_1801 <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/hft_1801.db')

dge_order_1801 <- dbReadTable(order_1801,'dge_order_1801')
dge_order <- dge_order_1801
#整理数据
# colnames(dge_order)
dge_order <- clean_order(dge_order)
#给DELETE加上时间
dge_order <- dge_order[2:nrow(dge_order),]
dge_order$PR_DATE <- na.locf(dge_order$PR_DATE)
dge_order$LV_TIM_NS <- na.locf(dge_order$LV_TIM_NS)
#给DELETE机上side、price、size
dge_order <- dge_order %>% group_by(MsgNum) %>% mutate_all(funs(na.locf(.,na.rm = F)))
dge_order <- na.omit(dge_order)
dge_order <- dge_order %>% mutate(Date.Time = paste(PR_DATE,LV_TIM_NS))
dge_order$Date.Time <- ymd_hms(dge_order$Date.Time)

dge_order <- dge_order %>% 
  arrange(ORDER_SIZE,LV_TIM_NS) %>% 
  group_by(ORDER_SIZE) %>%
  mutate(difft = c(diff(Date.Time),0))
dge_order <- dge_order %>% 
  arrange(ORDER_SIZE,LV_TIM_NS) %>% 
  group_by(ORDER_SIZE) %>%
  mutate(difftrev = rev(c(as.vector(diff(rev(Date.Time))),NA)))

dge_order <- dge_order %>% mutate(hft = if_else(abs(difft)<0.1|abs(difftrev)<0.1,1,0))

dge_order <- dge_order %>% group_by(MsgNum) %>% mutate(hft = if_else(any(hft==1),1,0))

plyr::count(dge_order$hft)

# dge_order <- dge_order %>% 
#   select(Date.Time,MsgNum,UpdateType.Action,ORDER_SIDE,ORDER_PRC,ORDER_SIZE,hft) %>%
#   arrange(Date.Time)

dge_hft <- dge_order %>% 
  mutate(hm=format(Date.Time,'%Y%m%d%H%M')) %>% 
  select(hm,hft,MsgNum) %>%
  group_by(hm) %>% summarise(hft_act = plyr::count(hft)$freq[2]/length(hft))

dbWriteTable(hft_1801,'dge_hft',dge_hft)


#回归

# a <- dge_order %>% filter(ORDER_SIZE==1089) %>% arrange(Date.Time) 
# write.csv(a,'/Users/xuchaoqun/大四/毕业论文选题/order_clean_hft.csv')
