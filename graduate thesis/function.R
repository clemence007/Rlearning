options(scipen = 200)
options(digits.secs=10)
library(DBI)
library(RSQLite)
library(zoo)
library(lubridate)
library(dplyr)
library(xts)
library(highfrequency)
library(tidyr)

#固定时间函数fixtime
#y,z的格式是“2018-02-01”,y表示起始日期，z表示结束日期，data就是data，v表示具体到那一列
fixtime <- function(data,v,y,z){
  
  v <- ymd_hms(v,tz='GB')
  startdate <- as.Date(y) 
  enddate <- as.Date(z)
  ndays <- enddate - startdate + 1
  tt <- ts(1:ndays, frequency =1, start =as.Date(y))
  ss <- as.Date(y)
  dates <- seq(from=ss, by=1, length.out=ndays)
  tt <- data.frame(dates,tt)
  tt$starttime <- paste(tt$dates,'08:00:00',seq = ' ')
  tt$starttime <- ymd_hms(tt$starttime, tz = "GB")
  tt$endtime <- paste(tt$dates,'16:30:00',seq = ' ')
  tt$endtime <- ymd_hms(tt$endtime, tz = "GB")
  int1 <- interval(tt$starttime,tt$endtime,tz = 'GB')
  
  fixt <- dplyr::filter(data,v %within% int1[1] | v %within% int1[2] |
                   v %within% int1[3] | v %within% int1[4] |
                   v %within% int1[5] | v %within% int1[6] |
                   v %within% int1[7] | v %within% int1[8] |
                   v %within% int1[9] | v %within% int1[10] |
                   v %within% int1[11] | v %within% int1[12] |
                   v %within% int1[13] | v %within% int1[14] |
                   v %within% int1[15] | v %within% int1[16] |
                   v %within% int1[17] | v %within% int1[18] |
                   v %within% int1[19] | v %within% int1[20] |
                   v %within% int1[21] | v %within% int1[22] |
                   v %within% int1[23] | v %within% int1[24] |
                   v %within% int1[25] | v %within% int1[26] |
                   v %within% int1[27] | v %within% int1[28] |
                   v %within% int1[29] | v %within% int1[30] |
                   v %within% int1[31] )
  return(fixt)
}

#数据清理的函数,包括修改名称，去掉price错误
clean <- function(data){
  data <- data[-1,]
  colnames(data) <- c("RIC","Domain","Date-Time","Type","Price","Volume","Bid_Price","Bid_Size",
                              "Ask_Price","Ask_Size")

  data <- data %>% do(na.locf(.))
  data <- data %>% filter(Bid_Price > 0 ) 
  data <- data %>% filter(Ask_Price > 0 ) 
  data <- data %>% filter(Type == 'Trade')
  
  return(data)
  
}

clean_taq <- function(data){
  data <- data[-1,]
  colnames(data) <- c("RIC","Domain","Date-Time","Type","Price","Volume","Bid_Price","Bid_Size",
                      "Ask_Price","Ask_Size")
  
  data <- data %>% do(na.locf(.))
  data <- data %>% filter(Bid_Price > 0 ) 
  data <- data %>% filter(Ask_Price > 0 ) 
  data <- data %>% filter(Ask_Price > Bid_Price ) 
  
  return(data)
  
}


#创建bvsv列
bvsv <- function(data){
  data <- data %>% mutate(Seller_ini = if_else(abs(Price-Bid_Price)<abs(Price-Ask_Price),Volume,0))
  data <- data %>% mutate(Buyer_ini = if_else(abs(Price-Bid_Price)>abs(Price-Ask_Price),Volume,0))
  return(data)
}
#计算moib
moib <- function(data){
  newdata <- data %>% group_by(RIC,hm) %>% summarise(moib = (sum(Buyer_ini)-sum(Seller_ini))/(sum(Buyer_ini)+sum(Seller_ini)))
  return(newdata)
}

#描述性统计
continue <- function(x){
  s <- nrow(data.frame(na.omit(x)))
  lost <- 11220-s
  summ <- summary(na.omit(x))[2:5]
  sdt <- sd(na.omit(x))
  value <- c(s,lost,summ,sdt)
  names(value) <- c('有效间隔（个）','缺失间隔（个）','下四分位','中位数','均值','上四分位','标准差')
  value <- format(value,digits = 3)
  value <- as.data.frame(value)
  colnames(value) <- deparse(substitute(x))
  return(value)
}

#---------------------------------------------------------------------------
inner <- function(data){
  data <- data %>% 
    mutate( inner1 = (L1_BidSize+L2_BidSize+L3_BidSize+L4_BidSize+
                        L5_BidSize+L6_BidSize+L7_BidSize+L8_BidSize+
                        L9_BidSize+L10_BidSize))
  data <- data %>% 
    mutate( inner2 =  (L1_AskSize+L2_AskSize+L3_AskSize+L4_AskSize+
                         L5_AskSize+L6_AskSize+L7_AskSize+L8_AskSize+
                         L9_AskSize+L10_AskSize))
  
  data <- data %>% mutate(inner = (inner1-inner2)/(inner1+inner2) )
  
  data <- data %>% dplyr::filter(!is.nan(inner))
  data <- data %>% mutate(hm=format(Date_Time,'%Y%m%d%H%M'))
  newdata <- data %>% group_by(RIC,hm) %>% summarise(inner = mean(inner))
  return(newdata)
}

#--------------------------------------------------------------------------------
inner_sele_ask <- function(data){
  newdata <- data %>% mutate(L1_AskSize = if_else(abs(L1_AskPrice - midquote)<=height/3, L1_AskSize,0),
                                           L2_AskSize = if_else(abs(L2_AskPrice - midquote)<=height/3, L2_AskSize,0),
                                           L3_AskSize = if_else(abs(L3_AskPrice - midquote)<=height/3, L3_AskSize,0),
                                           L4_AskSize = if_else(abs(L4_AskPrice - midquote)<=height/3, L4_AskSize,0),
                                           L5_AskSize = if_else(abs(L5_AskPrice - midquote)<=height/3, L5_AskSize,0),
                                           L6_AskSize = if_else(abs(L6_AskPrice - midquote)<=height/3, L6_AskSize,0),
                                           L7_AskSize = if_else(abs(L7_AskPrice - midquote)<=height/3, L7_AskSize,0),
                                           L8_AskSize = if_else(abs(L8_AskPrice - midquote)<=height/3, L8_AskSize,0),
                                           L9_AskSize = if_else(abs(L9_AskPrice - midquote)<=height/3, L9_AskSize,0),
                                           L10_AskSize = if_else(abs(L10_AskPrice - midquote)<=height/3, L10_AskSize,0))
  return(newdata)
}


#--------------------------------------------------------------------------------
inner_sele_bid <- function(data){
  newdata <- data %>% mutate(L1_BidSize = if_else(abs(L1_BidPrice - midquote)<=height/3, L1_BidSize,0),
                                      L2_BidSize = if_else(abs(L2_BidPrice - midquote)<=height/3, L2_BidSize,0),
                                      L3_BidSize = if_else(abs(L3_BidPrice - midquote)<=height/3, L3_BidSize,0),
                                      L4_BidSize = if_else(abs(L4_BidPrice - midquote)<=height/3, L4_BidSize,0),
                                      L5_BidSize = if_else(abs(L5_BidPrice - midquote)<=height/3, L5_BidSize,0),
                                      L6_BidSize = if_else(abs(L6_BidPrice - midquote)<=height/3, L6_BidSize,0),
                                      L7_BidSize = if_else(abs(L7_BidPrice - midquote)<=height/3, L7_BidSize,0),
                                      L8_BidSize = if_else(abs(L8_BidPrice - midquote)<=height/3, L8_BidSize,0),
                                      L9_BidSize = if_else(abs(L9_BidPrice - midquote)<=height/3, L9_BidSize,0),
                                      L10_BidSize = if_else(abs(L10_BidPrice - midquote)<=height/3, L10_BidSize,0))
  return(newdata)
}

#--------------------------------------------------------------------------------
middle_sele_ask <- function(data){
  newdata <- data %>% mutate(L1_AskSize = if_else(abs(L1_AskPrice - midquote)>height/3&
                                                     abs(L1_AskPrice - midquote)<=height*2/3, L1_AskSize,0),
                             L2_AskSize = if_else(abs(L2_AskPrice - midquote)>height/3&
                                                     abs(L2_AskPrice - midquote)<=height*2/3, L2_AskSize,0),
                             L3_AskSize = if_else(abs(L3_AskPrice - midquote)>height/3&
                                                     abs(L3_AskPrice - midquote)<=height*2/3, L3_AskSize,0),
                             L4_AskSize = if_else(abs(L4_AskPrice - midquote)>height/3&
                                                     abs(L4_AskPrice - midquote)<=height*2/3, L4_AskSize,0),
                             L5_AskSize = if_else(abs(L5_AskPrice - midquote)>height/3&
                                                     abs(L5_AskPrice - midquote)<=height*2/3, L5_AskSize,0),
                             L6_AskSize = if_else(abs(L6_AskPrice - midquote)>height/3&
                                                     abs(L6_AskPrice - midquote)<=height*2/3, L6_AskSize,0),
                             L7_AskSize = if_else(abs(L7_AskPrice - midquote)>height/3&
                                                     abs(L7_AskPrice - midquote)<=height*2/3, L7_AskSize,0),
                             L8_AskSize = if_else(abs(L8_AskPrice - midquote)>height/3&
                                                     abs(L8_AskPrice - midquote)<=height*2/3, L8_AskSize,0),
                             L9_AskSize = if_else(abs(L9_AskPrice - midquote)>height/3&
                                                     abs(L9_AskPrice - midquote)<=height*2/3, L9_AskSize,0),
                             L10_AskSize = if_else(abs(L10_AskPrice - midquote)>height/3&
                                                      abs(L10_AskPrice - midquote)<=height*2/3, L10_AskSize,0))
  return(newdata)
}

#--------------------------------------------------------------------------------
middle_sele_bid <- function(data){
  newdata <- data %>% mutate(L1_BidSize = if_else(abs(L1_BidPrice - midquote)>height/3&
                                                    abs(L1_BidPrice - midquote)<=height*2/3, L1_BidSize,0),
                             L2_BidSize = if_else(abs(L2_BidPrice - midquote)>height/3&
                                                    abs(L2_BidPrice - midquote)<=height*2/3, L2_BidSize,0),
                             L3_BidSize = if_else(abs(L3_BidPrice - midquote)>height/3&
                                                    abs(L3_BidPrice - midquote)<=height*2/3, L3_BidSize,0),
                             L4_BidSize = if_else(abs(L4_BidPrice - midquote)>height/3&
                                                    abs(L4_BidPrice - midquote)<=height*2/3, L4_BidSize,0),
                             L5_BidSize = if_else(abs(L5_BidPrice - midquote)>height/3&
                                                    abs(L5_BidPrice - midquote)<=height*2/3, L5_BidSize,0),
                             L6_BidSize = if_else(abs(L6_BidPrice - midquote)>height/3&
                                                    abs(L6_BidPrice - midquote)<=height*2/3, L6_BidSize,0),
                             L7_BidSize = if_else(abs(L7_BidPrice - midquote)>height/3&
                                                    abs(L7_BidPrice - midquote)<=height*2/3, L7_BidSize,0),
                             L8_BidSize = if_else(abs(L8_BidPrice - midquote)>height/3&
                                                    abs(L8_BidPrice - midquote)<=height*2/3, L8_BidSize,0),
                             L9_BidSize = if_else(abs(L9_BidPrice - midquote)>height/3&
                                                    abs(L9_BidPrice - midquote)<=height*2/3, L9_BidSize,0),
                             L10_BidSize = if_else(abs(L10_BidPrice - midquote)>height/3&
                                                     abs(L10_BidPrice - midquote)<=height*2/3, L10_BidSize,0))
  return(newdata)
}


#--------------------------------------------------------------------------------
middle <- function(data){
  data <- data %>% 
    mutate( middle1 = (L1_BidSize+L2_BidSize+L3_BidSize+L4_BidSize+
                         L5_BidSize+L6_BidSize+L7_BidSize+L8_BidSize+
                         L9_BidSize+L10_BidSize))
  data <- data %>% 
    mutate( middle2 =  (L1_AskSize+L2_AskSize+L3_AskSize+L4_AskSize+
                          L5_AskSize+L6_AskSize+L7_AskSize+L8_AskSize+
                          L9_AskSize+L10_AskSize))
  
  data <- data %>% mutate(middle = (middle1-middle2)/(middle1+middle2) )
  
  data <- data %>% dplyr::filter(!is.nan(middle))
  data <- data %>% mutate(hm=format(Date_Time,'%Y%m%d%H%M'))
  newdata <- data %>% group_by(RIC,hm) %>% summarise(middle = mean(middle))
  return(newdata)
}

#--------------------------------------------------------------------------------
# (L1_BidPrice*L1_BidSize+L2_BidPrice*L2_BidSize+L3_BidPrice*L3_BidSize+L4_BidPrice*L4_BidSize+
#     L5_BidPrice*L5_BidSize+L6_BidPrice*L6_BidSize+L7_BidPrice*L7_BidSize+L8_BidPrice*L8_BidSize+
#     L9_BidPrice*L9_BidSize+L10_BidPrice*L10_BidSize)
# 
# (L1_AskPrice*L1_AskSize+L2_AskPrice*L2_AskSize+L3_AskPrice*L3_AskSize+L4_AskPrice*L4_AskSize+
#     L5_AskPrice*L5_AskSize+L6_AskPrice*L6_AskSize+L7_AskPrice*L7_AskSize+L8_AskPrice*L8_AskSize+
#     L9_AskPrice*L9_AskSize+L10_AskPrice*L10_AskSize)
#--------------------------------------------------------------------------------
outer_sele_ask <- function(data){
  newdata <- data %>% mutate(L1_AskSize = if_else(abs(L1_AskPrice - midquote)>height*2/3, L1_AskSize,0),
                             L2_AskSize = if_else(abs(L2_AskPrice - midquote)>height*2/3, L2_AskSize,0),
                             L3_AskSize = if_else(abs(L3_AskPrice - midquote)>height*2/3, L3_AskSize,0),
                             L4_AskSize = if_else(abs(L4_AskPrice - midquote)>height*2/3, L4_AskSize,0),
                             L5_AskSize = if_else(abs(L5_AskPrice - midquote)>height*2/3, L5_AskSize,0),
                             L6_AskSize = if_else(abs(L6_AskPrice - midquote)>height*2/3, L6_AskSize,0),
                             L7_AskSize = if_else(abs(L7_AskPrice - midquote)>height*2/3, L7_AskSize,0),
                             L8_AskSize = if_else(abs(L8_AskPrice - midquote)>height*2/3, L8_AskSize,0),
                             L9_AskSize = if_else(abs(L9_AskPrice - midquote)>height*2/3, L9_AskSize,0),
                             L10_AskSize = if_else(abs(L10_AskPrice - midquote)>height*2/3, L10_AskSize,0))
  return(newdata)
}
#--------------------------------------------------------------------------------
outer_sele_bid <- function(data){
  newdata <- data %>% mutate(L1_BidSize = if_else(abs(L1_BidPrice - midquote)>height*2/3, L1_BidSize,0),
                             L2_BidSize = if_else(abs(L2_BidPrice - midquote)>height*2/3, L2_BidSize,0),
                             L3_BidSize = if_else(abs(L3_BidPrice - midquote)>height*2/3, L3_BidSize,0),
                             L4_BidSize = if_else(abs(L4_BidPrice - midquote)>height*2/3, L4_BidSize,0),
                             L5_BidSize = if_else(abs(L5_BidPrice - midquote)>height*2/3, L5_BidSize,0),
                             L6_BidSize = if_else(abs(L6_BidPrice - midquote)>height*2/3, L6_BidSize,0),
                             L7_BidSize = if_else(abs(L7_BidPrice - midquote)>height*2/3, L7_BidSize,0),
                             L8_BidSize = if_else(abs(L8_BidPrice - midquote)>height*2/3, L8_BidSize,0),
                             L9_BidSize = if_else(abs(L9_BidPrice - midquote)>height*2/3, L9_BidSize,0),
                             L10_BidSize = if_else(abs(L10_BidPrice - midquote)>height*2/3, L10_BidSize,0))
  return(newdata)
}
#--------------------------------------------------------------------------------
outer <- function(data){
  data <- data %>% 
    mutate( outer1 = (L1_BidSize+L2_BidSize+L3_BidSize+L4_BidSize+
                        L5_BidSize+L6_BidSize+L7_BidSize+L8_BidSize+
                        L9_BidSize+L10_BidSize))
  data <- data %>% 
    mutate( outer2 =  (L1_AskSize+L2_AskSize+L3_AskSize+L4_AskSize+
                         L5_AskSize+L6_AskSize+L7_AskSize+L8_AskSize+
                         L9_AskSize+L10_AskSize))
  
  data <- data %>% mutate(outer = (outer1-outer2)/(outer1+outer2) )
  
  data <- data %>% dplyr::filter(!is.nan(outer))
  data <- data %>% mutate(hm=format(Date_Time,'%Y%m%d%H%M'))
  newdata <- data %>% group_by(RIC,hm) %>% summarise(outer = mean(outer))
  return(newdata)
}
#--------------------------------------------------------------------------------
clean_depth <- function(data){
  data <- data %>% select(-Domain,-Type)
  data[,3:42] <- sapply(select(data,L1.BidPrice:L10.AskSize),as.numeric)
  data <- na.omit(data)
  data <- data %>% dplyr::filter(L1.BidPrice < L1.AskPrice)
  colnames(data) <- c("RIC","Date_Time","L1_BidPrice","L1_BidSize","L1_AskPrice","L1_AskSize",  
                      "L2_BidPrice","L2_BidSize","L2_AskPrice","L2_AskSize","L3_BidPrice","L3_BidSize",  
                      "L3_AskPrice","L3_AskSize","L4_BidPrice","L4_BidSize","L4_AskPrice","L4_AskSize" , 
                      "L5_BidPrice","L5_BidSize","L5_AskPrice","L5_AskSize","L6_BidPrice","L6_BidSize" , 
                      "L6_AskPrice","L6_AskSize","L7_BidPrice","L7_BidSize","L7_AskPrice","L7_AskSize",  
                      "L8_BidPrice","L8_BidSize","L8_AskPrice","L8_AskSize","L9_BidPrice","L9_BidSize", 
                      "L9_AskPrice","L9_AskSize","L10_BidPrice","L10_BidSize","L10_AskPrice","L10_AskSize")
  return(data)

}
#--------------------------------------------------------------------------------
#这是要已经算出midquote了
ret <- function(data){
  data <- data %>% mutate(lag.mq = dplyr::lag(midquote))
  data <- data %>% select(RIC,Date_Time,midquote,lag.mq)
  data <- data %>% mutate(return = log(midquote/lag.mq))
  data <- data %>% mutate(hm=format(Date_Time,'%Y%m%d%H%M'))
  newdata <- data %>% group_by(RIC,hm) %>% summarise(midquote = mean(midquote),
                                                     return = sum(return,na.rm = T))
  return(newdata)
}


#--------------------------------------------------------------------------------

clean_order <- function(data){
  data <- data %>% mutate(FIDName = if_else(is.na(FIDValue)&is.na(FIDName), 'UpdateType.Action', FIDName))
  data <- data %>% mutate(FIDValue = if_else(is.na(FIDValue), UpdateType.Action, FIDValue))
  data <- data %>% select(FIDName,FIDValue,Key.MsgSequenceNumber,NumberofFIDs)
  data[,4] <- as.numeric(data[,4])
  data[,1] <- as.factor(data[,1])
  num <- nrow(na.omit(select(data,NumberofFIDs)))
  data <- data %>% mutate(Num = rep(1:num,na.omit(1+NumberofFIDs)))
  data <- data %>% dplyr::filter(FIDName %in% c('PR_DATE','LV_TIM_NS','UpdateType.Action','ORDER_SIDE','ORDER_SIZE','ORDER_PRC','ORDER_TN'))
  data <- data %>% dplyr::filter(FIDValue != 'UNSPECIFIED' & !is.na(FIDValue))
  times <- plyr::count(data[,5])
  times <- times$freq
  data <- data %>% mutate(MsgNum = rep(na.omit(Key.MsgSequenceNumber),times),
                          Num = rep(1:length(times),times))
  data <- data %>% select(FIDName,FIDValue,MsgNum,Num)
  data <- data %>% tidyr::spread(FIDName,FIDValue)
  data <- data %>% dplyr::filter(ORDER_TN %in% c('1',NA))
  data <- data %>% select(PR_DATE,LV_TIM_NS,UpdateType.Action,MsgNum,ORDER_SIDE,ORDER_PRC,ORDER_SIZE)
  return(data)
}


