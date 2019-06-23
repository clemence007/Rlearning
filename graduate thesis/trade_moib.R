options(scipen = 200)
options(digits.secs=10)
library(DBI)
library(RSQLite)
library(zoo)
library(lubridate)
library(dplyr)
library(xts)
library(highfrequency)

con <- dbConnect(SQLite(),'/Users/xuchaoqun/大四/毕业论文选题/data.db')
#check the list of tables on the database file
as.data.frame(dbListTables(con))
trade_10s_1801 <- dbReadTable(con,'trade_10s_1801')

#trade数据中没有bidprice bidsize askprice asksize 需要填充
taq_10s_1801

trade_10s_1801$Price <- as.numeric(trade_10s_1801$Price)
trade_10s_1801$Price <- as.numeric(trade_10s_1801$Volume)
trade_10s_1801$Bid_Price <- as.numeric(trade_10s_1801$Bid_Price)
trade_10s_1801$Bid_Size <- as.numeric(trade_10s_1801$Bid_Size)
trade_10s_1801$Ask_Price <- as.numeric(trade_10s_1801$Ask_Price)
trade_10s_1801$Ask_Size <- as.numeric(trade_10s_1801$Ask_Size)


#去掉trade中size很大的，阀值=10000股
taq_10s_1801 <- taq_10s_1801 %>% filter(Type == 'Trade'& Volume <10000 | Type == 'Quote') 


