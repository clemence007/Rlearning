library(plyr)
library(ggplot2)
library(MASS)
#3.1基础
#3.2aaply
#3.3m*ply
#3.4辅助函数
#3.5例子baseball
#3.6例子ozone

#3.1
#atomic : character,logical,integer,numeric
#non-atomic : list-array
#dimnames(array), rownames(matrics/data frame), colnames(matrics/data frame), names(atomic)
#a代表array,d代表data frame,l代表list,m代表multiple inputs

#3.2aaply(.data,.margins,.fun=NULL)
#现将x（按行，按列）分割，再将分割后的排成行，再对每一行操作。
x <- array(1:24,2:4)
shape <- function(x) {if(is.vector(x))length(x)else dim(x)}
shape(x)
shape(aaply(x, 2, function(y) 0))
aaply(x,2,function(y)0)
shape(aaply(x, 2, function(y) rep(1:5)))
aaply(x, 2, function(y) rep(1:5))
shape(aaply(x, 2, function(y) matrix(0,nrow = 5,ncol = 6)))
aaply(x, 2, function(y) matrix(0,nrow = 5,ncol = 6))
shape(aaply(x,1,function(y) matrix(0,nrow = 5,ncol = 6)))
aaply(x,1,function(y) matrix(0,nrow = 5,ncol = 6))
aaply(x,3,function(y)matrix(1,nrow = 5,ncol = 6))
shape(aaply(x,3,function(y)matrix(1,nrow = 5,ncol = 6)))

#3.3m*ply(.data,.fun, ...)
#可以输入list-array,data frame,matrix。对每一行进行function操作，每行的的一个元素代表function中的一个参数
#例如生成若干随机数：
d <- data.frame(
  n=c(5,5,5),
  mean=c(1,20,300),
  sd=c(5,5,5)
)
mdply(d,rnorm)


#3.4辅助函数
#each(fun1,fun2,fun3...) ——> make a list of function
#splat()不懂
#
#colwise(.fun,.col=true, ...) 
#optional .if argument,colwise(.fun,.if=is.numeric)等价于numcolwise(.fun);colwise(.fun,.if=is.factor)等价于catcolwise(.fun)
#failwith(default,f) ——>当f=function得到error时，返回一个default值


#3.5例子baseball
data('baseball')
#数据集记录了运动员在连续好多年中每一年的状况
#第一步，先计算出每位运动员的职业生涯的长度。
#transform()是在一个是在一个data frame中直接对某些变量操作并生成新的一列。
#subset()用来筛选变量和观测
#reorder来分来id，根据rbi/ab平均值
baberuth <- subset(baseball,id=='ruthba01')
baberuth <- transform(baberuth,cyear=year-min(year)+1)
qplot(cyear,rbi/ab,data = baberuth,geom = 'line',xlim = range(baberuth$cyear,na.rm = TRUE),ylim = range(baberuth$rbi/baberuth$ab,na.rm = TRUE))
baseball <- ddply(baseball,.(id),transform,
                  cyear=year-min(year)+1) #新增一列数据cyear
baseball <- subset(baseball,ab>=25) #选取ab大于等于25的，缩小范围
xlim <- range(baseball$cyear, na.rm = TRUE) #na.rm=TRUE表示除去NA
ylim <- range(baseball$rbi/baseball$ab, na.rm = TRUE)
plotpattern <- function(df){
  qplot(cyear, rbi/ab,data=df, geom = 'line', xlim = xlim, ylim = ylim)
}
setwd('/Users/xuchaoqun/statistics/RLanguage/plyr_baseball')
pdf('paths.pdf',width = 8, height = 4)
d_ply(baseball,.(reorder(id,rbi/ab)),failwith(NA,plotpattern),.print = TRUE)
dev.off()
model1 <- function(df){
  lm(rbi/ab ~ cyear,data = df)
}
model1(baberuth)
coef(model1(baberuth))
bmodel1 <- dlply(baseball,.(id),model1) #把1145个lm model输出list形式
rsq <- function(y) summary(y)$r.squared
bcoefs <- ldply(bmodel1,function(y)c(coef(y),rsquare=rsq(y)))
names(bcoefs)[2:3] <- c('Intercept','slope')
baseballbcoefs <- merge(baseball,bcoefs,by='id')
subset(baseballbcoefs,rsquare>0.999)$id


#3.6例子ozone
value <- ozone[1,1,]
time <- 1:72/12
month.abbr <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
month <- factor(rep(month.abbr,length=72),levels = month.abbr)
year <- rep(1:6,each=12)
deseas1 <- rlm(value~month-1) # 这个‘-1’是什么意思？
summary(deseas1)
deseasf <- function(value) rlm(value~month-1,maxit=50)
models <- alply(ozone,1:2,deseasf)
failed <- laply(models,function(x) !x$converged)
coefs <- laply(models,coef)
dimnames(coefs)[[3]] <- month.abbr #dimnames(coefs)是一个list, [[3]]提取第3个子集中的元素，[3]是提取整个第3个子集
names(dimnames(coefs))[3] <- 'month'
deseas <- laply(models,resid)
dimnames(deseas)[[3]] <- 1:72
names(dimnames(deseas))[3] <- 'time'
coefs_df <- melt(coefs)
coefs_df <- ddply(coefs_df,.(lat,long),transform,
                  mean=mean(value),
                  std=value/max(value))
coef_limit <- range(coefs_df$value)
coef_mid <- mean(coefs_df$value)
monthsurface <- function(mon){
  df <- subset(coefs_df,month==mon)
  qplot(long,lat,data = df,geom = 'tile',fill=value,main = mon)+
    scale_fill_gradient(limits=coef_limit,low = 'blue' ,high = 'yellow')
}
pdf('ozone-animation.pdf',width = 8,height = 8)
l_ply(month.abbr,monthsurface,.print = TRUE)
dev.off()
