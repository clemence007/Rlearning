#table
  
# 1.1建立列联表
# 1.2保存列联表
# 1.3用卡方检验或者Fisher精度检验2*2列联表
  
#1.1建立2*2列联表
data("mtcars")
table_2d <- table(mtcars$gear,mtcars$carb)
table_2dx <- xtabs(~gear+carb,data = mtcars)
#xtabs比table多了变量名，而且xtabs可以做高维列联表
#接下来用xtabs（）建立高维列联表/三维列联表
table_3dx <- xtabs(~am+gear+carb,data = mtcars)
#xtabs()得到了不紧凑的高维列联表：多个二维列联表
#用ftable()可以得到更紧凑的扁平列联表，ftable(x, exclude = c(NA, NaN), row.vars = NULL,col.vars = NULL)
#这里的x是data.frame格式或者table格式，所以高维列联表一般x<-xtabs()
#am,gear,carb变量被赋值为1，2，3，row.var就是选第几个第几个变量为row变量，当然也可以用名称,如下
table_3df <- ftable(xtabs(~am+gear+carb,data = mtcars))
table_3df <- ftable(xtabs(~am+gear+carb,data = mtcars),row.vars = c(1,2))
table_3df <- ftable(xtabs(~am+gear+carb,data = mtcars),row.vars = c('am','carb'))

#1.2保存表格
# 对于列联表，可以直接用write.table()来保存
# write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
#             eol = "\n", na = "NA", dec = ".", row.names = TRUE,
#             col.names = TRUE, qmethod = c("escape", "double"),
#             fileEncoding = "")
# append只有在file是character string字符串才有用，FALSE的话所有子存在文件会被重写。
# sep表示分隔，一般保存为csv文件时都用','
# row.names可以是逻辑判断也可以是字符串向量给各行直接命名，当无法自动给名字时用TRUE
# col.names同上
setwd('/Users/xuchaoqun/statistics')
write.table(table_2d,'table_2d.csv',col.names = TRUE, row.names = TRUE , sep = ',')
#但是这样做还是有形式问题，比如列名称会往前移一格
#把col.names设置为NA，表示在列名前空一格，代码如下
write.table(table_2d,'table_2d.csv',col.names = NA, row.names = TRUE , sep = ',')
#在这里不能用stats:::format.ftable()因为这是用于ftable的，而ftable是扁平高维列联表，例如三维的。
#对table_3df就可以用了，代码如下
r <- stats:::format.table(table_3df, quote=F)
write.table(r,'table_3df.csv',col.names = FALSE, row.names = FALSE , sep = ',')

#1.3检验2*2列联表
# 一般我们会优先使用卡方检验来检验两个变量之间是否相关，chisq.tast(table)
# 但是，当自由度>1,最小理论频数T>=5,总频数N>40时，用Pearson卡方检验；
#       当自由度>1，1<T<5，n>=40时，用校正卡方检验；
#       当T<1或N<40时，用Fisher精度检验。
#发现table_2dx有部分频数等于0且N<40，所以用Fisher精度检验，代码如下
fisher.test(table_2dx)
#可以只输出Fisher精度检验的P-value
ftest <- fisher.test(table_2dx)
ftest$p.value
# fisher.test(x, y = NULL, workspace = 200000, hybrid = FALSE,
#             control = list(), or = 1, alternative = "two.sided",
#             conf.int = TRUE, conf.level = 0.95,
#             simulate.p.value = FALSE, B = 2000)
#对于高维列联表，可以‘条件独立性检验’就是按照某一属性进行分层研究。
