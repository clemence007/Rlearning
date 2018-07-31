#文字型数据处理
#2.1查询
#2.2替换
#2.3正则表达
#2.4其他字符函数

#2.1插叙
#grep()返回下标数字,
#grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,fixed = FALSE, useBytes = FALSE, invert = FALSE)
#pattern='要查的文字或者正则表达'
#ignore.case=TRUE <- 不区分大小写
#value=TRUE <- 得到不再是下标而是包含的pattern的字符串
#fixed=TRUE <- pattern只能是文本字符串而不能是正则表达
#invert=TRUE <- 返回不匹配的元素下标或者值
#代码如下：
text <- c("Don't","aim","for","success","if","you","want","it","just","do","what","you","love", 
        "and","believe","in","and","it","will","come","naturally") 
grep('[Dd]o',text)
#选择方括号中的任意一个(如[0-2]和[012]完全等价，[Rr]负责匹配字母R和r),等价于grep('Do',text,ignore.case=TRUE)
#上述代码输出：[1]  1 10
grep('[D]o',text) 
#等价于 grep('Do',text)
#grepl('要查的文字或者正则表达',要查的范围)返回对所有取值的判断（TRUE or FALSE），代码如下：
grepl('[Dd]o',text)
#上述代码输出： [1]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#               [19] FALSE FALSE FALSE


# 2.2替换
#sub()对查找到的第一个内容进行替换，并返回修改后的text
#sub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) 
#代码如下：
sub('f','F',text)
# 上述代码输出： [1] "Don't"     "aim"       "For"       "success"   "iF"        "you"       "want"      "it"        "just"     
# [10] "do"        "what"      "you"       "love"      "and"       "believe"   "in"        "and"       "it"       
# [19] "will"      "come"      "naturally"
#其中for被改成For，if被改成iF
#gsub对查找到的所有内容进行替换，返回替换后的text；否则直接返回text
#不过还没看出gsub和sub有啥不一样(0_0|||)


#2.3正则表达
# 2.3.1 [ ]：括号内的任意字符将被匹配
# 2.3.2 \：具有两个作用：
#                       a.对元字符进行转义(后续会有介绍)
#                       b.一些以\开头的特殊序列表达了一些字符串组
#代码如下
strsplit(x="strsplit.aslo.uses.regular.expressions", split="\\.")
strsplit(x="strsplit.aslo.uses.regular.expressions", split=".")
#两个代码分别输出：
# [[1]]
# [1] "strsplit"    "aslo"        "uses"        "regular"     "expressions"
# [[1]]
# [1] "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""
# [37] "" ""
#显然，第一个'\\.'是对的
#2.3.3 ^：匹配字符串的开始.将^置于character class的首位表达的意思是取反义。如[^5]表示匹配除了”5”以外的任何字符
#2.3.4 $：匹配字符串的结束。但将它置于character class内则消除了它的特殊含义。如[akm$]将匹配’a’,’k’,’m’或者’$’.
#代码如下：
test_vector<-c("123","456$","321")
grep('3$',test_vector)
#上述代码输出：[1] 1
grep('[3$]',test_vector)
#上述代码输出：[1] 1 2 3
#2.3.5 .：匹配除换行符以外的任意字符
#2.3.6 |：或者，代码如下
test_vector2<-c("AlphaGo实在厉害！","alphago是啥","阿尔法狗是一条很凶猛的狗。")
grep("AlphaGo|阿尔法狗",test_vector2)
#上述代码输出：[1] 1 3
#2.3.7 ?：匹配零个或一个字符
#2.3.8 *：匹配零个或任意多个字符或字符集合，也可以没有匹配
#2.3.9 +：匹配一个或任意多个字符，至少匹配一次
#2.3.10 ():提取匹配的字符串，(\\s*)表示连续空格的字符串  <- 还没有搞懂(0 ^ 0)
#R中预定的字符串：
# [:digit:]	数字：0-9
# [:lower:]	小写字母：a-z
# [:upper:]	大写字母：A-Z
# [:alpha:]	字母：a-z及A-Z
# [:alnum:]	所有字母及数字
# [:punct:]	标点符号，如. , ;等
# [:graph:]	Graphical characters,即[:alnum:]和[:punct:]
# [:blank:]	空字符，即：Space和Tab
# [:space:]	Space，Tab，newline，及其他space characters
# [:print:]	可打印的字符，即：[:alnum:]，[:punct:]和[:space:]
#需要用到\转义：
# \w	字符串，等价于[:alnum:]
# \W	非字符串，等价于[^[:alnum:]]
# \s	空格字符，等价于[:blank:]
# \S	非空格字符，等价于[^[:blank:]]
# \d	数字，等价于[:digit:]
# \D	非数字，等价于[^[:digit:]]
# \b	Word edge（单词开头或结束的位置）
# \B	No Word edge（非单词开头或结束的位置）
# \<	Word beginning（单词开头的位置）
# \>	Word end（单词结束的位置）


#其他字符函数，nachar(x)统计变量x重的字符数量，注意跟length()的区别，代码如下
ncahr(c('abc','aex','xa','xy','abc'))
#上述代码输出：[1] 3 3 2 2 3
#strsplit(x,split)在split处分割字符向量x中的元素
#paste (..., ..., sep = " ", collapse = NULL)可以在循环的时候用，批量取名啥的
#toupper(x)大写转换；tolower(x)小写转换
