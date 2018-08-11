library(ggplot2)

#scatter plot
#mtcars: mpg-miles/gallon; cyl-气缸数; disp-位移; hp-总马力; drat-后轴比; wt-weight; qsec-1/4 mile time; vs-V/S; am-0自动挡1手动挡; gear-前进档数; carb-化油器数
data("mtcars")
qplot(mtcars$wt,mtcars$mpg) # or...
qplot(wt,mpg,data=mtcars) # or...
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()

#linear plot
data("pressure")
#using plot()
plot(pressure$temperature,pressure$pressure,type = 'l')
points(pressure$temperature,pressure$pressure) #add points
lines(pressure$temperature,pressure$pressure/2,col='red') #add additional lines with lines()
points(pressure$temperature,pressure$pressure/2,col='red')
#using ggplot2
qplot(pressure$temperature,pressure$pressure,geom = 'line') # or...
qplot(temperature,pressure,data = pressure,geom = 'line') # or...
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()
qplot(temperature,pressure,data = pressure,geom = c('line','point'))

#bar plot
#using barplot()
barplot(table(mtcars$cyl))
#using ggplot2, pay attention to the difference between bar graph and histogram like following codes:
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
data('BOD') #biochemical oxygen demand
ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat = 'identity')
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat = 'identity')

#histogram plot
# using hist()
hist(mtcars$mpg,breaks = 10)
#using ggplot2
qplot(mtcars$mpg)
qplot(mpg,data = mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 4)

#box plot
data("ToothGrowth") #The Effect of Vitamin C on Tooth Growth in Guinea Pigs,supp-supplement type(VC or OJ); dose计量
plot(ToothGrowth$supp,ToothGrowth$len) #when x is a factor, plot() will automatically create a box plot
boxplot(len~supp,data = ToothGrowth)
boxplot(len~supp+dose,data = ToothGrowth)
#using ggplot2
qplot(ToothGrowth$supp,ToothGrowth$len,geom = 'boxplot') or
qplot(supp,len,data = ToothGrowth,geom = 'boxplot')
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
#for multi variable with interaction()
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()

#function curve plot
#using curve()
myfun <- function(x){
  1/(1+exp(-x+10))
}
curve(myfun(x),from=0,to=20)
curve(1-myfun(x),add=TRUE,col='blue')
#using ggplot2
ggplot(data.frame(x=c(0,20)),aes(x=x))+stat_function(fun = myfun,geom = 'line')
