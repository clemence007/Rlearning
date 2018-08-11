#getting your data into shape

#str() --> compactly display the structure of an arbitrary R object
data('mtcars')
str(mtcars)

#add a new column : dataframe$newcol <- single_value_or_vector
mtcars$newcol <- mtcars$cyl/mtcars$wt
mtcars$newcol2 <- 1

#delete a column : assign NULL to that column
mtcars[c('newcol','newcol2')] <- NULL

#select data 
data <- subset(mtcars,select = c(hp,am))
data2 <- subset(mtcars,select = c(-hp,-am))

#rename column in data frame
names(mtcars) <- c('Mpg','Cyl','Disp','Hp','Drat','Wt','Qsec','Vs','Am','Gear','Carb')
names(mtcars)[names(mtcars)=='Drat'] <- 'drat'
names(mtcars)[1] <- 'mpg'

#reorder column
data <- mtcars[c(1,3,2)]
data <- data[c('Disp','Cyl','mpg')] #list-style indexing
data <- data[,c('Disp','Cyl','mpg')] #matrix-style indexing
data[3] #list-style indexing
data[,3] #matrix-style indexing 
#but there is a liitle difference between list-style and matrix-style

#subset(x,subset,select) subset is logical expression indicating elements or rows to keep
data("Orange")
str(Orange)
subset(Orange,Tree==1,select = c(age,circumference))
subset(Orange,Tree==1 & age < 1300 & circumference > 60, select = c(age,circumference))
Orange[Orange$Tree==1&Orange$age<1300&Orange$circumference>60,c('age','circumference')]
Orange[Orange$Tree==1&Orange$age<1300&Orange$circumference>60,c('circumference')] # if you grab just a single column,it will return a vector rather than a data frame
Orange[Orange$Tree==1&Orange$age<1300&Orange$circumference>60,c('circumference'),drop=FALSE] # you can use drop=FALSE to return a single variable data frame

#rev(x) provides a reversed version of its argument,x is a vector or other object for which reversal is defined
size <- c('large','medium','small')
rev(size)
#but pay attention, transposing matrix argument is t(x)
mymatrix <- matrix(1:9,nrow = 3)
t(mymatrix)

#change name of factor levels or character vector <-- 5 methods
sizes <- factor(c( "small", "large", "large", "small", "medium"),levels = c('large','medium','small'))
library(plyr) #for revalue() and mapvalue() arguments
sizes1 <- revalue(sizes,c('small'='S','medium'='M','large'='L')) #method 1
sizes2 <- mapvalues(sizes,c('large','medium','small'),c('L','M','S')) #method 2 
levels(sizes)[levels(sizes)=='small'] <- 'S' #method 3
levels(sizes)[levels(sizes)=='medium'] <- 'M'
levels(sizes)[levels(sizes)=='large'] <- 'L'
levels(sizes) <- list(Large='L',Medium='M',Small='S') #method 4
levels(sizes)[1] <- 'L' #method5

#delete unuseful levels <-- droplevels(x)
sizes3 <- factor(c("small", "large", "large"),levels = c('large','medium','small'))
sizes3 <- droplevels(sizes3)

#continuous variable to categorical variable
mtcars$mpgclass <- cut(mtcars$mpg,breaks = c(10,20,30,Inf),labels = c('small','medium','large'))

#transform variable <-- transform() or matate() in plyr
Orange <- transform(Orange,ageMon=age/30,circumferenceCm=circumference/10)
Orange <- mutate(Orange,newcol = circumference/age)

#convert data from wide to long <-- melt(data,id.vars=..., variable.name=..., value.name=...) in reshape2
#it's very useful for plotting
ag <- data.frame(angle=c(-20,-10,0,10,20),expt=c(1,7,2,0,0),ctrl=c(0,3,3,3,1))
library(reshape2)
ag <- melt(ag,id.vars = 'angle', variable.name = 'condition',value.name = 'count')
