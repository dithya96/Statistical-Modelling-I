###########################################################################
## This code is to explore student data using different visualizations (EDA)
## Dithya Sridharan 
## Person no : 50286923
## Created : 12 September, 2018
###########################################################################

#install some libraries
install.packages("DAAG")
install.packages("lattice")
install.packages("MASS")
install.packages("devtools")
install.packages("gridExtra")



#load library
library("gridExtra")
library(ggplot2)
library(DAAG)
library(lattice)
library(MASS)
library(devtools)
library(easyGgplot2)
library(tidyverse)
library(dplyr)


###########################################################################
## Reading and loading Data into R
###########################################################################

df1=read.table("E:\\student-mat.csv",sep=";",header=TRUE)
df2=read.table("E:\\student-por.csv",sep=";",header=TRUE)



#######################################################################################################
## Performance analysis criteria
#######################################################################################################

df1 <- mutate(df1, Pass.Fail = ifelse(G1 > 10, "Pass", "Fail"))
df2 <- mutate(df2, Pass.Fail = ifelse(G1 > 10, "Pass", "Fail"))
head(df1)
head(df2)


###########################################################################
## EDA analysis by bar plots
###########################################################################

attach(df1)
b1 <- ggplot(data=df1, aes(x=Pass.Fail, fill=Pass.Fail)) +
  geom_bar(stat="count")
attach(df2)
b2 <- ggplot(data=df2, aes(x=Pass.Fail, fill=Pass.Fail)) +
  geom_bar(stat="count")
grid.arrange(b1,b2,nrow=1)

attach(df1)
b3 <- ggplot(data=df1, aes(x=sex, fill=Pass.Fail)) +
  geom_bar(stat="count")
attach(df2)
b4 <- ggplot(data=df2, aes(x=sex, fill=Pass.Fail)) +
  geom_bar(stat="count")
grid.arrange(b3,b4,nrow=1)

###########################################################################
## EDA analysis with box plots and jitter plots
###########################################################################


c1 <- ggplot(data = df1, mapping = aes(x = Pass.Fail, y = age)) + geom_boxplot()
c2 <- ggplot(data = df2, mapping = aes(x = Pass.Fail, y = age)) + geom_boxplot()
grid.arrange(c1,c2,nrow=1)


##Performing some data transformation to analyse different variables wrt Pass.Fail

df1$sex <- as.factor(df1$sex)
df2$sex <- as.factor(df2$sex)
df1$address <- as.factor(df1$address)
df2$address <- as.factor(df2$address)
x1 <- ggplot(data = df1, mapping = aes(x = Pass.Fail, y = higher)) + geom_jitter(aes(colour = Pass.Fail))
x2 <- ggplot(data = df1, mapping = aes(x = Pass.Fail, y = address)) + geom_jitter(aes(colour = Pass.Fail))
x3 <- ggplot(data = df1, mapping = aes(x = Pass.Fail, y = internet)) + geom_jitter(aes(colour = Pass.Fail))

x4 <- ggplot(data = df2, mapping = aes(x = Pass.Fail, y = higher)) + geom_jitter(aes(colour = Pass.Fail))
x5 <- ggplot(data = df2, mapping = aes(x = Pass.Fail, y = address)) + geom_jitter(aes(colour = Pass.Fail))
x6 <- ggplot(data = df2, mapping = aes(x = Pass.Fail, y = internet)) + geom_jitter(aes(colour = Pass.Fail))

grid.arrange(x1,x2,x3,x4,x5,x6,nrow=2,ncol=3)


###################################################################################
## Histogram analysis
##################################################################################

par(mfrow = c(2,2))
p1 <- ggplot(df1, aes(x=health, color=Pass.Fail)) +
  geom_histogram(fill="white",binwidth = 0.5)
p2 <- ggplot(df1, aes(x=absences, color=Pass.Fail)) +
  geom_histogram(fill="white",binwidth = 0.5)
grid.arrange(p1,p2,nrow=1)

p3 <- ggplot(df2, aes(x=health, color=Pass.Fail)) +
  geom_histogram(fill="white",binwidth = 0.5)
p4 <- ggplot(df2, aes(x=absences, color=Pass.Fail)) +
  geom_histogram(fill="white",binwidth = 0.5)
grid.arrange(p3,p4,nrow=1)

####################################################################################
## Scatter plot analysis
####################################################################################



y1 <- ggplot(df1, aes(x=Pass.Fail, y=Walc)) + geom_point()
y2 <- ggplot(df1, aes(x=Pass.Fail, y=failures)) + geom_point()
grid.arrange(y1,y2,nrow=1)

y3 <- ggplot(df2, aes(x=Pass.Fail, y=Walc)) + geom_point()
y4 <- ggplot(df2, aes(x=Pass.Fail, y=failures)) + geom_point()
grid.arrange(y3,y4,nrow=1)



####################################################################################
## Outlier elimination from box plot analysis done on age, absences and health
###################################################################################

age[!age %in% boxplot.stats(age)$out]
absences[!absences %in% boxplot.stats(absences)$out]
health[!health %in% boxplot.stats(health)$out]


####################################################################################
## Data Transformation
####################################################################################

##Adding a column of data to see if student is a first generation student
df1$firstgen=ifelse(df1$Fedu>4&&df1$Medu>4,'1','0')

##Modifying age to be a range of values
d4=df1
brks=c(15,16,17,18,19,21)
d4$age=cut(d4$age,breaks=brks,include.lowest=TRUE)

##Modifying the type of data 
df2$reason=gsub('C',"course",df2$reason)
df2$reason=gsub('R',"reputation",df2$reason)



##################################################################################################
## Variable elimination in data frame
#################################################################################################


vareli1 <- names(df1) %in% c("romantic", "guardian","firstgen") 
newdata1 <- df1[!vareli1]
vareli2 <- names(df2) %in% c("romantic", "guardian","firstgen") 
newdata2 <- df2[!vareli2]




##################################################################################################
## Question 2
##################################################################################################


#################################################################################################
## Understanding the data
#################################################################################################

head(newdata1)
head(newdata2)
summary(newdata1)
summary(newdata2)


################################################################################################
## Checking correlation b/w G1 and final grade G3
################################################################################################

cor(newdata1[sapply(newdata1, is.numeric)])
cor(newdata2[sapply(newdata2, is.numeric)])
reg113 <- lm(G3 ~ G1, data = newdata1)
reg213 <- lm(G3 ~ G1, data = newdata2)
summary(reg113)
summary(reg213)

plot(x = newdata1$G1,
     y = newdata1$G3,
     pch = 16,
     col = gray(.1, .1),
     xlab = "Period 1",
     ylab = "Period 3",
     main = "Student Math Data"
)

abline(reg113, lty = 2)

################################################################################################
## Applying standard regression to data wrt to significant variables analysed in Q1
################################################################################################

myvar <- c("internet","Walc","higher","absences","health")
nd <- newdata1[myvar]
head(nd)
plot(nd)

nd1 <- newdata2[myvar]
head(nd1)
plot(nd1)

results = lm(G1 ~ internet + Walc + absences + failures + higher, data=newdata1)
summary(results)



#################################################################################################
## Finding regression and correlation wrt interactions
################################################################################################

modx <- lm(G1 ~ sex*internet, data=newdata1)
mody <- lm(G1 ~ age:sex, data=newdata2)
summary(modx)
summary(mody)



