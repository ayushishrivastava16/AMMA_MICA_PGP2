##Business problem- Data understanding, breaking down the problem, data analysis, Issue tree


install.packages("Rcmdr")

library("Rcmdr")
install.packages('rJava')

setwd('C:/Users/RAJNEESH VISHNU/Desktop/MICA2017/Data')
setwd('F:/MICA/Study/Term 4/AMMA/AMMA 2017/Data/resession2')

#R imports and stores the data in a data frame called "dt"
dt<-read.csv("train.csv")
#structure  of the data file imported
str(dt)
View(dt)


install.packages('shiny')
library('shiny')

runExample("02_text")
