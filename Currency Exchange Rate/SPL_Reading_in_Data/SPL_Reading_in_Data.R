#import all relevant libraries and install packages
library(Rcmdr)
library(sandwich)
library(car)
library(splines)
library(RcmdrMisc)
attach(mtcars)

#set working directory 
#setwd('C:/Users/Helen/Desktop/FinalVersion-SSPL')

# read file with economic data
EconData = read.table("EconData.txt", header=TRUE, 
                      sep = "\t")

#Column names give to variables
colnames(EconData) = c('Date','EXUS','EX', 'EXP',
                       'IMP', 'GDP_EU','GDP_US', 
                       'OIL', 'GOLD', 'SIL')
EconData = na.omit(EconData)

# set date format
EconData$DATE =as.Date(EconData$Date,"%Y-%m-%d")