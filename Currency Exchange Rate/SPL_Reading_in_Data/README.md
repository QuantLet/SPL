[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPL_reading_in_data** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

﻿
Name of Quantlet: SPL_reading_in_data

Published in: 'Statistical programming languages - Student Project on "Overview with Analysis of the Effects of the Currency Exchange Rate US Dollar/EUR on the European and U.S. Economy" '

Description: reads in a textfile and prepares the dataframe.

Keywords: Readind Data, Exchange rate, USD, EURO, Europe, US

Author: Anna-Helena Mihov, Hao Cheng, Liv Jantzen

See also: SPL_Descriptive_and_Regression, SPL_Animation_Choreopleth_Map, SPL_Animation_Dollar_and_EUR_signs, SPL_Animation_of_Currency_Exchange_Rate_Map

Submitted:  14.08.2016

Input: 'EconData.txt'

```

### R Code
```r

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
```

automatically created on 2018-05-28