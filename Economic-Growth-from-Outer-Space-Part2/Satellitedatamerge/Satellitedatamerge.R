# library packages
if (!require ("pacman")) {
  install.packages("pacman")
} 
pacman::p_load(readxl, reshape, ggplot2, tidyr, dplyr, stringr,
               plyr)

# get average of SUM for 2 years data, first set path to read rfile 
setwd('F:/google/language programming/language')
source('rfile/lights_function.R') 

# path for data with 2 satellites in same year needed 
setwd('F:/google/language programming/language/data/gis/2year') 
data.avg = SUM.average(c(1994, 1997:2007))

# get satellite data
# path for data with only one satellite in same year needed
setwd('F:/google/language programming/language/data/gis') 
data = getdata(c(1992:1993, 1995:1996, 2008:2013))
# rbind data and data.avg
data = rbind(data, data.avg)

# merge data with country code, GDP and Ranking infos
# path for data GDP, Raning and Country Code needed
cntry.code = read_excel("cntry code.xlsx")
cntry.code = separate(cntry.code, "AB Cntry",  # seperate data by space  
                      c("Code", "SOVEREIGNT"), " ", extra = "merge")
realGDP    = read.csv("GDP_LCU.csv")
GDP.growth = read.csv("GDP_growth.csv") 

#merge data with country code
data = merge(data, cntry.code, 
                by = c(names(cntry.code)[2]), 
             all.x =TRUE)

#merge GDP.growth
GDP = melt.GDP(GDP.growth)
names(GDP) = c('Code',"year","GDP.growth")

#merge data GDP 1992-2014
data = merge(data, GDP, 
                by = c('Code', 'year'), 
             all.x = TRUE)

#merge GDP_LCU
GDP.real = melt.GDP(realGDP)
names(GDP.real) = c('Code', "year", "realGDP")
data = merge(data, GDP.real, 
                by = c('Code', 'year'), 
             all.x = TRUE)

#merge SCI data quality index
SCI = read.csv("SCI.csv")
SCI = melt.GDP(SCI)
names(SCI) = c('Code', 'year', 'ranking')
SCI.2004   = SCI[SCI$year == 2004, ]
data = merge(data, 
             SCI.2004[ ,c('Code', 'ranking')], 
                by = c('Code'), 
             all.x = TRUE)

#merge class of income: high low middle
CLASS = data.frame(read_excel("CLASS.xls"))
####merge country group ####
CLASS = CLASS[ , c(3,5)]
CLASS = CLASS[is.na(CLASS$Income.group)==F, ]  # delete NA values
names(CLASS)[2] = 'INC.GROUP'
data = merge(data, CLASS, 
                by = c("Code"),
             all.x = T)

# regression preparation
data$lightarea = data$SUM/data$AREA

#log_difference for GDP
long        = log.dif('realGDP')
data        = dif.merge('logdif.GDP')

#log_difference for lights
long        = log.dif('lightarea')
data        = dif.merge('log.lightarea')

setwd('F:/google/language programming/language/data')
write.csv(data, "SatellitedatamergeOutput1.csv", row.names = F)

#long_difference 0506-9293, 1213-9293,0506-1213
M         = data.frame(c(rep('realGDP', 3), rep('lightarea', 3)), 
                       rbind(1992, 1992, 2005, 1992, 1992, 2005), 
                       rbind(13, 20, 7, 13, 20, 7)) 
longdif   = data.frame('Code' = unique(data$Code))
mergelongdif = function(x) {
  a = long.dif( var = as.character(M[x, 1]), 
               year = M[x, 2], 
                  i = M[x, 3])
}
longdif = cbind(longdif, sapply(1:6, mergelongdif))
names(longdif) = c("Code", "realGDP9205", "realGDP9213", "realGDP0513",
                   "lightarea9205", "lightarea9213", "lightarea0513")

# merge country group 
longdif = merge(longdif, CLASS, 
             by = c("Code"),
             all.x = T)
write.csv(longdif,'SatellitedatamergeOutput2.csv',row.names = F)

##country remove
NAcode = which(rowSums(is.na(longdif)) > 0)  # 34 Country with NA
longdif$Code[which(rowSums(sapply(longdif[NAcode, ], is.na)) > 0)]  # AGO, ALB, AND


