# clear variables and close windows

rm(list = ls(all = TRUE))
graphics.off()

#import all relevant libraries and install packages

libraries = c("stats", "tidyverse", "LSTS")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#set working directory 


#load all_data table for autocorrealtion test

all_data = readRDS("~/EFSM/SPL_EFSM_Box/big_table.RDS")


#set list of variables to calculate autocorrelation coefficients

variables = c("YP", "MP", "DEI", "UI", "URP", "UTS", "CG", "OG", "EWNY")

#subset data set from all_data

data = all_data[ ,variables]

#set parameters

lag_max    = 24            # maximum order of autocorrelation coefficients
ac_table   = as.data.frame(matrix(data= NA, ncol = lag_max, nrow = length(variables)))
n          = ncol(data)    # number of variables

#Ljung-Box (Q*(24)) statistics 

Box.test = as.data.frame(rep(NA, 9))
for(i in 1:n){
  test = Box.test(data[ ,i], lag = lag_max, type = c("Ljung-Box"))
  Box.test[i,] = test$statistic
}
Box.test           = round(Box.test, 2)  # round test result within 2 digits
rownames(Box.test) = variables           # set rownames

#plot Ljung-Box (Q*(24)) statistics 

for(i in 1:n){
  path = paste("~/EFSM/SPL_EFSM_Box/SPL_EFSM_Box", i, ".pdf", sep="")
  pdf(file = path)
  Box.Ljung.Test(data[ ,i], lag = lag_max, main = paste("Ljung_Box test", colnames(data)[i]))
  dev.off()
}

#merge table

auto_table   = merge.data.frame(ac_table, Box.test, by = 0, sort = FALSE)
auto_table   = auto_table %>% 
  remove_rownames %>% 
  column_to_rownames(var="Row.names")
colnames(auto_table)[25] = c("Box.test")
auto_table12 = auto_table[ ,c(1:12, 25)]

#save output

saveRDS(auto_table, file = "auto_table24.RDS")
write.csv(auto_table, file = "auto_table24.CSV")
saveRDS(auto_table12, file = "auto_table12.RDS")
write.csv(auto_table12, file = "auto_table12.CSV")