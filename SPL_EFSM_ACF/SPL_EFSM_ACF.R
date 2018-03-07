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
setwd("~/EFSM/SPL_EFSM_ACF")

#load all_data table for autocorrealtion test
all_data = readRDS("~/EFSM/SPL_EFSM_ACF/big_table.RDS")


#set list of variables to calculate autocorrelation coefficients
variables = c("YP", "MP", "DEI", "UI", "URP", "UTS", "CG", "OG", "EWNY")

#subset data set from all_data
data = all_data[ ,variables]

#set parameters
lag_max    = 24            # maximum order of autocorrelation coefficients
ac_table   = as.data.frame(matrix(data= NA, ncol = lag_max, nrow = length(variables)))
n          = ncol(data)    # number of variables

#create autocorrelation table and save plots
for(i in 1:n){
      ac = acf(data[ ,i], lag.max = lag_max, na.action = na.pass, plot = FALSE)
      path = paste("~/EFSM/SPL_EFSM_ACF/SPL_EFSM_ACF", i, ".pdf", sep="")
      pdf(file = path)
      plot(ac, main = colnames(data)[i])
      dev.off()
      ac_table[i, ] = ac$acf[-1]
}

rownames(ac_table) = variables             # set rownames for output table
ac_table = round(ac_table, 4)              # round result within 4 digits
saveRDS(ac_table, file = "ac_table24.RDS") # save table