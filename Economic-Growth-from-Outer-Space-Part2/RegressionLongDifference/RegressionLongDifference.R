if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(reshape, ggplot2, readxl, tidyr,  stargazer,
               dplyr,   stringr, MASS,   lmtest, sandwich)
setwd('F:/google/language programming/language/data') 

# long difference
wide = read.csv('longdif.csv')
wide = wide[wide$Code != 'GNQ' & 
            wide$Code != 'ATA' & 
            wide$Code != 'TUV', ]
wide = wide[complete.cases(wide) & 
              ! is.infinite(wide$lightarea9205), ]
#####################################################
fmla.long = realGDP9205 ~ lightarea9205
model = list()
model[['longdif.9205.full']] = lm(fmla.long, data = wide)
model[['longdif.9205.re']] = coeftest(model[["longdif.9205.full"]], 
                                     vcov = vcovHC(model[["longdif.9205.full"]],
                                                   "HC1"))
summary(model[['longdif.9205.full']])

# explore model 
model[['longdif.9213']] = lm(realGDP9213 ~ lightarea9213, data = wide)
model[['longdif.9213re']] = coeftest(model[["longdif.9213"]], 
                                      vcov = vcovHC(model[["longdif.9213"]],
                                                    "HC1"))
model[['longdif.0513']] = lm(realGDP0513 ~ lightarea0513, data = wide)
model[['long.0513re']] = coeftest(model[["longdif.0513"]], 
                                      vcov = vcovHC(model[["longdif.0513"]],
                                                    "HC1"))
##################################################################
#table code with stargazer
stargazer(model[['longdif.9205.full']], 
          model[['longdif.0513']],
          model[['longdif.9213']],
          title   = 'Results for model test with latest data',
          summary = FALSE)

