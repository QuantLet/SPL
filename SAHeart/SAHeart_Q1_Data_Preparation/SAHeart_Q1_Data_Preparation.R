
## Check if required packages are installed, if not install and load
loadPKG <- function(pkg){
  if(!(pkg %in% installed.packages())){
    message(paste0("The required package \'",
                   pkg, "\'is currently not installed. ",
                   "It will now be installed to the default library path."))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

loadPKG("dplyr")
loadPKG("ggplot2")          
loadPKG("gridExtra")        
loadPKG("ElemStatLearn")    
loadPKG("corrplot")
loadPKG("xtable")

data(SAheart)


## Specify Path to save graphic outputs
PDFpath <- "~/Desktop"


## check if PDFpath exists, if not choose current working directory
if(!dir.exists(PDFpath)){
  PDFpath <- getwd()
  message(paste0("The \'PDFpath\' that was specified does not exist. ",
                 "Instead, the graphic outputs will be saved to the current ",
                 "working directory. The current working directory is: \'",
                 getwd(),"\'"))
}


################################################################################

  
## Variables of dataset
# sbp         systolic blood pressure
# 
# tobacco     cumulative tobacco (kg)
# 
# ldl         low density lipoprotein cholesterol
# 
# adiposity   a numeric vector
# 
# famhist     family history of heart disease, a factor with levels Absent Present
# 
# typea       type-A behavior
# 
# obesity     a numeric vector
# 
# alcohol     current alcohol consumption
# 
# age         age at onset
# 
# chd         response, coronary heart disease


##################################
##                              ##
##    Description of dataset    ##
##                              ##
##################################


## Some overview stuff
  
glimpse(SAheart)    
str(SAheart)

head(SAheart, 5)

summary(SAheart)


## Create id variable to easier handle single observations (using 'dplyr')
SAheart <- SAheart %>%
  mutate(id = row_number())
    

## famhist
head(SAheart$famhist, 10)                           
head(as.numeric(SAheart$famhist), 10)               
SAheart$famhist <- as.numeric(SAheart$famhist)     
SAheart$famhist[SAheart$famhist == 1] <- 0   
SAheart$famhist[SAheart$famhist == 2] <- 1
