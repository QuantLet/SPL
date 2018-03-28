
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



## Create id variable to easier handle single observations (using 'dplyr')
SAheart <- SAheart %>%
  mutate(id = row_number())
SAheart$famhist <- as.numeric(SAheart$famhist)     
SAheart$famhist[SAheart$famhist == 1] <- 0   
SAheart$famhist[SAheart$famhist == 2] <- 1


  
##################################
##                              ##
##      Logistic Regression     ##
##                              ##
##################################       


## M0: (naive) Full model
m0 <- glm(chd ~ ., family = binomial(link = "logit"), 
          data = select(SAheart, -tobacco, -id))
summary(m0)

 
## M1: Wihtout sbp, alcohol; and exclude either adiposity or obesity
m1.1 <- glm(chd ~ tobacco + age + famhist + ldl + typea + adiposity, 
            family = binomial(link = "logit"), data = SAheart)
summary(m1.1)

m1.2 <- glm(chd ~ tobacco + age + famhist + ldl + typea + obesity, 
            family = binomial(link = "logit"), data = SAheart)
summary(m1.2)



## M2: Exclude sbp, alcohol, adiposity, obesity
m2 <- glm(chd ~ tobacco + age + famhist + ldl + typea, 
          family = binomial(link = "logit"), data = SAheart)
summary(m2)


## M3: For sake of simplicity of models we try to exclude typea, since effect seems to be still very low
m3 <- glm(chd ~ tobacco + age + famhist + ldl, 
          family = binomial(link = "logit"), data = SAheart)
summary(m3)



## typea: mediator effect via age
    
## Does typea has an influence on chd as regression suggest; graphics doesn't really show this
mean(SAheart$chd[SAheart$typea >= median(SAheart$typea)])
mean(SAheart$chd[SAheart$typea < median(SAheart$typea)])

multiBP("typea")  

obslist <- (SAheart %>%
              filter(chd == 1 & typea < 30) %>%
              select(id))$id

varlist <- c("sbp", "tobacco", "age", "ldl", "typea", "famhist")
multiJP(varlist, obslist, label = TRUE)


          
summary(glm(chd ~ typea, family = binomial(link = "logit"), data = SAheart))
summary(glm(chd ~ typea + age, family = binomial(link = "logit"), data = SAheart))

