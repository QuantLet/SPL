
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


## M0: (naive) Full model
m0 <- glm(chd ~ ., family = binomial(link = "logit"), 
          data = select(SAheart, -tobacco, -id))

## M1: Wihtout sbp, alcohol; and exclude either adiposity or obesity
m1.1 <- glm(chd ~ tobacco + age + famhist + ldl + typea + adiposity, 
            family = binomial(link = "logit"), data = SAheart)

m1.2 <- glm(chd ~ tobacco + age + famhist + ldl + typea + obesity, 
            family = binomial(link = "logit"), data = SAheart)

## M2: Exclude sbp, alcohol, adiposity, obesity
m2 <- glm(chd ~ tobacco + age + famhist + ldl + typea, 
          family = binomial(link = "logit"), data = SAheart)

## M3: For sake of simplicity of models we try to exclude typea, since effect seems to be still very low
m3 <- glm(chd ~ tobacco + age + famhist + ldl, 
          family = binomial(link = "logit"), data = SAheart)


##################################
##                              ##
##        loo-cv function       ##
##                              ##
##################################


loo_cv <- function(model, cutoff = 0.5){
  
  cur.data <- model$data
  cur.formula <- model$formula
  N <- nrow(cur.data)

  prediction <- vector(mode = "numeric", length = N)
  
  for(i in 1:N){
    cur.model <- glm(cur.formula, family = binomial(link = "logit"), 
                     data = cur.data[-i,])
    prediction[i] <- predict(cur.model, newdata = cur.data[i,], type = "response")
  }
  
  correct <- cur.data$chd == (prediction > cutoff)
  
  accuracy <- sum(correct)/N
    
  return(accuracy)
}


loo_cv(m0)
loo_cv(m2)
loo_cv(m3)
