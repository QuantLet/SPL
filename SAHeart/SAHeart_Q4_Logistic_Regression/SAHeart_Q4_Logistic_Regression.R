
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

## Function to show Boxplots of covariates over chd
multiBP <- function(varlist, PDFpath = NULL){
  plot.list <- lapply(varlist, function(ivar){
    ggplot(data = SAheart, mapping = aes(x = as.factor(chd), y = get(ivar))) +
      geom_boxplot(stat = "boxplot", position = "dodge") +
      theme_bw() +
      xlab("chd") + 
      ylab(ivar)
  })
  
  if(length(plot.list) <= 3){
    ncol <- length(plot.list)
  }else{
    ncol <- 3
  }
  
  final.plot <- grid.arrange(grobs = plot.list, ncol=ncol)
}

## Function to visualize behaviour of single observations 
multiJP <- function(varlist, obslist = 0, PDFpath = NULL, label = FALSE){
  
  if(label){
    labellist <- obslist
    varlist <- c(varlist, "legend")
  }else{
    labellist <- ""
  }
  
  color.list <- c("firebrick", "dodgerblue", "forestgreen", 
                  "orangered", "violet", "yellow3")
  
  plot.list <- lapply(varlist, function(ivar){
    if(ivar == "legend"){
      hackdata <- data.frame(id = obslist,
                             position = seq(1,length(obslist)),
                             xvalue = 0.11)
      legend <- ggplot(data = hackdata, mapping = aes(x = xvalue, y = position)) +
        geom_point(colour = color.list[1:length(obslist)], shape = 17, size = 3) +
        theme_classic() +
        theme(axis.text = element_blank(), axis.ticks = element_blank(), 
              axis.line = element_blank()) + 
        labs(x = "", y = "") +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max(hackdata$position)+1)) +
        scale_x_continuous(limits = c(0.1, 1)) +
        geom_text(data = hackdata, aes(label = id), 
                  colour = color.list[1:length(obslist)], size = 3, hjust = -1)
      
    }else{
      
      ggplot(data = SAheart[!(SAheart$id %in% obslist),], 
             mapping = aes(x = as.factor(chd), y = get(ivar))) +
        geom_jitter(shape = 1) +
        theme_bw() +
        xlab("chd") + 
        ylab(ivar) +
        geom_jitter(data = SAheart[SAheart$id %in% obslist,], 
                    colour = color.list[1:length(obslist)], shape = 17, size = 3) # +
    }
  })
  
  if(length(plot.list) <= 3){
    ncol <- length(plot.list)
  }else{
    ncol <- 3
  }
  final.plot <- grid.arrange(grobs = plot.list, ncol=ncol)
}
  
##################################
##                              ##
##      Logistic Regression     ##
##                              ##
##################################       


## M0: (naive) Full model
m0 <- glm(chd ~ ., family = binomial(link = "logit"), 
          data = select(SAheart, -id))
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


## M3: For sake of simplicity of models we try to exclude
## typea, since effect seems to be still very low
m3 <- glm(chd ~ tobacco + age + famhist + ldl, 
          family = binomial(link = "logit"), data = SAheart)
summary(m3)



## typea: mediator effect via age

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