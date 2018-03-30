
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

##################################
##                              ##
##       Cook's Distance        ##
##                              ##
##################################            
  
          
cookPlot <- function(model, threshold = NULL, PDFpath = NULL){
  
  cd <- cooks.distance(m2)
  
  if(!is.null(threshold)){
    additional_params <- list(geom_text(data = data.frame(id = SAheart$id,
                                                          cd = cd)[cd >= threshold,], 
                                        aes(label = id), colour = 'firebrick', 
                                        size = 3, vjust = -0.5),
                               geom_hline(yintercept = threshold, color = 'firebrick'))
  }else{
    additional_params <- NULL
  }
  
  final.plot <- ggplot(data = data.frame(id = SAheart$id, cd = cd), 
                       mapping = aes(x = as.factor(id), y = cd)) +
    geom_bar(stat = "identity", width = 0.1, color = "black") +
    theme_classic() +
    labs(x = "", y = "Cook's D") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +                                  
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(cd)+0.002)) +                                    
    additional_params
  
  plot(final.plot)
  
  if(!is.null(PDFpath)){
    ggsave(filename = "/CooksDplot.pdf", path = PDFpath, device = "pdf", plot = final.plot)
  }
  
}


cookPlot(m2, threshold = 0.015, PDFpath = PDFpath)

cd <- cooks.distance(m2)

obslist <- SAheart$id[cd >= 0.015]
varlist <- c("typea", "tobacco", "age", "ldl", "famhist")
multiJP(varlist, obslist, label = TRUE, PDFpath = PDFpath)

## Drop all very influencial obs for model calc
  obslist <- SAheart$id[cd >= 0.015]
  m2.1 <- glm(chd ~ tobacco + age + famhist + ldl + typea, 
              family = binomial(link = "logit"), 
              data = SAheart[!(SAheart$id %in% obslist),])
  summary(m2.1)
  loo_cv(m2.1)
  
# Drop only the influencial points that are very suspiciuous in the graphics
# and whose excluding can be justified with theory
  obslist <- c("17", "261", "21")
  m2.2 <- glm(chd ~ tobacco + age + famhist + ldl + typea, 
              family = binomial(link = "logit"), 
              data = SAheart[!(SAheart$id %in% obslist),])
  summary(m2.2)
  loo_cv(m2.2)