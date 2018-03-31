
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
    

## famhist
head(SAheart$famhist, 10)                           
head(as.numeric(SAheart$famhist), 10)               
SAheart$famhist <- as.numeric(SAheart$famhist)     
SAheart$famhist[SAheart$famhist == 1] <- 0   
SAheart$famhist[SAheart$famhist == 2] <- 1



##################################
##                              ##
##     Exploratory Analysis     ##
##                              ##
##################################

  
## Correlation matrix (using package corrplot)
pdf(file = paste0(PDFpath,"/Corrplot.pdf"), width = 7, height = 8)  
corr_matrix <- cor(select(SAheart, chd, everything(), -id))   
colset <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot.mixed(corr_matrix,                    
               upper = "ellipse", 
               lower = "number", 
               number.cex = .7,           
               upper.col = colset(10),
               lower.col = "black", 
               tl.col = "black", 
               tl.pos = "lt")   
dev.off()


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
  
  if(!is.null(PDFpath)){
    ggsave(filename = "/BPplot.pdf", path = PDFpath, device = "pdf", plot = final.plot)
  }
}


varlist <- c("sbp", "tobacco", "age", "ldl", "adiposity")       
multiBP(varlist, PDFpath)


## Suspicious outlier
SAheart$id[SAheart$age <= 20 & SAheart$chd == 1]

subset(SAheart, select = varlist, id == 261)         
subset(SAheart, select = varlist, id == 21)
summary(subset(SAheart, select = varlist, chd == 1))

## Function to visualize behaviour of single observations 
multiJP <- function(varlist, obslist = 0, PDFpath = NULL, label = FALSE){
  
  
  if(label){
    varlist <- c(varlist, "legend")
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
    ncol <- 3 }
  
  final.plot <- grid.arrange(grobs = plot.list, ncol=ncol)
  
  if(!is.null(PDFpath)){
    ggsave(filename = "/Jitterplot_outliers.pdf", path = PDFpath, device = "pdf", plot = final.plot)}}


varlist <- c("sbp", "tobacco", "age", "ldl", "adiposity")
multiJP(varlist, obslist = c(261))


## Also check for the other very young person:
multiJP(varlist, obslist = c(261, 21), label = TRUE, PDFpath = PDFpath)
  

## Check in general for obs that have chd but have very low values on all important variables
obslist <- (SAheart %>%
              filter(chd == 1,
                     sbp <= quantile(sbp, .25),
                     tobacco <= quantile(tobacco, .25),
                     age <= quantile(age, .25),
                     ldl <= quantile(ldl, .25),
                     adiposity <= quantile(adiposity, .25),
                     famhist == 0) %>%
              select(id))$id

