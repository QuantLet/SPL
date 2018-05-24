
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
##  Fisher Linear Discirminant  ##
##                              ##
##################################
  

## Function for Fisher Linear Discriminant
fisher <- function(X, c, PDFpath = NULL){
  
  X <- as.matrix(X)
  
  X0 <- X[c == 0,]
  X1 <- X[c == 1,]

  n0 <- nrow(X0)
  n1 <- nrow(X1)

  m0 <- as.vector(colMeans(X0))
  m1 <- as.vector(colMeans(X1))

  M0 <- as.vector(rep(1, n0)) %*% t(m0)
  M1 <- as.vector(rep(1, n1)) %*% t(m1)
    
  X0_c <- X0 - M0
  X1_c <- X1 - M1

  S0 <- t(X0_c) %*% X0_c
  S1 <- t(X1_c) %*% X1_c
  
  S_w <- S0 + S1

  S_w_inv <- solve(S_w)

  w <- S_w_inv %*% (m0 - m1)

  m <- as.vector(colMeans(X))
  M <- as.vector(rep(1, nrow(X))) %*% t(m)
  X_c <- X - M
  
  z <- t(w) %*% t(X_c) < 0 
  
  accuracy <- sum(t(z) == c) / length(z)
    
  Y0 <- X0 %*% w
  Y1 <- X1 %*% w
    
  Y <- rbind(cbind(Y0, rep(0,nrow(Y0))),
             cbind(Y1, rep(1,nrow(Y1))))
  Y <- as.data.frame(Y)
  colnames(Y) <- c("Y","c")
    
  final.plot <- ggplot(Y, aes(Y, fill = as.factor(c))) + geom_density(alpha = 0.2) +
    theme_classic() +
    labs(x = "", y = "") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) +                              
      scale_fill_manual(values = c("firebrick", "dodgerblue"), 
                        name = "chd", labels = c("no", "yes"))
  
  plot(final.plot)  
      
  if(!is.null(PDFpath)){
    ggsave(filename = "/Densitiesplot.pdf", path = PDFpath, device = "pdf", plot = final.plot)
  }

  return(accuracy)
}
  
  
X <- select(SAheart, -id, -chd)
c <- SAheart$chd

fisher(X=X, c=c, PDFpath)
