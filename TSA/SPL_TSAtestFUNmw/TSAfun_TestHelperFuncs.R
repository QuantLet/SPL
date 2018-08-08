

# Load required packages
if(!require("zoo")) install.packages("zoo")
library("zoo")
if(!require("e1071")) install.packages("e1071")
library("e1071")

# Function for easy string pasting
"%&%" = function(x, y) paste(x, y, sep = "")


# Functions using rollapply that use the same args as getMovingWindow functions
rollapplyACF  = function(x, w) {
    rollapply(x,
              width = w,
              FUN   = function(x) {acf(x, plot = FALSE, lag.max = 1)$acf[2]})
    }
rollapplyVAR  = function(x, w) {
    rollapply(x,
              width = w,
              var)
    }
rollapplySKEW = function(x, w) {
    rollapply(x,
              width = w,
              skewness)
    }


# Function to compute deviation and print running time of two functions f1 and f2
compareFunctions = function(f1, f2, ...) {
    t1 = system.time(y1 <-  f1(...))
    t2 = system.time(y2 <-  f2(...))
    
    return(list('sum of squared deviations' = sum((y1 - y2)^2),
                'running times'             = rbind(t1, t2)))
}


# Function to compare running time of of two functions f1 and f2 iterating
# through all relevant window sizes and repeating 50 times to make differences 
# in running time more apparent
compareTime = function(ts, f1, f2){
    a  = 0
    t1 = system.time(
        while(a < 50){
            a = a+1
            for(i in 3:floor(length(ts)/2)){
                t = f1(ts, i)
            }
        })
    
    a  = 0
    t2 = system.time(
        while(a < 50){
            a = a+1
            for(i in 3:floor(length(ts)/2)){
                t = f2(ts, i)
            }
        })
    return(list("getMovingWindow" = t1, "rollapply" = t2))
}

