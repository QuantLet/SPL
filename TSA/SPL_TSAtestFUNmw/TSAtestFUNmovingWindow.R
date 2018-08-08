
rm(list = ls())

# Load required packages
if(!require("zoo")) install.packages("zoo")
library("zoo")

# Source custom functions
function.sources = list.files(pattern = "(TSAfun_){1}.*.R")
sapply(function.sources, source, .GlobalEnv)



#### TEST 1: Simulated data ####

# Simulate time series
set.seed(20180705)
sim.ma = arima.sim(list(ma = c(0.6, -0.4)), n = 100000)
window = 10

# Test differences in running time and deviation between results
# (this may take a few seconds depending on your hardware)
compareFunctions(getMovingWindowACF,  rollapplyACF,  sim.ma, window)
compareFunctions(getMovingWindowVAR,  rollapplyVAR,  sim.ma, window)
compareFunctions(getMovingWindowSKEW, rollapplySKEW, sim.ma, window)



#### TEST 2: Empirical time series (sentiment data from Tweets) ####

# Get data
ts = getData("tweets_raw.csv")[[2]]

# Test whether getMovingWindow* functions and rollapply approach
# generate same results

# Set tolerance for deviations between results (Smaller deviations
# are assumed to be due to accumulated rounding differences)
tol = 1e-10

# Iterate through window sizes
for(ws in 3:floor(length(ts)/2)) {
    
    # Lag-1-autocorrelation
    test_acf  = getMovingWindowACF(ts, w = ws)
    test_acf2 = rollapply(ts, width = ws, FUN = function(x){
        acf(x, plot = FALSE, lag.max = 1)$acf[1+1]
    })
    
    print(paste("Window size "%&%ws%&%":"))
    print(paste("Lag-1-autocorrelation ->",
                if(max(abs(test_acf-test_acf2)) > tol) {
                    paste("Maximum absolute deviation between FFT and brute",
                          "force approach is: "%&%max(abs(test_acf-test_acf2)))
                } else {
                    paste("All deviations are smaller than "%&%tol)
                }))
    
    # Variance
    test_var  = getMovingWindowVAR(ts, w = ws)
    test_var2 = rollapply(ts, width = ws, var)
    
    print(paste("Variance              ->",
                if(max(abs(test_var-test_var2)) > tol) {
                    paste("Maximum absolute deviation between FFT and brute",
                          "force approach is: "%&%max(abs(test_var-test_var2)))
                } else {
                    paste("All deviations are smaller than "%&%tol)
                }))
    
    # Skewness
    test_skew  = getMovingWindowSKEW(ts, w = ws)
    test_skew2 = rollapply(ts, width = ws, skewness)
    
    print(paste("Skewness              ->",
                if(max(abs(test_skew-test_skew2)) > tol) {
                    paste("Maximum absolute deviation between FFT and brute",
                          "force approach is: "%&%max(abs(test_skew-test_skew2)))
                } else {
                    paste("All deviations are smaller than "%&%tol)
                }))
    
    # Tidy up after each window size
    rm(ws, list = ls(pattern = "test_"))
    
}    


# Test gain in computing time from using getMovingWindow* funcitons vs rollapply
# (this may take a few seconds depending on your hardware)
compareTime(ts, getMovingWindowACF,  rollapplyACF)
compareTime(ts, getMovingWindowVAR,  rollapplyVAR)
compareTime(ts, getMovingWindowSKEW, rollapplySKEW)

# end of file
            