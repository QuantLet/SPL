
rm(list = ls())

# Source custom functions
function.sources = list.files(pattern = "(TSAfun_){1}.*.R")
sapply(function.sources, source, .GlobalEnv)

# Function for easy string pasting
"%&%" = function(x, y) paste(x, y, sep = "")

# Get data
ts = getData("tweets_raw.csv")[[2]]



#### Calculate rolling window statistics for ts ####

# Set maximium window size
max.ws = floor(length(ts)/2)

# Lag-1-autocorrelation
acf = c(1:length(ts))

for(i in 3:max.ws) {
    temp = getMovingWindowACF(ts, w = i)
    acf  = data.frame(acf, c(rep(NA, i-1), temp))
    
    colnames(acf)[ncol(acf)] = "acf_ws"%&%i
}

# Variance
var = c(1:length(ts))

for(i in 3:max.ws) {
    temp = getMovingWindowVAR(ts, w = i)
    var  = data.frame(var, c(rep(NA, i-1), temp))
    
    colnames(var)[ncol(var)] = "var_ws"%&%i
}

# Skewness
skew = c(1:length(ts))

for(i in 3:max.ws) {
    temp = getMovingWindowSKEW(ts, w = i)
    skew  = data.frame(skew, c(rep(NA, i-1), temp))
    
    colnames(skew)[ncol(skew)] = "skew_ws"%&%i
}



#### Test getRunningSD function ####
  
# Test whether getRunningSD function and for-loop calculating standard
# deviation with standard package functions generate same results

# Set tolerance for deviations between results (Smaller deviations
# are assumed to be due to accumulated rounding differences)
tol = 1e-15

# Iterate through window sizes
for(ws in 3:floor(length(ts)/2)) {
    
    # Lag-1-autocorrelation
    run_sd_acf  = getRunningSD(acf, ws)
    
    run_sd_acf2 = rep(NA, nrow(acf))
    for (i in (ws+1):nrow(acf)) {
        run_sd_acf2[i] = sd(acf[ws:i, ws-1])
    }
    print(paste("Window size "%&%ws%&%":"))
    print(paste("Lag-1-autocorrelation ->",
                if(max(abs(run_sd_acf-run_sd_acf2), na.rm = TRUE) > tol) {
                    paste("Maximum absolute deviation between getRunningSD",
                          "and for-loop is: "%&%max(abs(run_sd_acf-run_sd_acf2),
                                                    na.rm = TRUE))
                } else {
                    paste("All deviations are smaller than "%&%tol)
                }
    ))
    
    # Variance
    run_sd_var  = getRunningSD(var, ws)
    
    run_sd_var2 = rep(NA, nrow(var))
    for (i in (ws+1):nrow(var)) {
        run_sd_var2[i] = sd(var[ws:i, ws-1])
    }
    
    print(paste("Variance              ->",
                if(max(abs(run_sd_var-run_sd_var2), na.rm = TRUE) > tol) {
                    paste("Maximum absolute deviation between getRunningSD",
                          "and for-loop is: "%&%max(abs(run_sd_var-run_sd_var2),
                                                    na.rm = TRUE))
                } else {
                    paste("All deviations are smaller than "%&%tol)
                }
    ))
    
    # Skewness
    run_sd_skew  = getRunningSD(skew, ws)
    
    run_sd_skew2 = rep(NA, nrow(skew))
    for (i in (ws+1):nrow(skew)) {
        run_sd_skew2[i] = sd(skew[ws:i, ws-1])
    }
    
    print(paste("Skewness              ->",
                if(max(abs(run_sd_skew-run_sd_skew2), na.rm = TRUE) > tol) {
                    paste("Maximum absolute deviation between getRunningSD",
                          "and for-loop is: "%&%max(abs(run_sd_skew-run_sd_skew2),
                                                    na.rm = TRUE))
                } else {
                    paste("All deviations are smaller than "%&%tol)
                }
    ))
    
    # Tidy up after each window size
    rm(ws, list = ls(pattern = "run_"))
    
}    

    