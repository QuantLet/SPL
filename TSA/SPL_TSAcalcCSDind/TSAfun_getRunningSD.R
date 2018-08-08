

## Function: getRunningSD
    ## Args:    x  - output of one of the getMovingWindow* functions (matrix
    ##               or data frame containing CSD indicators computed for
    ##               moving windows in each row with different window size in
    ##               each column).
    ##          ws - size of rolling window for which running standard
    ##               deviation should be computed.
    ##           
    ## Returns: vector of running standard deviations

getRunningSD = function(x, ws){
    
    # Vector that contains values for n in standard deviation formula
    n      = c(rep(NA, (ws-1)), 1:(nrow(x)-ws+1))
    
    # Calculate squared cumulative sum
    x.2    = c(rep(NA, (ws-1)), cumsum(x[ws:nrow(x) , (ws-1)]^2))
    
    # Calculate running mean
    x.mean = c(rep(NA, (ws-1)), cumsum(x[ws:nrow(x) , (ws-1)])) / n
    
    # Calculate standard deviation
    sd     = sqrt((x.2 - n * x.mean^2) / (n-1))
    
    # Return result vector
    return(sd)
    
}

