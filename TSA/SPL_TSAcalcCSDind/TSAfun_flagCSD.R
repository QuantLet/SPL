

## Function: flagCSD
    ## Args:     x  - data frame containing moving window W_2 measures.
    ##           
    ## Returns:  data frame with six columns PER window size included in x:
    ##             - col 1: TRUE/FALSE indicating whether the W_2 measure for
    ##                      a window is deviating by more than two standard
    ##                      deviations from its running mean
    ##             - col 2: running mean of W2 measure
    ##             - col 3: running standard deviation of W2 measure
    ##             - col 4: running standard deviation of W2 times 2
    ##             - col 5: negative running standard devation of W2
    ##             - col 6: negative running standard devation of W2 time 2


flagCSD = function(x){
    # Check wether function getRunningSD was already sourced
    if(!exists("getRunningSD")){
        # If not ask for path
        q = readline(paste("Function getRunningSD not available.",
                            "Specify path to source file:", sep = " "))
        if(is.character(q) == TRUE){
            q = as.character(q)
            source(q)
        } else {
            source(q)
        }
        # Check whether function is availble now
        if(!exists("getRunningSD")){
            # If not throw error
            stop("Invalid file path")
        }
        
        # If function is available proceed with calculation of CSD flags
    } else {
        # Initialize data frame
        out = c(1:nrow(x))
        
        # Initialize column index for column naming
        c   = 2
        
        # Loop through window sizes
        for(i in 2:ncol(x)){
            running.mean    = c(rep(NA, sum(is.na(x[, i]))),
                                cumsum(x[!is.na(x[, i]), i]) /
                                    1:(nrow(x) - sum(is.na(x[, i]))))
            running.SD      = getRunningSD(apply(x, 2, function(y) {
                                                       replace(y, is.nan(y), 0)
                                                       }),
                                           (i+1))
            running.SD2     = 2 * running.SD
            neg.running.SD  = - running.SD
            neg.running.SD2 = - running.SD2
            
            flag            = abs(x[, i]) > running.mean + 2 * running.SD
            
            # Attach calculated vectors to data frame
            out = data.frame(out, flag, running.mean, running.SD, running.SD2,
                             neg.running.SD, neg.running.SD2)
            
            # Name columns
            colnames(out)[c:(c+5)] = paste(colnames(out[, c:(c+5)]),
                                           "_ws", (i+1), sep = "")
            c = c + 6
        }
        
        return(out[,-1])
    }
}

