

## Function: getMovingWindowSKEW
## Based on: https://stats.stackexchange.com/a/113662 
    ## Args:    x  - time series of length N.
    ##          ws - size of rolling window (must be ws < N).
    ##
    ## Returns: Vector of length N-ws containing the moving window
    ##          skewness for x.

getMovingWindowSKEW = function(x, ws) {
    
    # Load packages
    if(!require("stats")) install.packages("stats")
    library("stats")
    
    # Check if window size is to wide
    N = length(x)
    if (ws > N) stop("Window too wide.")
    
    # Generate auxiliary objects
    zero = 0
    
    # Function to compute a rolling sum given the FFT of its kernel
    # Args: z - time series of which the rolling sum should be computed
    #       k - kernel used to compute rolling sum 
    
    sumWindow = function(z, k) {
        Re(fft(fft(z) * k, inverse=TRUE)) / length(z)
    }
    
    # Precompute kernels for summing over windows of length ws
    # (Discrete Fourier Transform of 0/1 vector according to window size)
    m = floor((ws+1) / 2)
    
    kernel.full = fft(c(rep(1, m), rep(0, N-ws), rep(1, ws-m)))
    
    # Compute the needed rolling sums contained in SKEW(x) formula
    x.sum  = sumWindow(x, kernel.full)
    x.mean = sumWindow(x, kernel.full) / ws
    x.2    = sumWindow(x^2, kernel.full)
    x.3    = sumWindow(x^3, kernel.full)
    
    # Compute SKEW(x)
    s = x.3 - 3 * x.2 * x.mean + 3 * x.sum * x.mean^2  - ws * x.mean^3
    s = s / (ws * ( sqrt((x.2 - ws * x.mean^2) / (ws-1)) )^3)   
    
    # Return vector with skewness for for moving windows of size ws
    return(s[m:(N-ws+m)])
    
}

