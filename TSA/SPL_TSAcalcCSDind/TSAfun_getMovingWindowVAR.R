

## Function: getMovingWindowVAR
## Based on: https://stats.stackexchange.com/a/113662 
    ## Args:    x  - time series of length N.
    ##          ws - size of rolling window (must be ws < N).
    ##
    ## Returns: Vector of length N-ws containing the moving window
    ##          variance for x.

getMovingWindowVAR = function(x, ws) {
    
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
    
    # Precompute kernel for summing over windows of length ws
    # (Discrete Fourier Transform of 0/1 vector according to window size)
    m = floor((ws+1) / 2)
    
    kernel.full = fft(c(rep(1, m), rep(0, N-ws), rep(1, ws-m)))
    
    # Compute the needed rolling sums contained in VAR(x) formula
    x.mean = sumWindow(x, kernel.full) / ws
    x.2    = sumWindow(x^2, kernel.full)
    
    # Compute VAR(x)
    v = (x.2 - ws * x.mean^2) / (ws-1)
    
    # Return vector with variance for for moving windows of size ws
    return(v[m:(N-ws+m)])
    
}
