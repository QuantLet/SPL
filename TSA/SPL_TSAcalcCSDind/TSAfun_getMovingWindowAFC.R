

## Function: getMovingWindowACF
## Based on: https://stats.stackexchange.com/a/113662 
    ## Args:    x  - time series of length N.
    ##          ws - size of rolling window (must be w < N).
    ##
    ## Returns: Vector of length N-ws containing the moving window
    ##          lag-1-autocorrelation for x.

getMovingWindowACF = function(x, ws) {
    
    # Load packages
    if(!require("stats")) install.packages("stats")
    library("stats")
    
    # Check if window size is too wide
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
    
    # Precompute kernels for summing over windows of length ws and ws-1
    # (Discrete Fourier Transform of 0/1 vector according to window size)
    m = floor((ws+1) / 2)
    
    kernel      = fft(c(rep(1, m), rep(0, N-ws+1), rep(1, ws-m-1)))
    kernel.full = fft(c(rep(1, m), rep(0, N-ws), rep(1, ws-m)))
    
    # Lag 1 of the original data
    x.lag   = c(x[-1], zero)
    x.trunc = c( x[-N], zero)
    
    # Compute the needed rolling sums contained in ACF(x)_1 formula
    x.sum       = sumWindow(x, kernel)
    x.lag.sum   = c(x.sum[-1], zero)
    x.trunc.sum = c(x.sum[-N], zero)
    x.prod      = sumWindow(x.lag * x.trunc, kernel)
    x.mean      = sumWindow(x, kernel.full) / ws
    x.2         = sumWindow(x^2, kernel.full)
    
    # Compute ACF(x)_1
    a = x.prod - x.mean*(x.lag.sum+x.trunc.sum) + x.mean^2*(ws-1)
    a = a / (x.2 - ws * x.mean^2)
    
    # Return vector with autocorrelations for for moving windows of size ws
    return(a[m:(N-ws+m)])
    
}
