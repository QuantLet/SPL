

## Function: getData
    ## Args:     file      - file path of csv dataset containing at least 5
    ##                       Tweets per day to be read.
    ##           sentiment - "positive" or "negative ("positive" is default).
    ##           
    ## Returns:  s         - matrix with to columns: 'Date' contains the date
    ##                       the Tweet was posted in the format dd-mm-yyyy.
    ##                       'Sentiment' contains the sentiment score of that
    ##                       Tweet according to syuzhet::get_sentiment.
    ##           ts        - time series containing the share of Tweets for
    ##                       each day that have a positive sentiment score or
    ##                       the share of Tweets for each day that have a
    ##                       negative sentiment score depending on the arg
    ##                       'sentiment'.

getData = function(file, sentiment = "positive"){
    
    # Load required package
    if(!require("data.table")) install.packages("data.table")
    if(!require("lubridate"))  install.packages("lubridate")
    if(!require("syuzhet"))    install.packages("syuzhet")
    
    library("data.table")
    library("lubridate")
    library("syuzhet")
    
    # Read data sets
    df = fread(file)
    
    # Reformat Tweet texts as character and timestamp as date
    if("text" %in% colnames(df) & "created_at" %in% colnames(df)) {
        df$text       = as.character(df$text)
        df$created_at = ymd_hm(df$created_at)
    } else {
        stop(paste("Data file does not contain columns",
                   "named 'text' and 'created_at'"))
    }
    
    # Get sentiment scores for each tweet
    s = data.frame("Date"      = as.Date(df$created_at ,  format = '%Y/%m/%d'),
                   "Sentiment" = get_sentiment(df$text, method = "syuzhet"))
    
    
    # Check that for each date present in time series at least five Tweets
    # are available and each date between minimum and maximum date is contained
    if((min(unique(table(s$Date))) < 5) |
       (sum(seq(min(s$Date), max(s$Date), by = 1) %in% s$Date)
        != length(unique(s$Date)))) {
        stop(paste("For at least one date, there are less than 5", 
                   "or none observations available."))
    }
    
    # Calculate share of positive/negative sentiment Tweets per day
    ts = switch(sentiment,
                "positive" = as.vector(by(s, s$Date, function(x) {
                    sum(x[x$Sentiment > 0, 2])/nrow(x)
                })),
                "negative" = as.vector(by(s, s$Date, function(x){
                    sum(x[x$Sentiment < 0, 2])/nrow(x)
                }))
    )
    
    return(list(s, ts))
    
    
}

