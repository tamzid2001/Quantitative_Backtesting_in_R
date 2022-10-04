generateBuySignals <- function(timeframe) {
    columns <- c("Date", "price", "angle", "tan_left", "tan_right", "tan2_left", "tan2_right", "position", "active")
    buy_signals <- data.frame(matrix(nrow = nrow(data[[timeframe]]), ncol = length(columns)))
    colnames(buy_signals) <- columns
    l <- nrow(data[[timeframe]]) - 4
    for (i in l:10) {
        # print(sma9_data[[timeframe]]$SMA[[i]] > sma9_data[[timeframe]]$SMA[[i + 1]])
    }
    f <- function(x) as.POSIXct(as_datetime(x, origin = lubridate::origin, tz="UTC"))
    # print(f(1632686400))
    bf <- na.omit(buy_signals)
    z <- read.zoo(bf, header=TRUE, index.column = 1, FUN = function(x) as.POSIXct(as_datetime(x, origin = lubridate::origin, tz="UTC")))
    g <- as.xts(z)
    #print(g)
    #s <- merge(data[[timeframe]], g)
    return(g)
}
#ff <- generateBuySignals(5)
#rr <- 1:nrow(ff)
#print(ff)
#plot(rr, ff$angle, main = "angle_distribution", xlab = "date", ylab = "angle")
#plot(rr, ff$tan_left, main = "angle_distribution", xlab = "date", ylab = "angle")
# store sell signals
generateSellSignals <- function(timeframe) {
    columns <- c("Date", "limit_price", "trade_angle", "tan_left", "tan_right", "position", "active")
    sell_signals <- data.frame(matrix(nrow = nrow(data[[timeframe]]), ncol = length(columns)))
    colnames(sell_signals) <- columns
    for (i in 2:length(data[timeframe])){

    }

    bf <- na.omit(sell_signals)
    z <- read.zoo(bf, header=TRUE, index.column = 1, FUN = function(x) as.POSIXct(as_datetime(x, origin = lubridate::origin, tz="UTC")))
    g <- as.xts(z)
    s <- merge(data[[timeframe]], g)
    return(s)
}
