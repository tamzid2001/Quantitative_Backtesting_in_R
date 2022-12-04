e <- 2.71828
#store fractals high
generateFractalsUp <- function(timeframe) {
    fractal_up <- as.data.frame(matrix(nrow = nrow(data[[timeframe]]), ncol = 5))
    colnames(fractal_up) <- c("Date", "fractal_up_high", "fractal_up_close", "fractal_up_position", "fractal_up_timeframe")
    l <- nrow(data[[timeframe]]) - 2
    for (i in 1 : l) {
        if(data[[timeframe]]$High[[i]] < data[[timeframe]]$High[[i + 1]] 
        && data[[timeframe]]$High[[i + 1]] > data[[timeframe]]$High[[i + 2]]) {
            fractal_up$fractal_up_high[i + 1] <- data[[timeframe]]$High[[i + 1]]
            fractal_up$fractal_up_close[i + 1] <- data[[timeframe]]$Close[[i + 1]]
            fractal_up$fractal_up_position[i + 1] <- i + 1
            fractal_up$fractal_up_timeframe[i + 1] <- timeframe
            d <- index(data[[timeframe]][i + 1])
            #print(d)
            fractal_up$Date[i + 1] <- d
            #fractal_up[i] <- c(Date, fractal_up_close, fractal_up_high, fractal_up_position, fractal_up_timeframe)
        }
    }
    #print(as.xts(na.omit(fractal_up)), header=TRUE, index.column=1, format="%Y-%m-%d %H:%M:%OS",tz="UTC")
    #print(fractal_up)
    f <- function(x) as.POSIXct(as_datetime(x, origin = lubridate::origin, tz="UTC"))
    #print(f(1630454400))
    bf <- na.omit(fractal_up)
    z <- read.zoo(bf, header=TRUE, index.column = 1, FUN = function(x) as.POSIXct(as_datetime(x, origin = lubridate::origin, tz="UTC")))
    g <- as.xts(z)
    s <- merge(data[[timeframe]], g)
    #print(s)
    #print(g)
    return(s)
}

print(generateFractalsUp(8))

#store fractals low
generateFractalsDown <- function(timeframe) {
    for (i in 1:length(data[[timeframe]])) {
        if(data[[timeframe]]$Low[i] > data[[timeframe]]$Low[i] 
        && data[[timeframe]]$Low[i] > data[[timeframe]]$Low[i]){
            fractal_down_high <- data[[timeframe]]$Low[i]
            if(data[[timeframe]]$Close[i] > data[[timeframe]]$Open[i]){
                fractal_down_close <- data[[timeframe]]$Open[i]
            } else if(data[[timeframe]]$Close[i] < data[[timeframe]]$Open[i]){
                fractal_down_close <- data[[timeframe]]$Close[i]
            }
            fractal_down_position <- i
            fractal_down_timeframe <- timeframe
            fractal_down[nrow(fractal_up) + 1] <- c(fractal_down_close, fractal_down_high, fractal_down_position, fractal_down_timeframe)
        }
    }
    return(fractal_down)
}

getNearestFractalUp <- function(timeframe, pos) {
    fractal_up <- generateFractalsUp(timeframe)
   for (i in pos : 1){
        if(!is.na(fractal_up$fractal_up_position[i])) {
            return(fractal_up$fractal_up_close[[i]])
        }
    }
}

print(getNearestFractalUp(8, 10))

getNearestFractalDown <- function(pos) {
    fractal_down <- generateFractalsDown(timeframe)
   for (i in 1 : length(fractal_down)) {
        if(!is.na(fractal_down$fractal_down_position[i])) {
            return(fractal_down$fractal_down_close[i])
        }
    }
}

# calculate angle
dist <- function(x1, x2, y1, y2) {

    return(sqrt(((abs(x2 - x1))^2 + (abs(y2 - y1))^2)))

}

# pointA <- 1658.669
# pointB <- 1658.587
# pointC <-1658.720

# legA <- dist(2, 1, 1658.669, 1658.587)
# legB <- dist(2, 1, 1658.587, 1658.720)
# legC <- dist(3, 1, 1658.669, 1658.720)

theta <- function(a, b, c) {
    rad1 <- acos((a^2 + b^2 - c^2) / (2 * a * b))
    degree1 <- rad1 * (180 / pi)
    rad2 <- acos((a^2 + c^2 - b^2) / (2 * a * c))
    degree2 <- rad2 * (180 / pi)
    rad3 <- acos((b^2 + c^2 - a^2) / (2 * c * b))
    degree3 <- rad3 * (180 / pi)
    angle <- c(degree1, degree2, degree3)
    return(max(angle))
}

tan_left <- function(o, h) {
    rad <- asin(o/h)
    angle <- rad * (180 / pi)
    return(angle)
}

tan_right <- function(o, h) {
    rad <- asin(o/h)
    angle <- rad * (180 / pi)
    return(angle)
}
