generateFractalsDown <- function(timeframe) {
    for (i in 1:length(data[[timeframe]])) {
        if(data[[timeframe]]$Low[i] > data[[timeframe]]$Low[i] 
        && data[[timeframe]]$Low[i] > data[[timeframe]]$Low[i]){
            fractal_down_high <- data[[timeframe]]$Low[i]
            fractal_down_close <- data[[timeframe]]$Close[i]
            fractal_down_position <- i
            fractal_down_timeframe <- timeframe
            fractal_down[nrow(fractal_up) + 1] <- c(fractal_down_close, fractal_down_high, fractal_down_position, fractal_down_timeframe)
        }
    }
    return(fractal_down)
}

getNearestFractalUp <- function(pos) {
   for (i in pos : 1){
        if(fractal_up[i]$fractal_up_position < pos && fractal_up[i]$Low[i] > fractal_up[i]$Low[i]) {
            return(fractal_up_close[i])
        }
    }
}

getNearestFractalDown <- function(pos) {
   for (i in 1 : length(fractal_down)) {
        if(data[[timeframe]]$Low[i] > data[[timeframe]]$Low[i] && data[[timeframe]]$Low[i] > data[[timeframe]]$Low[i]) {
            return(fractal_down[i])
        }
    }
}
