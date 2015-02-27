format.percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

counter.print = function(interval){
    if(counter %% interval ==0) print(paste(Sys.time(),counter))
    counter <<- counter+1
}

counter.reset = function(){counter <<- 1}

time.print = function(ts_now, ts_last, str_desc){
    print(paste(ts_now, str_desc, 
                as.integer(ts_now)-as.integer(ts_last), "seconds.") )
}
