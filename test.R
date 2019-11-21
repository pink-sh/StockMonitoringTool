library(lubridate)

formatTimestamp <- function(ts) {
        x <- format(as.Date(as.POSIXct(ts, origin="1970-01-01")), format="%Y-%m-%d")
        return(x)
}

x <- c('2016-01-05')
y <-  c('2016-03-05',
       '2016-04-05',
       '2016-07-05',
       '2016-08-05',
       '2016-10-05',
       '09-08-2019')

z <- c('2/7/2016')

timestamps <- as.vector(
                unlist(
                        lapply(
                        lapply(y, parse_date_time, order=c('ymd', 'dmy', 'mdy') )
                        , formatTimestamp)
                )
        )
print (timestamps)

