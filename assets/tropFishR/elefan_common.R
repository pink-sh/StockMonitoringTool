
library (lubridate)
formatTimestamp <- function(ts) {
  return(format(as.Date(as.POSIXct(ts, origin="1970-01-01")), format="%Y-%m-%d"))
}

validateElefanInputFile <- function(file) {
  contents <- read.csv(file)
  
  a<-as.vector(colnames(contents))
  print(head(a,1))
  if (head(a,1) != 'midLength') {
    print("RET1")
    return (NULL)
  } else {
    for (row in 2:length(a)) {
      x <- a[row]
      if (startsWith(x, 'X')) {
        x <- substring(x, 2)
      }
      d <- formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))
      if (is.na(d)) {
        print("RET2")
        return (NULL)
      }
    }
  }
  return (contents)
}

read_elefan_csv <- function(csvFile) {
  a <- validateElefanInputFile(csvFile)
  if (is.null(a)) {
    return (NULL)
  }
  dataSet <- list()
  dataSet$sample.no <- seq(1, nrow(as.matrix(a[,-1])))
  colname <- as.vector(unlist(lapply(colnames(a)[2:length(colnames(a))], substring, 2)))
  
  vectorDates <- as.vector(
    unlist(
      lapply(
        lapply(colname, parse_date_time, order=c('ymd', 'dmy', 'mdy') )
        , formatTimestamp)
    )
  )
  dataSet$dates <- as.Date(vectorDates, "%Y-%m-%d")
  
  #dataSet$dates <- as.Date(colnames(a)[2:length(colnames(a))], "X%Y.%m.%d")
  dataSet$midLengths <- a[[1]]
  dataSet$catch <- as.matrix(a[,-1])
  return (dataSet)
  
}