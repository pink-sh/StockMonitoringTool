
library (lubridate)
formatTimestamp <- function(ts) {
  return(format(as.Date(as.POSIXct(ts, origin="1970-01-01")), format="%Y-%m-%d"))
}

validateElefanInputFile <- function(file) {
  contents <- read.csv(file)
  decimal<-if(length(setdiff(names(contents),names(contents)[sapply(contents, is.numeric)]))==0){"point"}else{"not point"}
  a<-as.vector(colnames(contents))
  #print(head(a,1))
  if (decimal!="point") {
    return (list(check_dec=decimal,check_name=NULL,contents=NULL))
  } else  if (head(a,1) != 'midLength') {
    return (list(check_dec=decimal,check_name="colname error",contents=NULL))
    
  } else {
    for (row in 2:length(a)) {
      x <- a[row]
      if (startsWith(x, 'X')) {
        x <- substring(x, 2)
      }
      d <- formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))
      if (is.na(d)) {
        return (list(check_dec=decimal,check_name=NULL,contents=contents))
      }
    }
  }
  return (list(check_dec=decimal,check_name=NULL,contents=contents))
}

read_elefan_csv <- function(csvFile, format="") {
  Sys.setlocale("LC_TIME", "C")
  
  print (paste0("Format is: ", format))
  
  if (is.null(format) || is.na(format)) {
    order <- c('ymd', 'ydm', 'dmy', 'mdy')
  } else if (format == "mdy") {
    order <- c('mdy')
  } else if (format == "dmy") {
    order <- c('dmy')
  } else if (format == 'ymd') {
    order = c('ymd', 'ydm')
  } else if (format == 'ydm') {
    order = c('ydm', 'ymd')
  } else {
    order <- c('ymd', 'ydm', 'dmy', 'mdy')
  }
  
  a <- validateElefanInputFile(csvFile)$contents
  check_dec<-validateElefanInputFile(csvFile)$check_dec
  check_name<-validateElefanInputFile(csvFile)$check_name
  if (is.null(a)) {
    return (list(checkDec=check_dec,checkName=check_name,catch=a))
  }
  
  dataSet <- list()
  dataSet$sample.no <- seq(1, nrow(as.matrix(a[,-1])))
  colname <- as.vector(unlist(lapply(colnames(a)[2:length(colnames(a))], substring, 2)))
  
  vectorDates <- as.vector(
    unlist(
      lapply(
        lapply(colname, parse_date_time, order )
        , formatTimestamp)
    )
  )
  dataSet$dates <- as.Date(vectorDates, "%Y-%m-%d")
  
  #dataSet$dates <- as.Date(colnames(a)[2:length(colnames(a))], "X%Y.%m.%d")
  dataSet$midLengths <- a[[1]]
  dataSet$catch <- as.matrix(a[,-1])
  
  colnames(dataSet$catch) <- vectorDates
  
  return (dataSet)
  
}