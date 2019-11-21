library (lubridate)

read_elefan_csv <- function(csvFile) {
  a <- read.csv(csvFile)
  dataSet <- list()
  dataSet$sample.no <- seq(1, nrow(as.matrix(a[,-1])))
  print(typeof(colnames(a)[2:length(colnames(a))]))
  print(colnames(a)[2:length(colnames(a))])
  colname <- as.vector(unlist(lapply(colnames(a)[2:length(colnames(a))], substring, 2)))
  print (typeof(colname))
  print (colname)
  timestamps <- as.vector(unlist(lapply(colname, parse_date_time, order=c('ymd', 'dmy', 'mdy') )))
  formattedDates <- as.vector(unlist(lapply(timestamps, lubridate::date)))
  print("printing formatted dates")
  print(formattedDates)
  #dataSet$dates <- as.Date(formattedDates, "X%Y.%m.%d")
  dataSet$dates <- as.Date(colnames(a)[2:length(colnames(a))], "X%Y.%m.%d")
  dataSet$midLengths <- a[[1]]
  dataSet$catch <- as.matrix(a[,-1])
  
  print (dataSet$dates)
  

  return(dataSet)
}