read_elefan_csv <- function(csvFile) {
  a <- read.csv(csvFile)
  dataSet <- list()
  dataSet$sample.no <- seq(1, nrow(as.matrix(a[,-1])))
  dataSet$dates <- as.Date(colnames(a)[2:length(colnames(a))], "X%Y.%m.%d")
  dataSet$midLengths <- a[[1]]
  dataSet$catch <- as.matrix(a[,-1])
  return(dataSet)
}