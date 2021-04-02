
library (lubridate)
formatTimestamp <- function(ts) {
    return(format(as.Date(as.POSIXct(ts, origin="1970-01-01")), format="%Y-%m-%d"))
}

validateElefanInputFile <- function(file){

    ## read in file
    inputData <- try(read.csv(file, sep=",", dec="."), silent=TRUE)

    ## Major error, e.g. no csv file?
    check_csv <- ifelse(!inherits(inputData, "try-error"), TRUE, FALSE)

    if(check_csv){

        ## Does data include several columns and correct delimiter
        check_delimiter <- ifelse(ncol(inputData) > 1, TRUE, FALSE)

        ## try if different delimiter solves the issue
        if(!check_delimiter){
            inputData <- read.csv(file, sep=";", dec=".")
            check_delimiter <- ifelse(ncol(inputData) > 1, TRUE, FALSE)
        }
        if(!check_delimiter){
            inputData <- read.csv(file, sep="\t", dec=".")
            check_delimiter <- ifelse(ncol(inputData) > 1, TRUE, FALSE)
        }

        ## Is the first column numeric? -> length classes
        check_lengths <- ifelse(is.numeric(inputData[,1]), TRUE, FALSE)

        ## Can date be read from column names?
        colnams <- as.vector(colnames(inputData))
        dates <- sapply(colnams[2:length(colnams)], function(x){
            x <- ifelse(startsWith(x, 'X'), substring(x, 2), x)
            formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))
        })
        inputData <- cbind(inputData[,1], inputData[,which(!is.na(dates))+1])
        check_dates <- ifelse(any(!is.na(dates)), TRUE, FALSE)

        ## Only use numeric columns + Are there sufficient numeric samples?
        inputData <- inputData[,sapply(inputData, is.numeric)]
        check_ncols <- ifelse(!is.null(ncol(inputData)) && ncol(inputData) > 2, TRUE, FALSE)

    }else{
        check_delimiter <- FALSE
        check_lengths <- FALSE
        check_dates <- FALSE
        check_ncols <- FALSE
    }

    ## return checks
    checks <- list(csv=check_csv, delimiter=check_delimiter, lengths=check_lengths,
                   dates=check_dates, ncols=check_ncols)
    return(list(inputData=inputData, checks=checks))
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

    ## read in and validate csv file
    dataset <- validateElefanInputFile(csvFile)
    inputData <- dataset$inputData

    if(all(unlist(dataset$checks))){

        ## lfq list
        dataset$lfq <- list()

        ## sample number
        dataset$lfq$sample.no <- seq(1, nrow(as.matrix(inputData[,-1])))

        ## dates
        colnams <- colnames(inputData)[-1]
        colnams <- sapply(colnams, function(x) ifelse(startsWith(x, 'X'), substring(x, 2), x))
        dates <- sapply(colnams,
                        function(x)
                            formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy'))))
        dates <- as.Date(dates, "%Y-%m-%d")
        dataset$lfq$dates <- sort(dates)

        ## length classes
        dataset$lfq$midLengths <- inputData[,1]

        ## catch matrix
        dataset$lfq$catch <- as.matrix(inputData[,-1])[,order(dates)]
        colnames(dataset$lfq$catch) <- sort(dates)
    }

    return(dataset)
}
