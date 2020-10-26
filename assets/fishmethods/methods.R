validateFishMethodsFile <- function(file) {
  contents <- read.csv(file)
  delimiter<-if(ncol(contents)!=1){"ok"}else{"not ok"}
  decimal<-if(length(setdiff(names(contents),names(contents)[sapply(contents, is.numeric)]))==0){"point"}else{"not point"}
  
  validInputColumns <- c('age', 'ssbwgt', 'partial', 'pmat')
  if(delimiter!="ok") {
    return (list(checkDelim=delimiter,checkDec=NULL,checkName=NULL,contents=NULL))
  } else if (decimal!="point") {
    return (list(checkDelim=NULL,checkDec=decimal,checkName=NULL,contents=NULL))
  } else if(length(setdiff(names(contents),validInputColumns))!=0){
    return (list(checkDelim=NULL,checkDec=decimal,checkName="colname error",contents=NULL))
  }else{
    return (list(checkDelim=NULL,checkDec=NULL,checkName=NULL,contents=contents))
  } 
  return(list(checkDelim=NULL,checkDec=NULL,checkName=NULL,contents=contents))
}
 

ypr_shinyApp <-function(age=NULL,wgt=NULL,partial=NULL,M=NULL,plus=FALSE,oldest=NULL,maxF=2,incrF=0.001,graph=TRUE) {				
  res <- tryCatch({
    ypr(age,wgt,partial,M,plus,oldest,maxF,incrF,graph)
  },
  error=function(cond) {
    errorResult = list()
    errorResult[['error']] <- gettext(cond)
    return(errorResult)
  })
  return(res)
}


sbpr_shinyApp<-function(age=NULL,ssbwgt=NULL,partial=NULL,pmat=pmat,M=NULL,pF=NULL, pM=NULL,MSP=40,plus=FALSE,oldest=NULL,maxF=2,incrF=0.0001,graph=TRUE){				
  res <- tryCatch({
    sbpr(age,ssbwgt,partial,pmat,M,pF, pM,MSP,plus,oldest,maxF,incrF,graph)
  },
  error=function(cond) {
    errorResult = list()
    errorResult[['error']] <- gettext(cond)
    return(errorResult)
  })
  return(res)
}

round_df <- function(df, digits = 3) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}