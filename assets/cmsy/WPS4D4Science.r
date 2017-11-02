###VERSION 2.0###
options(warn=-1)
library(httr)
library(XML)
require("httr")

unsink <- function(){
  
  n<-sink.number()
  if (n>0){
  for (i in 1:n){
    sink()
  }
  }
}
  
######Get Capabilities######
getCapabilities <- function(wps_uri, username, token){
  tryCatch({
  sink("")
  #build the URL
  wpsService<-paste(wps_uri,"?Request=GetCapabilities&Service=WPS",sep="")
  #get the URL with user and token
  got<-GET(wpsService,authenticate(username,token), timeout(1*3600))
  #parse the tree
  xmlfile <- xmlTreeParse(got)
  class(xmlfile)
  #get the root of the tree
  xmltop = xmlRoot(xmlfile)
  
  #get the identifiers of the algorithms
  algorithmsList = xpathSApply(xmltop, "//ows:Identifier")
  algorithms<-(rapply(algorithmsList, function(x) head(x, 1)))
  algorithms<-algorithms[seq(3,length(algorithms),by=3)]
  #get the titles of the algorithms
  titlesList = xpathSApply(xmltop, "//ows:Title")
  titles<-(rapply(titlesList, function(x) head(x, 1)))
  titles<-titles[seq(6,length(titles),by=3)]
  
  #build the capabilities as a data frame
  capabilities<- data.frame(algorithms,titles,stringsAsFactors=F)
  colnames(capabilities) <- c("Identifier","Title")
  unsink()
  return(capabilities)
  
  },
   error = function(e) {
    unsink()
    print(e)
  }, interrupt = function(ex) {
    unsink()
    print(ex)
  },
  finally = {
    unsink()
  }
  ) # tryCatch()
}

#get the objects of the algorithms description: this function manages both inputs and outputs
getProcessObjectDescription <- function(wps_uri, username, token, process_id,is.input){
  tryCatch({
  sink("")
  #set the prefixes
  prefixRoot<-"Input"
  prefixComplexDataFinder<-"Input"
  prefixComplexData<-"ComplexData"
  LiteralData<-"LiteralData"
  if (!is.input){
    prefixRoot<-"Output"
    prefixComplexDataFinder<-"ProcessOutputs/Output"
    prefixComplexData<-"ComplexOutput"
    LiteralData<-"LiteralOutput"
  }
  
  #build the process description URL
  wpsService<-paste(wps_uri,"?Request=DescribeProcess&Service=WPS&Version=1.0.0&Identifier=",process_id,sep="")
  #parse the xml tree
  got<-GET(wpsService,authenticate(username,token),timeout(1*3600))
  xmlfile <- xmlTreeParse(got)
  class(xmlfile)
  xmltop = xmlRoot(xmlfile)
  
  #extract the titles the identifiers and the abstracts of the inputs/outputs
  titles = xpathSApply(xmltop, paste("//",prefixRoot,"/ows:Title",sep=""))
  identifiers = xpathSApply(xmltop, paste("//",prefixRoot,"/ows:Identifier",sep=""))
  abstracts = xpathSApply(xmltop, paste("//",prefixRoot,"/ows:Abstract",sep=""))
  titles<-(rapply(titles, function(x) head(x, 1)))
  titles<-titles[seq(3,length(titles),by=3)]
  identifiers<-(rapply(identifiers, function(x) head(x, 1)))
  identifiers<-identifiers[seq(3,length(identifiers),by=3)]
  abstracts<-(rapply(abstracts, function(x) head(x, 1)))
  abstracts<-abstracts[seq(3,length(abstracts),by=3)]
  #prepare the objects to collect information
  n<-length(identifiers)
  #meta object: 1st col=identifier, 2nd=wps-type, 3rd=mime-types, 4th=default value
  meta<- matrix(data=NA, nrow = n, ncol = 4)
  #a matrix recording the allowed values for each I/O
  allowedValuesMatrix<- list()
  
  for (i in 1:n){
    #take the literal values if any
    literal = xpathSApply(xmltop, paste("//",prefixRoot,"/ows:Identifier[text()='",identifiers[i],"']/../",LiteralData,"",sep=""))
    allowedvaluesarray<-NA
	#case 1: literal values
    if (length(literal)>0 && !is.null(literal[1])){
      literal<-as.array(rapply(literal, function(x) head(x, 1)))
	  # get type: LiteralInput/Output 
      type<-literal[1][[1]]
      if (type==LiteralData){
		#get mime-type
        mime<-literal[3][[1]]
        if (is.na(mime))
          mime<-"xs:string"
        meta[i,1]<-mime
		
		#get allowed values or record an AnyValue tag
        allowed<-literal[4][[1]]
        if (is.na(allowed))
          allowed<-"ows:AnyValue"
        meta[i,2]<-allowed
		
		#if there are allowed values, record them
        if (allowed=="ows:AllowedValues"){
		  #add the values to a list
          allowedvalues<-literal[7:length(literal)-3]
		  #skip metadata
          allowedvalues<-allowedvalues[seq(4,length(allowedvalues),by=3)]
          nv<-length(allowedvalues)
		  #from the list of objects create a much simpler vector to attach to the recorded input/output
          allowedvaluesarray<-array()
          for (j in 1:nv){
            allowedvaluesarray[j]<-allowedvalues[j][[1]]
          }
		  #get the default value
          default<- literal[length(literal)][[1]]
		  #record the value
          meta[i,3]<-default 
        }
		#manage the case of AnyValue: record only the default value
        else if (allowed=="ows:AnyValue"){
          meta[i,3]<-literal[7][[1]]
        }
    #add abstract to the meta
		      meta[i,4]<-titles[i]
      }
    }#end of literals management
    else{
	  #get complex data default mime-type
      complexDefault = xpathSApply(xmltop, paste("//",prefixComplexDataFinder,"/ows:Identifier[text()='",identifiers[i],"']/../",prefixComplexData,"/Default/Format/MimeType",sep=""))
	  #get complex data possible mime-types
      complex = xpathSApply(xmltop, paste("//",prefixComplexDataFinder,"/ows:Identifier[text()='",identifiers[i],"']/../",prefixComplexData,"/Supported",sep=""))
	  #manage a random bug of the xml parser: go into the format tag to get the supported formats
      if (length(complex)==2){
        complex = xpathSApply(xmltop, paste("//",prefixComplexDataFinder,"/ows:Identifier[text()='",identifiers[i],"']/../",prefixComplexData,"/Supported/Format",sep=""))
        complex<-as.array(rapply(complex, function(x) head(x, 1)))
        complex<-complex[seq(4,length(complex),by=4)]
      }
      else{
        complex<-as.array(rapply(complex, function(x) head(x, 1)))
        complex<-complex[seq(5,length(complex),by=4)]
      }
	  #end of bug management
	  #collect allowed values
      nv<-length(complex)
      allowedvaluesarray<-array()
      for (j in 1:nv){
        allowedvaluesarray[j]<-complex[j][[1]]
      }
      meta[i,1]<-"ComplexData"
      meta[i,2]<-"Supported"
      meta[i,3]<-as.character(complexDefault$children$text)[6]
	    meta[i,4]<-titles[i]
    }
	#feed the allowed values matrix for this I/O
    allowedValuesMatrix[[i]]<-allowedvaluesarray
  }
  
  #build a data frame
  iframe<- data.frame(identifiers,meta,stringsAsFactors=F)
  #add coluns labels
  colnames(iframe) <- c("Identifier","Type","AllowedValuesType","DefaultValue","Description")
  #add allowed values
  iframe$AllowedValues<-allowedValuesMatrix
  #delete the row.names
  row.names(iframe)<-NULL
  unsink()
  return(iframe)  
  },
  error = function(e) {
    unsink()
    print(e)
  }, interrupt = function(ex) {
    unsink()
    print(ex)
  },
  finally = {
    unsink()
  }
  ) # tryCatch()
}

#PROCESS INPUT DESCRIPTION
getProcessInputDescription <- function(wps_uri, username, token, process_id){
  return (getProcessObjectDescription(wps_uri, username, token, process_id,is.input=T))
}

#PROCESS OUTPUT DESCRIPTION
getProcessOutputDescription <- function(wps_uri, username, token, process_id){
  return (getProcessObjectDescription(wps_uri, username, token, process_id,is.input=F))
}

#OUTPUT RETRIEVAL
getOutput <- function(wps_uri, username, token, process_id,keys,values)
{
  tryCatch({
  sink("")
  #go through the keys and values and build the URL of the process
  n<-length(keys)
  inputs<-""
  for (i in 1:n){
    inputs<-paste(inputs,keys[i],"=",values[i],sep="")
    if (i<n)
      inputs<-paste(inputs,";",sep="")
  }
  inputs<-URLencode(inputs)
  #process URL building
  wpsService<-paste(wps_uri,"?request=Execute&service=WPS&Version=1.0.0&lang=en-US&Identifier=",process_id,"&DataInputs=",inputs,sep="")
  #output retrieval and xml parsing
  got<-GET(wpsService,authenticate(username,token), timeout(20*3600))
  xmlfile <- xmlTreeParse(got)
  class(xmlfile)
  xmltop = xmlRoot(xmlfile)
  
  #get output identifiers
  outputIds = xpathSApply(xmltop, "//wps:Output/ows:Identifier")
  outputIds<-(rapply(outputIds, function(x) head(x, 1)))
  outputIds<-outputIds[seq(3,length(outputIds),by=3)]
  #get output titles
  outputTitles = xpathSApply(xmltop, "//wps:Output/ows:Title")
  outputTitles<-(rapply(outputTitles, function(x) head(x, 1)))
  outputTitles<-outputTitles[seq(3,length(outputTitles),by=3)]
  #prepare output objects
  n<-length(outputIds)
  #resultList structure: identifier,title,mime-type,payload
  resultList<- list()
  k<-1
  for (i in 1:n){
	#get all information for the i-th output
    results = xpathSApply(xmltop, paste("//wps:Output/ows:Identifier[text()='",outputIds[i][[1]],"']/../wps:Data",sep=""))
    results<-(rapply(results, function(x) head(x, 1)))
	#type= complex or simple data
    type<-results[2][[1]]
	#case of complex data
    if (type=="wps:ComplexData"){
	  nr<-length(results)
	  #case of multi object: an output containing multiple outputs 
      if (nr>7){
		#record each sub-ojbect
        for (j in seq(7, nr, by = 15)){
          single_result<- results[j][[1]]
		  #stop when gml is met
          if (single_result=="gml")
            break
		  #result parsing
          result_name<-results[j+1][[1]]
          result_value<-results[j+4][[1]]
          result_description<-results[j+8][[1]]
          result_mimetype<-results[j+12][[1]]
		  #add a list to the results matrix
          resultList[[k]]<-c(result_name,result_description,result_mimetype,result_value)
          k<-k+1
        }
      }
      else{
		#record the simple-complex output
        result_name<-outputIds[i][[1]]
        result_value<-results[5][[1]]
        result_description<-outputTitles[i][[1]]
        result_mimetype<-results[3][[1]]
		#add a list to the results matrix
        resultList[[k]]<-c(result_name,result_description,result_mimetype,result_value)
        k<-k+1
      }
    }#end complex data management
    else if(type=="wps:LiteralData"){
	  #manage literal data
      result_name<-outputIds[i][[1]]
      result_value<-results[5][[1]]
      result_description<-outputTitles[i][[1]]
      result_mimetype<-results[3][[1]]
	  #add a list to the results matrix
      resultList[[k]]<-c(result_name,result_description,result_mimetype,result_value)
      k<-k+1
    }
  }
  #build a data frame
  resultsframe<- data.frame(resultList,stringsAsFactors=F)
  resultsframe<-t(resultsframe)
  #delete the row names columns
  row.names(resultsframe)<-NULL
  #add names to the columns
  colnames(resultsframe) <- c("Identifier","Title","Type","Value")
  unsink()
  return(resultsframe)
  },
  error = function(e) {
    unsink()
    print(e)
  }, interrupt = function(ex) {
    unsink()
    print(ex)
  },
  finally = {
    unsink()
  }
  ) # tryCatch()
}


#OUTPUT RETRIEVAL
parseResponse <- function(response)
{
  tryCatch({
    sink("")
    
    got<-response
    xmlfile <- xmlTreeParse(got,encoding = "UTF-8")
    class(xmlfile)
    xmltop = xmlRoot(xmlfile)
    
    #get output identifiers
    outputIds = xpathSApply(xmltop, "//wps:Output/ows:Identifier")
    outputIds<-(rapply(outputIds, function(x) head(x, 1)))
    outputIds<-outputIds[seq(3,length(outputIds),by=3)]
    #get output titles
    outputTitles = xpathSApply(xmltop, "//wps:Output/ows:Title")
    outputTitles<-(rapply(outputTitles, function(x) head(x, 1)))
    outputTitles<-outputTitles[seq(3,length(outputTitles),by=3)]
    #prepare output objects
    n<-length(outputIds)
    #resultList structure: identifier,title,mime-type,payload
    resultList<- list()
    k<-1
    for (i in 1:n){
      #get all information for the i-th output
      results = xpathSApply(xmltop, paste("//wps:Output/ows:Identifier[text()='",outputIds[i][[1]],"']/../wps:Data",sep=""))
      results<-(rapply(results, function(x) head(x, 1)))
      #type= complex or simple data
      type<-results[2][[1]]
      #case of complex data
      if (type=="wps:ComplexData"){
        nr<-length(results)
        #case of multi object: an output containing multiple outputs 
        if (nr>7){
          #record each sub-ojbect
          for (j in seq(7, nr, by = 15)){
            single_result<- results[j][[1]]
            #stop when gml is met
            if (single_result=="gml")
              break
            #result parsing
            result_name<-results[j+1][[1]]
            result_value<-results[j+4][[1]]
            result_description<-results[j+8][[1]]
            result_mimetype<-results[j+12][[1]]
            #add a list to the results matrix
            resultList[[k]]<-c(result_name,result_description,result_mimetype,result_value)
            k<-k+1
          }
        }
        else{
          #record the simple-complex output
          result_name<-outputIds[i][[1]]
          result_value<-results[5][[1]]
          result_description<-outputTitles[i][[1]]
          result_mimetype<-results[3][[1]]
          #add a list to the results matrix
          resultList[[k]]<-c(result_name,result_description,result_mimetype,result_value)
          k<-k+1
        }
      }#end complex data management
      else if(type=="wps:LiteralData"){
        #manage literal data
        result_name<-outputIds[i][[1]]
        result_value<-results[5][[1]]
        result_description<-outputTitles[i][[1]]
        result_mimetype<-results[3][[1]]
        #add a list to the results matrix
        resultList[[k]]<-c(result_name,result_description,result_mimetype,result_value)
        k<-k+1
      }
    }
    #build a data frame
    resultsframe<- data.frame(resultList,stringsAsFactors=F)
    resultsframe<-t(resultsframe)
    #delete the row names columns
    row.names(resultsframe)<-NULL
    #add names to the columns
    colnames(resultsframe) <- c("Identifier","Title","Type","Value")
    unsink()
    return(resultsframe)
  },
  error = function(e) {
    unsink()
    print(e)
  }, interrupt = function(ex) {
    unsink()
    print(ex)
  },
  finally = {
    unsink()
  }
  ) # tryCatch()
}


xmeansD4s<-function(wps_uri,username,token,data,features,maxiterations,minclusters,maxclusters,minpoints){

  options(digits.secs=3)
  
  dffile<-paste("xmeans_data_",Sys.time(),".csv",sep="")
  dffile<-gsub(":", "_", dffile)
  dffile<-gsub(" ", "_", dffile)
  write.csv(data,file=dffile, quote = FALSE, eol = "\n", row.names = FALSE,  fileEncoding = "UTF-8")
    
  templateFile="TemplateXMeansRequest.xml";
  featuresString<-paste(features, sep="|", collapse="|")
    
  sentfile=paste("xmeans_req_",Sys.time(),".xml",sep="")
  #sentfile=paste("xmeans_req_",".xml",sep="")
  sentfile<-gsub(":", "_", sentfile)
  sentfile<-gsub(" ", "_", sentfile)
  
  filexml<-readChar(templateFile, file.info(templateFile)$size)
  filexml<-gsub("\r\n", "\n", filexml)
  
  body<-readChar(dffile, file.info(dffile)$size)
  body<-gsub("\r\n", "\n", body)
  body<-gsub("\n$", "", body)
  
  
  filexml<-gsub("#FILE#", body, filexml)
  filexml<-gsub("#FEATURES#", featuresString, filexml)
  filexml<-gsub("#MAXITERATIONS#", maxiterations, filexml)
  filexml<-gsub("#MINCLUSTERS#", minclusters, filexml)
  filexml<-gsub("#MAXCLUSTERS#", maxclusters, filexml)
  filexml<-gsub("#MINPOINTS#", minpoints, filexml)
  
  filehandle <- file(filexml)
  write(filexml, file = sentfile,append = FALSE, sep = "")
  close(filehandle)
  
  out<-POST(url = wps_uri, config=c(authenticate(username, token, type = "basic")),body = upload_file(sentfile, type="text/xml"),encode = c("multipart"), handle = NULL)
  
  wps.output<-parseResponse(response=out)
  
  closeAllConnections()
  
  outframe<-as.data.frame(wps.output)
  
  con <- textConnection(as.character(outframe$Value)[1])
  dataClustered <- read.csv(con)
  return (dataClustered)
}
