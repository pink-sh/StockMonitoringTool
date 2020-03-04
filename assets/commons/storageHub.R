###########WORKSPACE INTERACTION FUNCTIONS####################
#Version 3.2: changed functions for Storage Hub Interaction and VRE folders managed
# GoogleVIS is very verbose
suppressPackageStartupMessages(library(googleVis))

#list folder elements
library(httr)
library(XML)
require(XML)
require("httr")
library(jsonlite)
library(stringr)
library(pracma)



getStoragehubEP <- function (username, token) {
  print(paste(sep=" ", "Username:",username,"Token:",token))
  urlString = paste("http://registry.d4science.org/icproxy/gcube/service/GCoreEndpoint/DataAccess/StorageHub?gcube-token=",token,sep="")
  #urlString = paste("http://registry.d5science.org/icproxy/gcube/service/GCoreEndpoint/DataAccess/StorageHub?gcube-token=",token,sep="")
  
  got<-GET(urlString,authenticate(username,token), timeout(1*3600))
  xmlfile <- xmlTreeParse(got)
  class(xmlfile)
  xmltop = xmlRoot(xmlfile)
  
  listxpath = xpathSApply(xmltop, "//Results/Result")
  print(listxpath)
  listx<-(rapply(listxpath, function(x) head(x, 1)))
  if (length(listx)>0){
    storagehubEP<-as.character(listx[which(listx=="Endpoint")[[1]]+3])
    cat("StorageHub EP",storagehubEP,"\n")
    return (storagehubEP)
    
  }else{
    cat("Found no storage ub endpoint\n")
    return (NULL)
  }
}

getRootInfoUrl <- function (username, token) {
  return (paste(sep="",getStoragehubEP(username,token),"?exclude=hl:accounting&gcube-token=",token))
}

getPublicUrlReq <- function (username, token) {
  return (paste(sep="",getStoragehubEP(username,token),"/items/#ID#/publiclink?exclude=hl:accounting&gcube-token=",token))
}

getDeleteFileReq <- function (username, token) {
  return (paste(sep="",getStoragehubEP(username,token),"/items/#ID#?exclude=hl:accounting&gcube-token=",token))
}

getListElementsUrl <- function (username, token) {
  return (paste(sep="",getStoragehubEP(username,token),"/items/#ID#/children?exclude=hl:accounting&gcube-token=",token))
}

getDownloadWSElementUrl <- function (username, token) {
  return (paste(sep="",getStoragehubEP(username,token),"/items/#ID#/download?exclude=hl:accounting&gcube-token=",token))
}

getWSFolderReq <- function (username, token) {
  return (paste(sep="",getStoragehubEP(username,token),"/vrefolder?exclude=hl:accounting&gcube-token=",token))
}

getCreateFolder <- function(username, token) {
  return (paste(sep="",getStoragehubEP(username,token),"/items/#ID#/create/FOLDER?gcube-token=",token))
}

getWSRootID<-function(username,token){
  document <- fromJSON(txt=getRootInfoUrl(username,token),authenticate(username,token), timeout(1*3600))
  return(document$item$id)
}

getWSElementID<-function(username,token,parentFolderID,folderPath){
  
  document <- fromJSON(txt=gsub("#ID#", parentFolderID, getListElementsUrl(username,token)),authenticate(username,token), timeout(1*3600))
  wsFolderID<-""
  for (el in document$itemlist){
    cat("->",el$path,"\n")
    if (el$path == folderPath){
      wsFolderID<-el$id
      break
    }
  }
  return(wsFolderID)
}


getWSElementIDRelativePath<-function(username,token,parentFolderID,folderPath){
  
  document <- fromJSON(txt=gsub("#ID#", parentFolderID, getListElementsUrl(username,token)),authenticate(username,token), timeout(1*3600))
  wsFolderID<-""
  for (el in document$itemlist){
    cat("->",el$path,"\n")
    if (endsWith(el$path, paste0("/",folderPath))){
      wsFolderID<-el$id
      break
    }
  }
  return(wsFolderID)
}

getWSElementIDVRE<-function(username,token,parentFolderID,folderPath){
  
  document <- fromJSON(txt=gsub("#ID#", parentFolderID, getListElementsUrl(username,token)),authenticate(username,token), timeout(1*3600))
  wsFolderID<-""
  for (el in document$itemlist){
    cat("->",el$path,"\n")
    #if (el$path == folderPath){
    if (grepl(paste0("/",folderPath),el$path) == T){
      wsFolderID<-el$id
      break
    }
  }
  return(wsFolderID)
}



getWSElementIDinVREFolderFromRelativePath<-function(username,token,folderPath){
  
  allSubElements<-str_split(string=folderPath,pattern="/")
  allSubElement<-allSubElements[[1]]

  parentId<- getVREFolderID(username,token)

  
  if (!grepl("/",folderPath))
    return(getWSElementIDRelativePath(username = username, token = token, parentFolderID = parentId,folderPath = folderPath));
  parentId<- getWSElementIDRelativePath(username,token,parentId,allSubElement[1])
  pathWS<-paste(sep="",allSubElement[1])
  
  for (elIdx in 2:length(allSubElement)){
    el<-allSubElement[elIdx]
    if (nchar(el)>0){
      pathWS<-paste(sep="",pathWS,"/",el)
      parentId<-getWSElementIDRelativePath(username=username, token=token, parentFolderID = parentId,folderPath = pathWS)
      cat("ID",parentId,"\n")
    }
  }
  
  return(parentId)
  
}


searchWSFolderID<-function(username,token,folderPath){
  if (folderPath==paste("/Home/",username,"/Workspace",sep="") || folderPath==paste("/Home/",username,"/Workspace/",sep="")){
    return(getWSRootID(username, token))
  }
  location<-str_locate_all(pattern ='/Workspace/', folderPath)
  rootString<-str_sub(start = 1, end=as.numeric(location[[1]][1,2])-1,string=folderPath)
  subElements<-str_sub(start = as.numeric(location[[1]][1,2])+1,string=folderPath)
  allSubElements<-str_split(string=subElements,pattern="/")
  allSubElement<-allSubElements[[1]]
  parentId<-getWSRootID(username, token)
  pathWS<-paste(sep="",rootString)
  for (elIdx in 1:length(allSubElement)){
    el<-allSubElement[elIdx]
    if (nchar(el)>0){
      pathWS<-paste(sep="",pathWS,"/",el)
      cat("Getting ID of",pathWS,"\n")
      parentId<-getWSElementID(username=username, token=token,parentFolderID = parentId,folderPath = pathWS)
      cat("ID",parentId,"\n")
    }
  }
  return(parentId)
}

getCredentials<-function(username,token){
  if(exists("token") && (nchar(token) > 0) && exists("username") &&  (nchar(username)>0) && exists("storagehubEP") && (nchar(storagehubEP)>0)){
    return()
  } else{
    if (!(exists("token") && (nchar(token) > 0) && exists("username") &&  (nchar(username)>0))){
      configfile<-"userconfig.csv"
      configfiledm<-"globalvariables.csv"
      configexists<-F
      if (file.exists(configfile)){
        csvfile<-read.csv(configfile,check.names=FALSE,sep=";")
        if (length(csvfile)>8){
          username<<-dimnames(csvfile[9])[[2]]; 
          token<<-dimnames(csvfile[10])[[2]];
          configexists<-T
        }
      }else if (file.exists(configfiledm)){
        csvfile<-read.csv(configfiledm,check.names=FALSE,sep=",")
        if (length(csvfile)>1){
          username<<-as.character(csvfile[1,2])
          token<<-as.character(csvfile[3,2])
          configexists<-T
        }
      }
    }
  }
  discoverStorageHub()
  getWSRootID()  
}


corrFolder<-function(folder){
  if (substr(folder,nchar(folder),nchar(folder))=="/"){return(substr(folder,0,nchar(folder)-1))}
  else{return(folder)}
}

uuidGen<-function(){
  baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
  
  uuid<-paste(
    substr(baseuuid,1,8),
    "-",
    substr(baseuuid,9,12),
    "-",
    "4",
    substr(baseuuid,13,15),
    "-",
    sample(c("8","9","a","b"),1),
    substr(baseuuid,16,18),
    "-",
    substr(baseuuid,19,30),
    sep="",
    collapse=""
  )
  
  return(uuid)
}

listHomeWS<-function(username,token){
 # getCredentials(username,token)
  home=paste("/Home/",username,"/Workspace",sep="")
  homeFolders<-listWS(username,token,home)
}

listWS<-function(username,token,path){
# getCredentials()
  wsid<-searchWSFolderID(username,token,path)
  document <- fromJSON(txt=gsub("#ID#", wsid, getListElementsUrl(username,token)),authenticate(username,token), timeout(1*3600))
  wsFolderID<-""
  folders<-c()

  for (el in document$itemlist){
    #cat("->",el$path,"\n")
    folders<-c(folders,el$path)
  }
  return(folders)
}

fileFolderExistsInPath <- function(username, token, path, entity) {
  folders <- listWS(username, token, path)

  if (!endsWith(path,"/")) {
    path<-paste0(path,"/")
  }

  hasEntity <- FALSE
  fullPath <- paste0(path,entity)
  for (e in folders) {
    if (strcmp(e,fullPath)) {
      hasEntity <- TRUE
    }
  }
  return (hasEntity)
} 

deleteWS<-function(username,token,file){
 # getCredentials()
  wsid<-searchWSFolderID(username,token,file)
  document <- DELETE(url = gsub("#ID#", wsid, getDeleteFileReq(username,token)),authenticate(username,token), timeout(1*3600))
  if (document$status_code==200){
    cat("Delete OK\n")
    return(T)
  }else {cat("Delete KO\n"); return(F)}
}

getPublicFileLinkWS<-function(username,token,remotefile){
  #getCredentials()
  wsid<-searchWSFolderID(username,token,remotefile)
  document <- fromJSON(txt=gsub("#ID#", wsid, getPublicUrlReq(username,token)),authenticate(username,token), timeout(1*3600))
  link<-as.character(document)
  link<-str_replace(link,"https","http")
  return(link)
}


getPublicFileLinkVREFolder<-function(username,token,remotefile){
  #getCredentials()
  #wsid<-getVREFolderID()
  #wsid<-getWSElementIDVRE(wsid, remotefile)
  wsid<-getWSElementIDinVREFolderFromRelativePath(remotefile)
  document <- fromJSON(txt=gsub("#ID#", wsid, getPublicUrlReq(username,token)),authenticate(username,token), timeout(1*3600))
  link<-as.character(document)
  link<-str_replace(link,"https","http")
  return(link)
}

getVREFolderID<-function(username,token){
  #getCredentials()
  document <- fromJSON(txt=getWSFolderReq(username,token),authenticate(username,token), timeout(1*3600))
  vrefid <- as.character(document$item$id) #as.character(document$item$users$map[username])
  return(vrefid)
}

downloadWS<-function(username,token,element,isfile){
  #getCredentials()
  cat("About to download",element,"\n")
  g <- regexpr("/[^/]*$", element)
  
  
  if (!isfile){
    
    filename<-paste(uuidGen(),".zip",sep="")
    cat("The entire folder will be downloaded to",filename,"\n")
    
  } else{filename<-substring(element,g[1]+1,nchar(element))}
  
  cat("Downloading...\n")
  
  if (isfile){
    urlToD<-getPublicFileLinkWS(username,token,element)
    urlToD<-str_replace(urlToD,"https","http")
    cat("Downloading from public url",urlToD,"to",filename,"\n")
    download.file(url = urlToD, destfile = filename, method="wget", quiet = T, mode = "w",cacheOK = FALSE)
  }else{
    wsid<-searchWSFolderID(username,token,element)
    urltodwn<-gsub("#ID#", wsid, getDownloadWSElementUrl(username,token))
    urltodwn<-str_replace(urltodwn,"https","http")
    urltodwn<-str_replace(urltodwn,":443","")
    cat("Downloading url",urltodwn,"\n")
    command = paste(sep="",'wget --no-check-certificate ',urltodwn," -O ",filename)
    exitstat<-system(command,intern=T)
    print(exitstat)
    #download.file(url = urltodwn, destfile = filename, method="wget", quiet = T, mode = "w",cacheOK = FALSE)
    
    return(filename)
  }
  cat("All done.\n")
}



downloadFromVREFolder<-function(username,token,element){
  #getCredentials()
  cat("About to download",element,"\n")
  g <- regexpr("/[^/]*$", element)
  
  filename<-substring(element,g[1]+1,nchar(element))
  
  cat("Downloading...\n")
  urlToD<-getPublicFileLinkVREFolder(username,token,element) #getPublicFileLinkVREFolder(element)
  urlToD<-str_replace(urlToD,"https","http")
  cat("Downloading from public url",urlToD,"to",filename,"\n")
  download.file(url = urlToD, destfile = filename, method="wget", quiet = T, mode = "w",cacheOK = FALSE)
  
  cat("All done.\n")
}

downloadFromVREFolder<-function(username,token,element){
  #getCredentials()
  cat("About to download",element,"\n")
  g <- regexpr("/[^/]*$", element)
  
  filename<-substring(element,g[1]+1,nchar(element))
  
  cat("Downloading...\n")
  urlToD<-getPublicFileLinkVREFolder(element)
  urlToD<-str_replace(urlToD,"https","http")
  cat("Downloading from public url",urlToD,"to",filename,"\n")
  download.file(url = urlToD, destfile = filename, method="wget", quiet = T, mode = "w",cacheOK = FALSE)
  
  cat("All done.\n")
}

downloadFileWS<-function(username,token,element){
  downloadWS(username,token,element,T)
}

downloadFolderWS<-function(username,token,element){
  downloadWS(username,token,element,F)
}


uploadWS<-function(username,token,path,file,overwrite){
  uploadWSManager(username,token,path,file,overwrite,F)
}


uploadToVREFolder<-function(username,token,relativePath, file,overwrite,archive){
  result = tryCatch({
    #getCredentials()
    description = basename(file)
    name = basename(file)
    mimetype=""
    
    pathID<-getVREFolderID(username,token)
    
    if (nchar(relativePath)>0){
      if (!endsWith(relativePath,"/"))
      {
        relativePath<-paste0(relativePath,"/")
      }
      pathID <- searchWSFolderID(username,token,relativePath)
    }
    absolutefile <- tools:::file_path_as_absolute(file)
    wdfile<- paste(getwd(),"/",basename(file),sep="")
    localfile <- absolutefile#basename(file)
    
    cat("Uploading",file,"to",pathID,"\n")
    
    if (!archive){
      command = paste(sep="",'curl -s -F "name=', name,'" ',
                      '-F "description=empty" ',
                      '-F "file=@',absolutefile,'" ',
                      getStoragehubEP(username,token), "/items/",pathID,'/create/FILE?gcube-token=',token)
    }else{
      command = paste(sep="",'curl -s -F "parentFolderName=', name,'" ',
                      '-F "file=@',absolutefile,'" ',
                      getStoragehubEP(username,token), "/items/",pathID,'/create/ARCHIVE?gcube-token=',token)
    }
  
    
    fileID<-system(command,intern=T)
    output<-F
    if (length(fileID)==1)
      output<-T
    
    print(paste0(file, " uploaded to the VRE folder"))
    return(output)
  }, error = function(err) {
    print(paste0("Error uploading file to VRE folder ",err))
    return(NULL)
  },
  finally = {
    
  })
}

createFolderWs<-function(username,token,inFolder,folderName,folderDescription) {

  wsid<-searchWSFolderID(username,token,inFolder)
  #curl -X POST -H "Content-Type: application/x-www-form-urlencoded" -d 'name=StockMonitoringTool&description=something sss&hidden=false' https://workspace-repository.d4science.org:443/storagehub/workspace/items/5f74b772-d184-4df0-8485-fc9e09cc5612/create/FOLDER?gcube-token=a45a70b4-885b-4e95-879a-8ea0fd5f041c-843339462
  command = paste(sep="",'curl -X POST -H "Content-Type: application/x-www-form-urlencoded" -d \'',paste0('name=',folderName,'&description=StockMonitoringTool SDG 14.4.1 results&hidden=false'),'\' ',
                    gsub("#ID#", wsid, getCreateFolder(username,token)))
  fileID<-system(command,intern=T)
  output<-F
  if (length(fileID)==1)
    output<-T
  
  cat("All done.\n")
  return(output)


}


uploadWSManager<-function(username,token,path,file,overwrite,archive){
  #getCredentials()
  description = basename(file)
  name = basename(file)
  mimetype=""
  path<-corrFolder(path)
  pathID<-searchWSFolderID(username,token,path)
  
  absolutefile <- tools:::file_path_as_absolute(file)
  wdfile<- paste(getwd(),"/",basename(file),sep="")
  localfile <- absolutefile#basename(file)
  
  cat("Uploading",file,"to",path,"\n")
  
  if (!archive){
    command = paste(sep="",'curl -s -F "name=', name,'" ',
                    '-F "description=empty" ',
                    '-F "file=@',absolutefile,'" ',
                    getStoragehubEP(username,token), "/items/",pathID,'/create/FILE?gcube-token=',token)
  }else{
    command = paste(sep="",'curl -s -F "parentFolderName=', name,'" ',
                    '-F "file=@',absolutefile,'" ',
                    getStoragehubEP(username,token), "/items/",pathID,'/create/ARCHIVE?gcube-token=',token)
  }
  
  fileID<-system(command,intern=T)
  print(fileID)
  output<-F
  if (length(fileID)==1)
    output<-T
  
  cat("All done.\n")
  return(output)
}

uploadAllWS<-function(username,token,path){
  overwrite=T
  home=corrFolder(paste("/Home/",username,"/Workspace",sep=""))
  pathverification = corrFolder(path)
  if (pathverification==home){
    cat("Cannot upload to the Home folder directly\n")
    return()
  }
  #getCredentials()
  zipfile<-"all.zip"
  
  if (file.exists(zipfile)){cat("Cleaning...\n"); file.remove(zipfile)}
  cat("Preparing workspace package\n")
  
  wd<-username
  exclude<-paste("-x ",wd,"/.**\\* ",wd,"/R/**\\*",sep="")
  zip <- paste("cd .. && zip -r ",wd,"/all.zip ",wd,"/ ",exclude,sep="")
  system(zip, intern = FALSE)
  
  returns<-F
  if (file.exists(zipfile)){
    cat("Ready to upload to",path,"\n")
    q<-uploadWSManager(username,token,path,zipfile,overwrite,T)
    cat("Cleaning...\n")
    if (file.exists(zipfile)){file.remove(zipfile)}
    cat("All done.\n")
    returns<-q
  }else{cat("Error - could not create zip package\n")}
  return(returns)
}


###########END - WORKSPACE INTERACTION FUNCTIONS####################