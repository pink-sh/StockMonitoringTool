###########WORKSPACE INTERACTION FUNCTIONS####################
#Version 3.3: fixed get public link for shared folders
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

discoverStorageHub<-function(){
  
  urlString = paste("http://registry.d4science.org/icproxy/gcube/service/GCoreEndpoint/DataAccess/StorageHub?gcube-token=",token,sep="")
  got<-GET(urlString,authenticate(username,token), timeout(1*3600))
  xmlfile <- xmlTreeParse(got)
  class(xmlfile)
  xmltop = xmlRoot(xmlfile)
  
  listxpath = xpathSApply(xmltop, "//Results/Result")
  print(listxpath)
  listx<-(rapply(listxpath, function(x) head(x, 1)))
  if (length(listx)>0){
    storagehubEP<<-as.character(listx[which(listx=="Endpoint")[[1]]+3])
    rootInfoUrl<<-paste(sep="",storagehubEP,"?exclude=hl:accounting&gcube-token=",token);
    publicUrlReq<<-paste(sep="",storagehubEP,"/items/#ID#/publiclink?exclude=hl:accounting&gcube-token=",token);
    deleteFileReq<<-paste(sep="",storagehubEP,"/items/#ID#?exclude=hl:accounting&gcube-token=",token);
    listElementsUrl<<-paste(sep="",storagehubEP,"/items/#ID#/children?exclude=hl:accounting&gcube-token=",token);
    downloadWSElementUrl<<-paste(sep="",storagehubEP,"/items/#ID#/download?exclude=hl:accounting&gcube-token=",token);
    WSFolderReq<<-paste(sep="",storagehubEP,"/vrefolder?exclude=hl:accounting&gcube-token=",token);
    cat("StorageHub EP",storagehubEP,"\n")
  }else{
    cat("Found no storage ub endpoint\n")
  }
  
}

getWSRootID<-function(){
  document <- fromJSON(txt=rootInfoUrl,authenticate(username,token), timeout(1*3600))
  rootWSID<<-document$item$id
}

getWSElementID<-function(parentFolderID,folderPath){
  
  
  document <<- fromJSON(txt=gsub("#ID#", parentFolderID, listElementsUrl),authenticate(username,token), timeout(1*3600))
  wsFolderID<-""
  for (el in document$itemlist){
    cat("->",el$path,"\n")
    if (!startsWith(el$path,"/Share/")){
      if (el$path == folderPath){
        wsFolderID<-el$id
        break
      }
    }else{
      location<-str_locate_all(pattern ='/', folderPath)
      lastPart<-str_sub(start = as.numeric(location[[1]][dim(location[[1]])[1]])+1,string=folderPath)
      locationWS<-str_locate_all(pattern ='/', el$path)
      lastPartWS<-str_sub(start = as.numeric(locationWS[[1]][dim(locationWS[[1]])[1]])+1,string=el$path)
      if (lastPart == lastPartWS){
        wsFolderID<-el$id
        break
      }
    }
  }
  return(wsFolderID)
}


getWSElementIDRelativePath<-function(parentFolderID,folderPath){
  
  document <- fromJSON(txt=gsub("#ID#", parentFolderID, listElementsUrl),authenticate(username,token), timeout(1*3600))
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

getWSElementIDVRE<-function(parentFolderID,folderPath){
  
  document <- fromJSON(txt=gsub("#ID#", parentFolderID, listElementsUrl),authenticate(username,token), timeout(1*3600))
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



getWSElementIDinVREFolderFromRelativePath<-function(folderPath){
  
  allSubElements<-str_split(string=folderPath,pattern="/")
  allSubElement<-allSubElements[[1]]
  parentId<- getVREFolderID()
  
  if (!grepl("/",folderPath))
    return(getWSElementIDRelativePath(parentFolderID = parentId,folderPath = folderPath));
  
  parentId<- getWSElementIDRelativePath(parentId,allSubElement[1])
  
  pathWS<-paste(sep="",allSubElement[1])
  
  for (elIdx in 2:length(allSubElement)){
    el<-allSubElement[elIdx]
    if (nchar(el)>0){
      pathWS<-paste(sep="",pathWS,"/",el)
      cat("Getting ID of",pathWS,"\n")
      parentId<-getWSElementIDRelativePath(parentFolderID = parentId,folderPath = pathWS)
      cat("ID",parentId,"\n")
    }
  }
  
  return(parentId)
  
}


searchWSFolderID<-function(folderPath){
  if (folderPath==paste("/Home/",username,"/Workspace",sep="") || folderPath==paste("/Home/",username,"/Workspace/",sep="")){
    return(rootWSID)
  }
  location<-str_locate_all(pattern ='/Workspace/', folderPath)
  rootString<-str_sub(start = 1, end=as.numeric(location[[1]][1,2])-1,string=folderPath)
  subElements<-str_sub(start = as.numeric(location[[1]][1,2])+1,string=folderPath)
  allSubElements<-str_split(string=subElements,pattern="/")
  allSubElement<-allSubElements[[1]]
  parentId<-rootWSID
  pathWS<-paste(sep="",rootString)
  for (elIdx in 1:length(allSubElement)){
    el<-allSubElement[elIdx]
    if (nchar(el)>0){
      pathWS<-paste(sep="",pathWS,"/",el)
      cat("Getting ID of",pathWS,"\n")
      parentId<-getWSElementID(parentFolderID = parentId,folderPath = pathWS)
      cat("ID",parentId,"\n")
    }
  }
  return(parentId)
}

getCredentials<-function(){
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

listHomeWS<-function(){
  getCredentials()
  home=paste("/Home/",username,"/Workspace",sep="")
  homeFolders<-listWS(home)
}

listWS<-function(path){
  getCredentials()
  wsid<-searchWSFolderID(path)
  document <- fromJSON(txt=gsub("#ID#", wsid, listElementsUrl),authenticate(username,token), timeout(1*3600))
  wsFolderID<-""
  folders<-c()
  for (el in document$itemlist){
    cat("->",el$path,"\n")
    folders<-c(folders,el$path)
  }
  return(folders)
}

deleteWS<-function(file){
  getCredentials()
  wsid<-searchWSFolderID(file)
  document <- DELETE(url = gsub("#ID#", wsid, deleteFileReq),authenticate(username,token), timeout(1*3600))
  if (document$status_code==200){
    cat("Delete OK\n")
    return(T)
  }else {cat("Delete KO\n"); return(F)}
}

getPublicFileLinkWS<-function(remotefile){
  getCredentials()
  wsid<-searchWSFolderID(remotefile)
  document <- fromJSON(txt=gsub("#ID#", wsid, publicUrlReq),authenticate(username,token), timeout(1*3600))
  link<-as.character(document)
  link<-str_replace(link,"https","http")
  return(link)
}


getPublicFileLinkVREFolder<-function(remotefile){
  getCredentials()
  #wsid<-getVREFolderID()
  #wsid<-getWSElementIDVRE(wsid, remotefile)
  wsid<-getWSElementIDinVREFolderFromRelativePath(remotefile)
  document <- fromJSON(txt=gsub("#ID#", wsid, publicUrlReq),authenticate(username,token), timeout(1*3600))
  link<-as.character(document)
  link<-str_replace(link,"https","http")
  return(link)
}

getVREFolderID<-function(){
  getCredentials()
  document <- fromJSON(txt=WSFolderReq,authenticate(username,token), timeout(1*3600))
  vrefid <- as.character(document$item$id) #as.character(document$item$users$map[username])
  return(vrefid)
}

downloadWS<-function(element,isfile){
  getCredentials()
  cat("About to download",element,"\n")
  g <- regexpr("/[^/]*$", element)
  
  
  if (!isfile){
    
    filename<-paste(uuidGen(),".zip",sep="")
    cat("The entire folder will be downloaded to",filename,"\n")
    
  } else{filename<-substring(element,g[1]+1,nchar(element))}
  
  cat("Downloading...\n")
  
  if (isfile){
    urlToD<-getPublicFileLinkWS(element)
    urlToD<-str_replace(urlToD,"https","http")
    cat("Downloading from public url",urlToD,"to",filename,"\n")
    download.file(url = urlToD, destfile = filename, method="wget", quiet = T, mode = "w",cacheOK = FALSE)
  }else{
    wsid<-searchWSFolderID(element)
    urltodwn<-gsub("#ID#", wsid, downloadWSElementUrl)
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



downloadFromVREFolder<-function(element){
  getCredentials()
  cat("About to download",element,"\n")
  g <- regexpr("/[^/]*$", element)
  
  filename<-substring(element,g[1]+1,nchar(element))
  
  cat("Downloading...\n")
  urlToD<-getPublicFileLinkVREFolder(element) #getPublicFileLinkVREFolder(element)
  urlToD<-str_replace(urlToD,"https","http")
  cat("Downloading from public url",urlToD,"to",filename,"\n")
  download.file(url = urlToD, destfile = filename, method="wget", quiet = T, mode = "w",cacheOK = FALSE)
  
  cat("All done.\n")
}

downloadFromVREFolder<-function(element){
  getCredentials()
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

downloadFileWS<-function(element){
  downloadWS(element,T)
}

downloadFolderWS<-function(element){
  downloadWS(element,F)
}


uploadWS<-function(path,file,overwrite){
  uploadWSManager(path,file,overwrite,F)
}


uploadToVREFolder<-function(relativePath, file,overwrite,archive){
  getCredentials()
  description = basename(file)
  name = basename(file)
  mimetype=""
  
  pathID<-getVREFolderID()
  
  if (nchar(relativePath)>0){
    if (!endsWith(relativePath,"/"))
    {
      relativePath<-paste0(relativePath,"/")
    }
    pathID <- getWSElementIDinVREFolderFromRelativePath(relativePath)
  }
  absolutefile <- tools:::file_path_as_absolute(file)
  wdfile<- paste(getwd(),"/",basename(file),sep="")
  localfile <- absolutefile#basename(file)
  
  cat("Uploading",file,"to",pathID,"\n")
  
  if (!archive){
    command = paste(sep="",'curl -s -F "name=', name,'" ',
                    '-F "description=empty" ',
                    '-F "file=@',absolutefile,'" ',
                    storagehubEP, "/items/",pathID,'/create/FILE?gcube-token=',token)
  }else{
    command = paste(sep="",'curl -s -F "parentFolderName=', name,'" ',
                    '-F "file=@',absolutefile,'" ',
                    storagehubEP, "/items/",pathID,'/create/ARCHIVE?gcube-token=',token)
  }
  
  fileID<-system(command,intern=T)
  print(fileID)
  output<-F
  if (length(fileID)==1)
    output<-T
  
  cat("All done.\n")
  return(output)
}


uploadWSManager<-function(path,file,overwrite,archive){
  getCredentials()
  description = basename(file)
  name = basename(file)
  mimetype=""
  path<-corrFolder(path)
  pathID<-searchWSFolderID(path)
  
  absolutefile <- tools:::file_path_as_absolute(file)
  wdfile<- paste(getwd(),"/",basename(file),sep="")
  localfile <- absolutefile#basename(file)
  
  cat("Uploading",file,"to",path,"\n")
  
  if (!archive){
    command = paste(sep="",'curl -s --max-time 10 --retry 5 --retry-delay 0 --retry-max-time 40	-F "name=', name,'" ',
                    '-F "description=empty" ',
                    '-F "file=@',absolutefile,'" ',
                    storagehubEP, "/items/",pathID,'/create/FILE?gcube-token=',token)
  }else{
    command = paste(sep="",'curl -s --max-time 10 --retry 5 --retry-delay 0 --retry-max-time 40	-F "parentFolderName=', name,'" ',
                    '-F "file=@',absolutefile,'" ',
                    storagehubEP, "/items/",pathID,'/create/ARCHIVE?gcube-token=',token)
  }
  
  fileID<-system(command,intern=T)
  print(fileID)
  output<-F
  if (length(fileID)==1)
    output<-T
  
  cat("All done.\n")
  return(output)
}

uploadAllWS<-function(path){
  overwrite=T
  home=corrFolder(paste("/Home/",username,"/Workspace",sep=""))
  pathverification = corrFolder(path)
  if (pathverification==home){
    cat("Cannot upload to the Home folder directly\n")
    return()
  }
  getCredentials()
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
    q<-uploadWSManager(path,zipfile,overwrite,T)
    cat("Cleaning...\n")
    if (file.exists(zipfile)){file.remove(zipfile)}
    cat("All done.\n")
    returns<-q
  }else{cat("Error - could not create zip package\n")}
  return(returns)
}


###########END - WORKSPACE INTERACTION FUNCTIONS####################