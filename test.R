source("/home/enrico/Work/StockMonitoringTool/assets/commons/storageHub.R")
uploadFolderName <- "StockMonitoringTool"
u<-"enrico.anello"
t<-"a45a70b4-885b-4e95-879a-8ea0fd5f041c-843339462"

if (fileFolderExistsInPath(u,t,paste0("/Home/",u,"/Workspace/"), uploadFolderName) == FALSE) {
  print("Creating folder")
  createFolderWs(
    u,t,
    paste0("/Home/",u,"/Workspace/"),
    uploadFolderName, 
    "Results of the SDG 14.4.1 Stock Monitoring Tool")
}

uploadToVREFolder(
  username = u, 
  token = t, 
  relativePath = paste0("/Home/",u,"/Workspace/", uploadFolderName, "/"), 
  file = "/tmp/ElefanGA_report_2019120416101575472239.pdf",
  overwrite = TRUE,
  archive = FALSE
)