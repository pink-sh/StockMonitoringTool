source("./assets/commons/storageHub.R")

username <<- "xxxxxxx" #Your username
token <<- "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx-xxxxxxxxx" #Your token

uploadWS(
  path = "/Home/xxxxxxx/Workspace/StockMonitoringTools/", #Your upload path
  file = "/tmp/ElefanGA_report_20200428_1218_1588069106.pdf", #Your file to upload
  overwrite = TRUE
)