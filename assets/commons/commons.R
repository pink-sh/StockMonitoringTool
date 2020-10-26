library(httr)
library(jsonlite)
library(pracma)

########## CONSTANTS ##########

uploadFolderName <- "StockMonitoringTools"
uploadFolderDescription <- "SDG 14.4.1 VRE Stock Monitoring Tools results"
VREUploadText <- "The report has been uploaded to your VRE workspace"
gcubeTokenQueryParam <- "gcube-token"
apiUrl <- "https://api.d4science.org/rest/2/people/profile?gcube-token="
vreToken <- "96d4f92a-ef32-42ab-a47a-425ee5618644-843339462"


######## END CONSTANTS ########

##### COMMON JAVASCRIPT CODE #####
jscode <- "
shinyjs.showBox = function(boxid) {
$('#' + boxid).parent('div').css('visibility','visible');
}
shinyjs.removeBox = function(boxid) {
$('#' + boxid).parent('div').css('visibility','hidden');
}
shinyjs.disableAllButtons = function() {
$('.action-button').attr('disabled', true);
}
shinyjs.enableAllButtons = function() {
$('.action-button').attr('disabled', false);
}
shinyjs.showComputing = function() {
$('.loadingCustom').css('visibility','visible');
}
shinyjs.hideComputing = function() {
$('.loadingCustom').css('visibility','hidden');
}
"
##### END COMMON JAVASCRIPT CODE #####

app_load_spinner <- function(message = "") {
  html <- ""
  html <- paste0(html, "<div style=\"position: absolute; left: 50%;\">")
  html <- paste0(html, "<div style=\"color: white; width:150px; margin-left:-75px;\">", message, "</div>")
  html <- paste0(html, "</div>")
  html <- paste0(html, "<div class=\"sk-fading-circle\">")
  html <- paste0(html, "<div class=\"sk-circle1 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle2 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle3 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle4 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle5 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle6 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle7 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle8 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle9 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle10 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle11 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle12 sk-circle\"></div>")
  html <- paste0(html, "</div>")
  return (html)
}

getVREUsername <- function(url, token) {
  url_ <- paste0(url, token)
  response <-  GET(url_)
  response$status
  if (response$status == 200) { 
    call <- fromJSON(content(response, as = "text"), flatten = TRUE)
    return (call$result$username)
  } else {
    return (NULL)
  }
}

buildUrl <- function(session, path) {
  port <- session$clientData$url_port
  host <- session$clientData$url_hostname
  protocol <- session$clientData$url_protocol
  url <- paste0(protocol, "//", host, ":", port, "/", path)
  return (url);
}


########### Save reports to file #############
createCmsyPDFReport <- function(file, cmsy, input) {
  tempReport <- file.path(tempdir(), "cmsyReportSingle.Rmd")
  file.copy("assets/cmsy/cmsyReportSingle.Rmd", tempReport, overwrite = TRUE)
  
  
  if (!is.null(cmsy$method$analisysChartUrl)) {
    fileAnalisysChart <- paste(tempdir(),"/","cmsy_fileAnalisysChart",".jpeg",sep="")
    downloadFile(cmsy$method$analisysChartUrl, fileAnalisysChart)
    cmsy$method$analisysChart <- fileAnalisysChart
  }
  if (!is.null(cmsy$method$analisysChartUrl)) {
    fileManagementChart <-paste(tempdir(),"/","cmsy_fileManagementChart",".jpeg",sep="")
    #fileManagementChart <- tempfile(fileext=".jpg")
    downloadFile(cmsy$method$managementChartUrl, fileManagementChart)
    cmsy$method$managementChart <- fileManagementChart
  }
  
  # Set up parameters to pass to Rmd document
  params <- list(cmsy = cmsy, inputParams = input)
  
  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  rmarkdown::render(tempReport, output_file = file, params = params)
}

createElefanGaPDFReport <- function(file, elefan_ga, input) {
  print(paste0("Input file", input$fileGa))
  tempReport <- file.path(tempdir(), "elefan_ga.Rmd")
  file.copy("assets/tropFishR/markdown/elefan_ga.Rmd", tempReport, overwrite = TRUE)
  params <- list(elefan = elefan_ga, inputParams = input)
  return (rmarkdown::render(tempReport, output_file = file, params = params))
}

createElefanSaPDFReport <- function(file, elefan_sa, input) {
  tempReport <- file.path(tempdir(), "elefan_sa.Rmd")
  file.copy("assets/tropFishR/markdown/elefan_sa.Rmd", tempReport, overwrite = TRUE)
  params <- list(elefan = elefan_sa, inputParams = input)
  return (rmarkdown::render(tempReport, output_file = file, params = params))
}

createElefanPDFReport <- function(file, elefan, input) {
  tempReport <- file.path(tempdir(), "elefan.Rmd")
  file.copy("assets/tropFishR/markdown/elefan.Rmd", tempReport, overwrite = TRUE)
  params <- list(elefan = elefan, inputParams = input)
  return (rmarkdown::render(tempReport, output_file = file, params = params))
}

createSbprPDFReport <- function(file, sbprExec, input) {
  print(paste(sep=" ", file))
  tempReport <- file.path(tempdir(), "sbpr.Rmd")
  file.copy("assets/fishmethods/sbpr.Rmd", tempReport, overwrite = TRUE)
  sbprExec$perc <- input$SBPR_MSP
  params <- list(sbprExec = sbprExec, inputParams = input)
  rmarkdown::render(tempReport, output_file = file, params = params)
}

createYprPDFReport <- function(file, yprExec, input) {
  tempReport <- file.path(tempdir(), "ypr.Rmd")
  file.copy("assets/fishmethods/ypr.Rmd", tempReport, overwrite = TRUE)
  params <- list(yprExec = yprExec, inputParams = input)
  rmarkdown::render(tempReport, output_file = file, params = params)
}

fileFolderExistsInPath <- function(path, entity) {
  folders <- listWS(path)
  
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

uploadToIMarineFolder <- function(file, baseFolder, folderName) {
  tryCatch({
    if (fileFolderExistsInPath(baseFolder, folderName) == FALSE) {
      flog.info("Creating folder [%s] in i-Marine workspace path: %s", folderName, baseFolder)
      createFolderWs(
        baseFolder,
        folderName, 
        uploadFolderDescription)
    }
    flog.info("Trying to upload %s to i-Marine workspace folder %s", file, paste0(baseFolder, uploadFolderName, "/"))
    uploadWS(
      path = paste0(baseFolder, folderName, "/"), 
      file = file,
      overwrite = TRUE
    )
    flog.info("File %s successfully uploaded to the i-Marine folder %s", file, paste0(baseFolder, folderName))
  }, error = function(err) {
    flog.error("Error uploading report to the i-Marine workspace: %s", err)
    stop(err)
  }, finally = {})
}

clearResults <- function(id) {
  localJs <- paste0("document.getElementById('", id, "').parentElement.style.visibility = 'hidden'" )
  shinyjs::runjs(localJs)
}