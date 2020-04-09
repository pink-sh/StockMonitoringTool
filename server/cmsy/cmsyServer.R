observeEvent(input$stock, {
  inFile1 <- input$file1
  
  if (is.null(inFile1)) {
    return(NULL)
  }
  a <- read.csv(inFile1$datapath)
  
  maxYear <- 0
  minYear <- 0
  i<-0
  for (line in rownames(a)) {
    if (a[line, "Stock"] == input$stock) {
      if (i == 0) {
        maxYear <- a[line, "yr"]
        minYear <- a[line, "yr"]
      } else {
        if (maxYear < a[line, "yr"]) {
          maxYear <- a[line, "yr"]
        }
        if (minYear > a[line, "yr"]) {
          minYear <- a[line, "yr"]
        }
      }
      i <- i + 1
    }
  }
  updateTextInput(session, "minOfYear", value=as.integer(minYear))
  updateTextInput(session, "maxOfYear", value=as.integer(maxYear))
  updateTextInput(session, "startYear", value=as.integer(minYear))
  updateTextInput(session, "endYear", value=as.integer(maxYear)-1)
  
  if (minYear < 1960) {
    updateTextInput(session, "stb.low", value=0.5)
    updateTextInput(session, "stb.hi", value=0.9)
  } else {
    updateTextInput(session, "stb.low", value=0.2)
    updateTextInput(session, "stb.hi", value=0.6)
  }
})

observeEvent(input$go_cmsy, {
  query <- parseQueryString(session$clientData$url_search)
  
  
  
  infile1 <- input$file1
  
  if (is.null(infile1)) {
    showModal(modalDialog(
      title = "Error",
      "No input file selected",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  }
  
  inputCsvFile <- infile1$datapath
  yr <- read.csv(inputCsvFile)$yr
  
  minYr <- NULL
  maxYr <- NULL
  for (y in yr) {
    if (is.null(minYr) || y < minYr) {
      minYr <- y
    }
    if (is.null(maxYr) || y > maxYr) {
      maxYr <- y
    }
  }
  
  if ((maxYr-minYr)<=15) {
    offset = maxYr-minYr
    showModal(modalDialog(
      title = "Error",
      paste0("The input time-series must cover at least 15 years in length, the provided one covers ", offset, " years"),
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  } else {
    js$showComputing()
    #templateFileDlmTools <- paste0(getwd(), "/assets/cmsy/cmsyFastTemplate.xml")
    templateFileDlmTools <- paste0(getwd(), "/assets/cmsy/cmsyLegacyTemplate.xml")
    
    a <- read.csv(inputCsvFile)
    
    group_ = ""
    name_ = ""
    en_name_ = ""
    scientific_name_ = ""
    sub_region_ = ""
    region_ = ""
    for (line in rownames(a)) {
      if (a[line, "Stock"] == input$stock) {
        name_ = a[line, "name"]
        group_ = a[line, "group"]
        en_name_ = a[line, "english_name"]
        scientific_name_ = a[line, "scientific_name"]
        region_ = a[line, "region"]
        sub_region_ = a[line, "subregion"]
        break
      }
    }
    
    cmsy$fast <- list()
    js$disableAllButtons()
    ret <- runCmsy(region_,toString(sub_region_),input$stock,toString(group_),toString(name_),toString(en_name_),toString(scientific_name_),"-",input$minOfYear,input$maxOfYear,input$startYear,input$endYear,input$flim,input$fpa,input$blim,input$bpa,input$bmsy,input$fmsy,input$msy,input$msyBTrigger,input$b40,input$m,input$fofl,input$last_f,input$resiliance,input$r.low,input$r.hi,input$stb.low,input$stb.hi,input$int.yr,input$intb.low,input$intb.hi,input$endb.low,input$endb.hi,input$q.start,input$q.end,input$btype,input$force.cmsy,input$comments, vreToken, inputCsvFile, templateFileDlmTools)
    js$enableAllButtons()
    js$hideComputing()
    js$showBox("box_cmsy_results")
    for(i in 1:nrow(ret)) {
      row <- ret[i,]
      if (row$description == "estimates") {
        contents <- getURL(row$url)
        print (paste0("Cmsy text url", row$url))
        contents <- gsub("Results for Management", "Reference points and indicators", contents)
        cmsy$method$textRaw <- contents
        contents <- gsub("\n\r", "<br/>", contents)
        contents <- gsub("\n", "<br/>", contents)
        contents <- gsub("B/Bmsy in last year", "<b>B/Bmsy in last year</b>", contents)
        contents <- gsub("----------------------------------------------------------", "", contents)
        cmsy$method$text <- contents
      }
      if (row$description == "analysis_charts") {
        fileAnalisysChart <- tempfile(fileext=".jpg")
        download.file(row$url, fileAnalisysChart)
        cmsy$method$analisysChart <- fileAnalisysChart
        cmsy$method$analisysChartUrl <- row$url
      }
      if (row$description == "management_charts") {
        fileManagementChart <- tempfile(fileext=".jpg")
        download.file(row$url, fileManagementChart)
        cmsy$method$managementChart <- fileManagementChart
        cmsy$method$managementChartUrl <- row$url
      }
      if (row$description == "Log of the computation") {
        cmsy$method$log <- row$url
      }
    }
    
    if (!is.null(sessionMode()) && sessionMode()=="GCUBE") {
      print("uploading to VRE")
      reportFileName <- paste("/tmp/","CMSY_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
      createCmsyPDFReport(reportFileName, cmsy, input)
      cmsyUploadVreResult$res <- FALSE
      tryCatch({
        if (fileFolderExistsInPath(sessionUsername(),sessionToken(),paste0("/Home/",sessionUsername(),"/Workspace/"), uploadFolderName) == FALSE) {
          print("Creating folder")
          createFolderWs(
            sessionUsername(), sessionToken(),
            paste0("/Home/",sessionUsername(),"/Workspace/"),
            uploadFolderName, 
            uploadFolderDescription)
        }
        uploadToVREFolder(
          username = sessionUsername(), 
          token = sessionToken(), 
          relativePath = paste0("/Home/",sessionUsername(),"/Workspace/", uploadFolderName, "/"), 
          file = reportFileName,
          overwrite = TRUE,
          archive = FALSE
        )
        cmsyUploadVreResult$res <- TRUE
      }, error = function(err) {
        print(paste0("Error uploading CMSY report to the Workspace: ", err))
        cmsyUploadVreResult$res <- FALSE
      }, finally = {})
    }
  }
})

####### END OBSERVERS #######

####### CMSY OUTPUT FUNCTION #######
output$fill <- renderUI({
  inFile1 <- input$file1
  
  if (is.null(inFile1)) {
    return(NULL)
  }
  a <- read.csv(inFile1$datapath)
  
  selectInput("stock", "Select a stock", sort(unique(a$Stock)))
})

output$downloadCmsyReport <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = paste("CMSY_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
  content = function(file) {
    createCmsyPDFReport(file, cmsy, input)
  }
)
output$renderCmsyLog <- renderText({
  if ("method" %in% names(cmsy)) {
    if (!is.null(cmsy$method)) {
      log <- paste0("<a href='", cmsy$method$log, "'>Download the log of the computation</a>")
      log
    } else { "" }
  } else { "" }
})
output$renderCmsyInfo <- renderText({
  if ("method" %in% names(cmsy)) {
    if (!is.null(cmsy$method)) {
      cmsy$method$text <- gsub("\n\r", "<br/>", cmsy$method$text)
      cmsy$method$text <- gsub("\n", "<br/>", cmsy$method$text)
      cmsy$method$text <- gsub("Results for Management", "Reference points and indicators", cmsy$method$text)
      #cmsy$method$text <- gsub("B/Bmsy in last year", "<b>B/Bmsy in last year</b>", cmsy$method$text)
      cmsy$method$text <- gsub("----------------------------------------------------------", "", cmsy$method$text)
      cmsy$method$text
    } else {  "" }
  } else {  "" }
})
output$renderCmsyManagementChart <- renderImage({
  if ("method" %in% names(cmsy)) {
    if (!is.null(cmsy$method)) {
      Sys.sleep(1)
      w1 <- session$clientData$output_renderCmsyManagementChart_width
      h1 <- (w1*3)/4
      list(src = cmsy$method$managementChart,
           contentType = 'image/jpg',
           width = w1,
           height = h1)
    } else {
      list(src = "NULL")
    }
  } else {
    list(src = "NULL")
  }
})
output$renderCmsyAnalysisChart <- renderImage({
  if ("method" %in% names(cmsy)) {
    if (!is.null(cmsy$method)) {
      Sys.sleep(1)
      w2 <- session$clientData$output_renderCmsyAnalysisChart_width
      h2 <- (w2*3)/4
      list(src = cmsy$method$analisysChart,
           contentType = 'image/jpg',
           width = w2,
           height = h2)
    } else {
      list(src = "NULL")
    }
  } else {
    list(src = "NULL")
  }
})
####### END CMSY OUTPUT FUNCTION #######

####### CMSY TEXT #######
output$cmsyMethodTitle <- renderText({
  text <- "<span><h3><b>CMSY (Catch-Maximum Sustainable Yield) Method</b></h3></span>"
  text
})
output$downloadCmsyReportButton <- renderUI({
  if (!is.null(cmsy$dlmTools) || !is.null(cmsy$legacy) || !is.null(cmsy$fast)) {
    downloadButton("downloadCmsyReport", label = "Download Report")
  }
})
output$CmsyVREUpload <- renderText(
  {
    text <- ""
    if (!is.null(cmsy$dlmTools) || !is.null(cmsy$legacy) || !is.null(cmsy$fast)) {
      if (!is.null(sessionMode()) && sessionMode() == "GCUBE") {
        if (isTRUE(cmsyUploadVreResult$res)) {
          text <- paste0(text, VREUploadText)
        }
      }
    }
    text
  }
)
output$titleCmsy <- renderText({
  if ("dlmTools" %in% names(cmsy)) {
    if (!is.null(cmsy$dlmTools)) {
      title <- "<h1> CMSY Method - Results</h1>"
      title
    } else {  "" }
  } else {  "" }
})
output$titleCmsyManagementChart <- renderText({
  if ("method" %in% names(cmsy)) {
    if (!is.null(cmsy$method)) {
      #title <- "<h2> Management Charts </h2>"
      title <- "<h2> Output Graphs </h2>"
      title
    } else {  "" }
  } else {  "" }
})
output$titleCmsyAnalisysChart <- renderText({
  if ("method" %in% names(cmsy)) {
    if (!is.null(cmsy$method)) {
      #title <- "<h2> Analysis Charts </h2>"
      #title
    } else {  "" }
  } else {  "" }
})
output$cmsyDataConsiderationsText <- renderText(getDataConsiderationTextForCmsy())