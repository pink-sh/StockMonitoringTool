cmsyModule <- function(input, output, session) {
  
  cmsy <- reactiveValues()
  cmsyUploadVreResult <- reactiveValues()
  
  fileContents <- reactiveValues()
  filePath <- reactiveValues()
  
  basicValidation <- function (a) {
    #if (is.null(colnames(a[1])) || is.na(colnames(a[1]))) return (FALSE)
    #if (tolower(colnames(a[1])) == 'stock') return(TRUE)
    validInputColumns<-c("stock","yr","ct","bt")
    if(ncol(a)==1){return ("delimiter error")
    } else if (length(setdiff(tolower(names(a[1:4])),validInputColumns))!=0) {return ("colname error")
    } else if (!is.numeric(a$ct)) {return ("not point")
    } else if ((max(as.numeric(a$yr))-min(as.numeric(a$yr)))<15) {return("under 15")
    } else {return(a)}
  }
  cmsyFileData <- reactive({
    inFileCmsy <- input$fileCmsy
    if (is.null(inFileCmsy)) {
      removeUI(selector="#stockSelectorContainerInner")
      return (NULL)
    }
    contents <- read.csv(inFileCmsy$datapath)
    contents <- basicValidation(contents)
    if (is.null(contents)) {
      return (NULL)
    }else{print("First calculate")
      calculateAndUpdateYear(contents,sort(unique(contents$Stock))[1])
      }
    return (contents)
  })
  
  
  observeEvent(input$fileCmsy, {
    filePath$datapath <- input$fileCmsy$datapath
    contents <- cmsyFileData()
    if (!is.data.frame(contents)) {
      shinyjs::disable("go_cmsy")
      removeUI(selector="#stockSelectorContainerInner")
      showModal(modalDialog(
        title = "Error",
        if(is.null(contents)){"Input file seems invalid"
          }else if(contents=="delimiter error"){"Please ensure that your .csv file delimiter is a comma ','" 
          }else if(contents=="not point"){"Please ensure your separate decimals using points ‘.’ or you don't have non numeric value"
          }else if(contents=="colname error"){
            text<-"Please ensure your columns names exactly match the guidelines, i.e."
            text<-paste0(text, "<ul>")
            text <- paste0(text, "<li>Stock</li>")
            text <- paste0(text, "<li>yr</li>")
            text <- paste0(text, "<li>ct</li>")
            text <- paste0(text, "<li>bt</li>")
            text <- paste0(text, "</ul>")
            HTML(text)
          }else if(contents=="under 15"){"Catch time series must be at least 15 years"
          } else{"Input file seems invalid"},
        easyClose = TRUE,
        footer = NULL
      ))
      fileContents$data <- NULL
      flog.error("Input file for CMSY %s seems invalid", filePath$datapath)
    } else {
      insertUI(
        ui=tags$div(
          selectInput(session$ns("stock"), "Select a stock", sort(unique(contents$Stock))), 
          id="stockSelectorContainerInner"), 
        selector="#stockSelectorContainer")
      shinyjs::enable("go_cmsy")
      fileContents$data <- contents
      flog.info("Input file for CMSY %s seems valid", filePath$datapath)
    }
  })
  
  calculateAndUpdateYear<-function(contents,stock) {
    
    if (is.null(contents)) {
      "Test"
      return(NULL)
    }
    
    maxYear <- 0
    minYear <- 0
    i<-0
    for (line in rownames(contents)) {
      if (contents[line, "Stock"] == stock) {
        if (i == 0) {
          maxYear <- contents[line, "yr"]
          minYear <- contents[line, "yr"]
        } else {
          if (maxYear < contents[line, "yr"]) {
            maxYear <- contents[line, "yr"]
          }
          if (minYear > contents[line, "yr"]) {
            minYear <- contents[line, "yr"]
          }
        }
        i <- i + 1
      }
    }
    print(minYear)
    print(maxYear)
    updateTextInput(session, "minOfYear", value=as.integer(minYear))
    updateTextInput(session, "maxOfYear", value=as.integer(maxYear))
    updateTextInput(session, "startYear", value=as.integer(minYear))
    updateTextInput(session, "endYear", value=as.integer(maxYear)-1)
    
    if (minYear < 1960) {
      updateSliderInput(session,"stb",value=c(0.5,0.9))
    } else {
      updateSliderInput(session,"stb",value=c(0.2,0.6))
    }
  }
  
  observeEvent(input$stock, {
    print("calculate")
    calculateAndUpdateYear(fileContents$data,input$stock)
  })
  
  observeEvent(input$go_cmsy, {

    query <- parseQueryString(session$clientData$url_search)
    
    if (is.null(fileContents$data)) {
      return (NULL)
    }
    yr <- fileContents$data$yr
    
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
      results = tryCatch({
        js$showComputing()
        #templateFileDlmTools <- paste0(getwd(), "/assets/cmsy/cmsyFastTemplate.xml")
        templateFileDlmTools <- paste0(getwd(), "/assets/cmsy/cmsyLegacyTemplate.xml")
        
        a <- fileContents$data
        
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
        flog.info("Starting CMSY computation")
      # ret <- runCmsy(region_,toString(sub_region_),input$stock,toString(group_),toString(name_),toString(en_name_),toString(scientific_name_),"-",input$minOfYear,input$maxOfYear,input$startYear,input$endYear,input$flim,input$fpa,input$blim,input$bpa,input$bmsy,input$fmsy,input$msy,input$msyBTrigger,input$b40,input$m,input$fofl,input$last_f,input$resiliance,input$r.low,input$r.hi,input$stb.low,input$stb.hi,input$int.yr,input$intb.low,input$intb.hi,input$endb.low,input$endb.hi,input$q.start,input$q.end,input$btype,input$force.cmsy,input$comments, vreToken, filePath$datapath, templateFileDlmTools)
        ret <- runCmsy(region_,toString(sub_region_),input$stock,toString(group_),toString(name_),toString(en_name_),toString(scientific_name_),"-",input$minOfYear,input$maxOfYear,input$startYear,input$endYear,input$flim,input$fpa,input$blim,input$bpa,input$bmsy,input$fmsy,input$msy,input$msyBTrigger,input$b40,input$m,input$fofl,input$last_f,input$resiliance,input$r.low,input$r.hi,min(input$stb),max(input$stb),input$int.yr,input$intb.low,input$intb.hi,min(input$endb),max(input$endb),input$q.start,input$q.end,input$btype,input$force.cmsy,input$comments, vreToken, filePath$datapath, templateFileDlmTools)
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
            #fileAnalisysChart <- tempfile(fileext=".jpeg")
            fileAnalisysChart <- paste(tempdir(),"/","cmsy_fileAnalisysChart",".jpeg",sep="")
            print(fileAnalisysChart)
            downloadFile(row$url, fileAnalisysChart)
            cmsy$method$analisysChart <- fileAnalisysChart
            cmsy$method$analisysChartUrl <- row$url
          }
          if (row$description == "management_charts") {
            #fileManagementChart <- tempfile(fileext=".jpeg")
            fileManagementChart <-paste(tempdir(),"/","cmsy_fileManagementChart",".jpeg",sep="")
            print(fileManagementChart)
            downloadFile(row$url, fileManagementChart)
            cmsy$method$managementChart <- fileManagementChart
            cmsy$method$managementChartUrl <- row$url
          }
          if (row$description == "Log of the computation") {
            cmsy$method$log <- row$url
          }
        }
        
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
          flog.info("Uploading CMSY report to i-Marine workspace")
          reportFileName <- paste(tempdir(),"/","CMSY_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
          createCmsyPDFReport(reportFileName, cmsy, input)
          cmsyUploadVreResult$res <- FALSE
          
          basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace/")
          tryCatch({
            uploadToIMarineFolder(reportFileName, basePath, uploadFolderName)
            cmsyUploadVreResult$res <- TRUE
          }, error = function(err) {
            flog.error("Error uploading CMSY report to the i-Marine Workspace: %s", err)
            cmsyUploadVreResult$res <- FALSE
          }, finally = {})
        }
      }, error = function(err) {
        flog.error("Error in CMSY: %s ",err)
        showModal(modalDialog(
          title = "Error",
          HTML(sprintf(getErrorMessage("CMSY"), err)),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      },
      finally = {
        js$hideComputing()
        js$enableAllButtons()
      })
    }
  })
  
  observeEvent(input$reset_cmsy, {
    resetCmsyInputValues()
    cmsy$method <- NULL
  })
  ####### END OBSERVERS #######
  
  ####### CMSY OUTPUT FUNCTION #######
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
             width = "100%",
             height = "100%")
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
             width = "100%",
             height = "100%")
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
    session$userData$page("cmsy")
    text <- "<span><h3><b>CMSY (Catch-Maximum Sustainable Yield) Method</b></h3></span>"
    text
  })
  output$downloadCmsyReportButton <- renderUI({
    if (!is.null(cmsy$method)) {
      downloadButton(session$ns("downloadCmsyReport"), label = "Download Report")
    }
  })
  output$CmsyVREUpload <- renderText(
    {
      text <- ""
      if (!is.null(cmsy$dlmTools) || !is.null(cmsy$legacy) || !is.null(cmsy$fast)) {
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
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
}