#
# This is the StockMonitoringTools Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# The StockMonitoringTools shiny application will support the FAO - SDG 14.4.1 E-learning course
# 
# Author: Enrico Anello <enrico.anello@fao.org> <enrico.anello@gmail.com>
#

library(shiny)
library(shinyBS)
library(shinyjs)
library(shinysky)
library(shinythemes)
library(shinydashboard)
library(RCurl)
library(fishmethods)
library(TropFishR)
library(ggplot2)
library(rfishbase)
library(waiter)
library(futile.logger)

##### Dependencies
source("ui/menu.R")
source("ui/cmsy/cmsyUI.R")
source("ui/elefan/commonUI.R")
source("ui/elefan/elefanGaUI.R")
source("ui/elefan/elefanSaUI.R")
source("ui/elefan/elefanUI.R")
source("ui/fishMethods/commonUI.R")
source("ui/fishMethods/sbprUI.R")
source("ui/fishMethods/yprUI.R")
source("ui/support/BasicSchaeferUI.R")
source("ui/support/BasicVonBertalannfyUI.R")
source("ui/support/NaturalMortalityUI.R")
source("ui/support/SeasonalVonBertalannfyUI.R")
source("server/common.R")
source("server/elefan/elefanGaServer.R")
source("assets/tropFishR/elefan_common.R")
source("assets/tropFishR/algorithms/run_elefan_ga.R")
source("assets/tropFishR/algorithms/run_elefan_sa.R")
source("assets/tropFishR/algorithms/run_elefan.R")
source("assets/cmsy/CmsyFunction.R")
source("assets/fishmethods/methods.R")
source("assets/support/shaefer.R")
source("assets/support/vonBertalannfly.R")
source("assets/support/seasonalVonBertalannfly.R")
source("assets/support/naturalMortality.R")
source("assets/commons/commons.R")
source("assets/commons/storageHub.R")
source("assets/commons/labels.R")

pdf(NULL)
dev.off()
#dev.list()
t
set.seed(1)
d <- data(package = "TropFishR")
parallel <- FALSE
fishingMortality <- "NA"

ui <- tagList(
  use_waiter(include_js = FALSE),
  waiter_show_on_load(app_load_spinner("Initializing R session. This process may take a while...")),
  dashboardPage(
  dashboardHeader(title = 'Stock Monitoring Tools'),
  dashboardSidebar(
    sidebarMenu(
      menuHome,
      menuCmsy,
      menuElefan,
      menuFishMethods,
      menuSupportingTools
    )
  ),
  dashboardBody(
    tags$div(
      tags$div(
        tags$span("Please wait while your request is being processed. This may take some time..."),
        class="loadingCustomInner"
      ),
      tags$div(
        tags$img(src = 'loading-circle.gif', height="20px"),
        class="loadingCustomInner"
      ),
      class="loadingCustom"),
    useShinyjs(),
    extendShinyjs(text = jscode),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/eqcss/1.7.0/EQCSS.min.js")),
    tags$head(tags$script(type="text/eqcss", src="styles.eqcss")),
    tags$head(tags$script(type="text/javascript", src="custom.js")),
    #busyIndicator(wait = 7000),
    tabItems(
      tabItem("homeTab",htmlOutput("homeInfo")),
      tabCmsyIntro,
      tabCmsySampleDataset,
      tabElefanIntro,
      tabElefanSampleDataset,
      tabFishMethodsIntro,
      tabFishMethodsSampleDataset,
      tabCmsy,
      tabElefanGa("elefanGaModule"),
      tabElefanSa,
      tabElefan,
      tabSbpr,
      tabYpr,
      tabBasicSchaefer,
      tabBasicVonBertalannfy,
      tabSeasonalVonBertalannfy,
      tabNaturalMortality
    )
  )
), tags$footer(footer, align = "center")
)


server <- function(input, output, session) {
  flog.threshold(DEBUG)
  flog.appender(appender.file("session.log"))
  
  session$allowReconnect("force")
  waiter_hide()
  onStop(function() {
    flog.warn("Lost connection to R server")
  })
  
  session$userData$sessionToken <- reactiveVal(NULL)
  session$userData$sessionUsername <- reactiveVal(NULL)
  session$userData$sessionMode <- reactiveVal(NULL)
  
  
  ##Guessing run mode
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[[gcubeTokenQueryParam]])) {
      session$userData$sessionToken(query[[gcubeTokenQueryParam]])
    }
  })
  
  observe({
    if (!is.null(session$userData$sessionToken())) {
      flog.info("Session token is: %s", session$userData$sessionToken())
      session$userData$sessionUsername(getVREUsername(apiUrl, session$userData$sessionToken()))
    } else {
      flog.info("Session token is: %s", "NULL")
    }
  })
  
  observe({
    if (!is.null(session$userData$sessionMode())) {
      flog.info("Session mode is: %s", session$userData$sessionMode())
    } else {
      flog.info("Session mode is: %s", "NULL")
    }
  })
  
  observe({
    if (!is.null(session$userData$sessionUsername())) {
      flog.info("Session username is: %s", session$userData$sessionUsername())
      session$userData$sessionMode("GCUBE")
    } else {
      flog.info("Session username is: %s", "NULL")
    }
  })
  
  session$userData$cmsy <- reactiveValues()

  session$userData$elefan_sa <- reactiveValues()
  session$userData$elefan <- reactiveValues()
  session$userData$sbprExec <- reactiveValues()
  session$userData$yprExec <- reactiveValues()
  session$userData$fishingMortality <- reactiveValues()
  
  session$userData$fishingMortality$FcurrGA <- NA
  session$userData$fishingMortality$FcurrSA <- NA
  session$userData$fishingMortality$Fcurr <- NA
  
  session$userData$cmsyUploadVreResult <- reactiveValues()
  
  session$userData$elefanSaUploadVreResult <- reactiveValues()
  session$userData$elefanUploadVreResult <- reactiveValues()
  session$userData$sbprUploadVreResult <- reactiveValues()
  session$userData$yprUploadVreResult <- reactiveValues()
  
  source("server/cmsy/cmsyServer.R", local=TRUE)
  
  
  callModule(elefanGaModule, "elefanGaModule")
  source("server/elefan/elefanSaServer.R", local=TRUE)
  source("server/elefan/elefanServer.R", local=TRUE)
  source("server/fishMethods/sbprServer.R", local=TRUE)
  source("server/fishMethods/yprServer.R", local=TRUE)
  source("server/support/BasicSchaeferServer.R", local=TRUE)
  source("server/support/BasicVonBertalannfyServer.R", local=TRUE)
  source("server/support/SeasonalVonBertalannfyServer.R", local=TRUE)
  source("server/support/NaturalMortalityServer.R", local=TRUE)
  source("server/labels.R", local=TRUE)
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
