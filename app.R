#
# This is the StockMonitoringTool Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# The StockMonitoringTool shiny application will support the FAO - SDG 14.4.1 E-learning course
#
# Author: Enrico Anello <enrico.anello@fao.org> <enrico.anello@gmail.com>
#

## Note that rfishbase v3.0.1 has to be installed for R<4.0.0:
## remotes::install_github("ropensci/rfishbase", ref = "3.0.1")
## Additional packages needed but no dependencies:
## install.packages(c("pracma","googleVis","lubridate","XML"))

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
library(R.utils)
library(knitr)
library(shinyWidgets)

##### Dependencies
source("ui/menu.R")
source("ui/cmsy/cmsyUI.R")
source("ui/elefan/commonUI.R")
source("ui/elefan/elefanGaUI.R")
# source("ui/elefan/elefanSaUI.R")
# source("ui/elefan/elefanUI.R")
# source("ui/fishMethods/commonUI.R")
# source("ui/fishMethods/sbprUI.R")
# source("ui/fishMethods/yprUI.R")
source("ui/support/BasicSchaeferUI.R")
source("ui/support/BasicVonBertalannfyUI.R")
source("ui/support/NaturalMortalityUI.R")
source("ui/support/SeasonalVonBertalannfyUI.R")
source("server/common.R")
source("server/cmsy/cmsyServer.R")
source("server/elefan/elefanGaServer.R")
# source("server/elefan/elefanSaServer.R")
# source("server/elefan/elefanServer.R")
# source("server/fishMethods/sbprServer.R")
# source("server/fishMethods/yprServer.R")
source("server/support/BasicSchaeferServer.R")
source("server/support/BasicVonBertalannfyServer.R")
source("server/support/SeasonalVonBertalannfyServer.R")
source("server/support/NaturalMortalityServer.R")
source("assets/tropFishR/elefan_common.R")
source("assets/tropFishR/algorithms/run_elefan_ga.R")
source("assets/tropFishR/algorithms/temp_elefan_ga.R")  ## temporarily needed until TropFishR updated
source("assets/tropFishR/algorithms/temp_predict_mod.R")  ## temporarily needed until TropFishR updated
# source("assets/tropFishR/algorithms/run_elefan_sa.R")
# source("assets/tropFishR/algorithms/run_elefan.R")
source("assets/cmsy/CmsyFunction.R")
# source("assets/fishmethods/methods.R")
source("assets/support/shaefer.R")
source("assets/support/vonBertalannfly.R")
source("assets/support/seasonalVonBertalannfly.R")
source("assets/support/naturalMortality.R")
source("assets/commons/commons.R")
source("assets/commons/storageHub.R")
source("assets/commons/labels.R")

fileLog <- Sys.getenv("SMT_LOG")
if (is.null(fileLog) || is.na(fileLog) || fileLog == "") {
  fileLog <- "session.log"
}
print(paste0("Logging to: ", fileLog))

pdf(NULL)
dev.off()
#dev.list()
t
set.seed(1)
d <- data(package = "TropFishR")
parallel <- FALSE
fishingMortality <- "NA"
username <- NULL
token <- NULL

sidebar <- dashboardSidebar(uiOutput("sidebar"))

ui <- tagList(
  use_waiter(),
  waiter_show_on_load(app_load_spinner("Initializing R session. This process may take a while...")),
  dashboardPage(
  dashboardHeader(title = 'Stock Monitoring Tool'),
  sidebar,
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
    extendShinyjs(text = jscode, functions =  c("showBox", "removeBox","showBox2", "removeBox2", "disableAllButtons", "enableAllButtons", "showComputing", "hideComputing", "expandBox","collapseBox")),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/eqcss/1.7.0/EQCSS.min.js")),
    tags$head(tags$script(type="text/eqcss", src="styles.eqcss")),
    tags$head(tags$script(type="text/javascript", src="custom.js")),
    #busyIndicator(wait = 7000),
    tabItems(
      tabItem("homeTab",htmlOutput("homeInfo"), selected=T),
      tabCmsyIntro,
      tabCmsySampleDataset,
      tabElefanIntro,
      tabElefanSampleDataset,
      # tabFishMethodsIntro,
      # tabFishMethodsSampleDataset,
      tabCmsy("cmsyModule"),
      tabElefanGa("elefanGaModule"),
      # tabElefanSa("elefanSaModule"),
      # tabElefan("elefanModule"),
      # tabSbpr("sbprModule"),
      # tabYpr("yprModule"),
      tabBasicSchaefer("basicShaeferModule"),
      tabBasicVonBertalannfy("vonBertalannfyModule"),
      tabSeasonalVonBertalannfy("seasonalVonBertalannfyModule"),
      tabNaturalMortality("naturalMortalityModule")
    )
  )
), tags$footer(footer, align = "center")
)


server <- function(input, output, session) {
  flog.threshold(DEBUG)

  flog.appender(appender.file(fileLog))

  session$allowReconnect("force")
  waiter_hide()
  onStop(function() {
    flog.warn("Lost connection to R server")
  })

  output$sidebar <- renderUI({
    dashboardSidebar(
      sidebarMenu(id="smt-tabs",
        menuItem("Home", tabName="homeTab"),
        menuCmsy,
        menuElefan,
        # menuFishMethods,
        menuSupportingTools
      )
    )
  })

  session$userData$page <- reactiveVal(NULL)

  ### Render the page set as last visited in session or by page= query param
  observe({
    currentPage <- NA
    if (!is.null(session$userData$page())) {
      currentPage <- session$userData$page()
    } else {
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query$page)) {
        currentPage <- query$page
      }
    }
    flog.info("Current Page: %s", currentPage)
    if (!is.na(currentPage)) {
      switch(currentPage,
             'cmsy-intro'= {isolate({updateTabItems(session, "smt-tabs", "cmsyIntro")})},
             'cmsy'= {isolate({updateTabItems(session, "smt-tabs", "cmsyWidget")})},
             'cmsy-sample'= {isolate({updateTabItems(session, "smt-tabs", "cmsySampleDataset")})},
             'elefan-intro' = {isolate({updateTabItems(session, "smt-tabs", "ElefanIntro")})},
             'elefan-ga' = {isolate({updateTabItems(session, "smt-tabs", "ElefanGaWidget")})},
             # 'elefan-sa' = {isolate({updateTabItems(session, "smt-tabs", "ElefanSaWidget")})},
             # 'elefan' = {isolate({updateTabItems(session, "smt-tabs", "ElefanWidget")})},
             'elefan-sample' = {isolate({updateTabItems(session, "smt-tabs", "ElefanSampleDataset")})},
             # 'fishmethods-intro' = {isolate({updateTabItems(session, "smt-tabs", "FishMethodsIntro")})},
             # 'sbpr' = {isolate({updateTabItems(session, "smt-tabs", "SBPRWidget")})},
             # 'ypr' = {isolate({updateTabItems(session, "smt-tabs", "YPRWidget")})},
             # 'fishmethods-sample' = {isolate({updateTabItems(session, "smt-tabs", "FishMethodsSampleDataset")})},
             'basic-shaefer' = {isolate({updateTabItems(session, "smt-tabs", "BasicSchaefer")})},
             'basic-von-bertalannfy' = {isolate({updateTabItems(session, "smt-tabs", "BasicVonBertalannfy")})},
             'seasonal-von-bertalannfy' = {isolate({updateTabItems(session, "smt-tabs", "SeasonalVonBertalannfy")})},
             'natural-mortality' = {isolate({updateTabItems(session, "smt-tabs", "NaturalMortality")})},
             'home' = {isolate({updateTabItems(session, "smt-tabs", "homeTab")})},
             {isolate({updateTabItems(session, "smt-tabs", "homeTab")})}
      )
    } else {
      isolate({updateTabItems(session, "smt-tabs", "homeTab")})
    }
  })

  session$userData$sessionToken <- reactiveVal(NULL)
  session$userData$sessionUsername <- reactiveVal(NULL)
  session$userData$sessionMode <- reactiveVal(NULL)

  ## Hide any overlay when session starts
  observe({
    js$hideComputing()
  })

  ##Guessing run mode
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[[gcubeTokenQueryParam]])) {
      session$userData$sessionToken(query[[gcubeTokenQueryParam]])
    }
  #})

  #observe({
    if (!is.null(session$userData$sessionToken())) {
      flog.info("Session token is: %s", session$userData$sessionToken())
      session$userData$sessionUsername(getVREUsername(apiUrl, session$userData$sessionToken()))
    } else {
      flog.info("Session token is: %s", "NULL")
    }
  #})

  #observe({
    if (!is.null(session$userData$sessionMode())) {
      flog.info("Session mode is: %s", session$userData$sessionMode())
    } else {
      flog.info("Session mode is: %s", "NULL")
    }
 # })

  #observe({
    if (!is.null(session$userData$sessionUsername())) {
      flog.info("Session username is: %s", session$userData$sessionUsername())
      session$userData$sessionMode("GCUBE")
      username <<- session$userData$sessionUsername()
      token <<- session$userData$sessionToken()
    } else {
      flog.info("Session username is: %s", "NULL")
    }
  })

  session$userData$cmsy <- reactiveValues()

  # session$userData$elefan_sa <- reactiveValues()
  # session$userData$elefan <- reactiveValues()
  session$userData$sbprExec <- reactiveValues()
  session$userData$yprExec <- reactiveValues()
  session$userData$fishingMortality <- reactiveValues()

  session$userData$fishingMortality$FcurrGA <- NA
  session$userData$fishingMortality$FcurrSA <- NA
  session$userData$fishingMortality$Fcurr <- NA

  session$userData$cmsyUploadVreResult <- reactiveValues()
  session$userData$elefanGaUploadVreResult <- reactiveValues()
  # session$userData$elefanSaUploadVreResult <- reactiveValues()
  # session$userData$elefanUploadVreResult <- reactiveValues()
  session$userData$sbprUploadVreResult <- reactiveValues()
  session$userData$yprUploadVreResult <- reactiveValues()

  callModule(cmsyModule, "cmsyModule")
  callModule(elefanGaModule, "elefanGaModule")
  # callModule(elefanSaModule, "elefanSaModule")
  # callModule(elefanModule, "elefanModule")
  # callModule(sbprModule, "sbprModule")
  # callModule(yprModule, "yprModule")
  callModule(basicShaeferModule, "basicShaeferModule")
  callModule(vonBertalannfyModule, "vonBertalannfyModule")
  callModule(seasonalVonBertalannfyModule, "seasonalVonBertalannfyModule")
  callModule(naturalMortalityModule, "naturalMortalityModule")


  source("server/labels.R", local=TRUE)


}

# Run the application
shinyApp(ui = ui, server = server)
