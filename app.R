#
# This is the StockMonitoringTools Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# The StockMonitoringTools shiny application will support the FAO - SDG 14.4.1 E-learning course

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

buildUrl <- function(session, path) {
  port <- session$clientData$url_port
  host <- session$clientData$url_hostname
  protocol <- session$clientData$url_protocol
  url <- paste0(protocol, "//", host, ":", port, "/", path)
  return (url);
}

##### Dependencies
source("assets/tropFishR/elefan_common.R")
source("assets/tropFishR/run_elefan_ga.R")
source("assets/tropFishR/run_elefan_sa.R")
source("assets/tropFishR/run_elefan.R")
source("assets/cmsy/CmsyFunction.R")
source("assets/fishmethods/methods.R")
source("assets/support/shaefer.R")
source("assets/support/vonBertalannfly.R")
source("assets/support/seasonalVonBertalannfly.R")
source("assets/support/naturalMortality.R")

##### Common Javascript code
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

pdf(NULL)
dev.off()
#dev.list()

set.seed(1)
d <- data(package = "TropFishR")
parallel <- FALSE
fishingMortality <- "NA"

ui <- tagList(dashboardPage(
  dashboardHeader(title = 'Stock Monitoring Tools'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="homeTab"),
      menuItem("CMSY",
               menuSubItem("Introduction", tabName = "CmsyIntro"),
               menuSubItem("CMSY Method", tabName = "cmsyMethod"),
               menuSubItem("CMSY Sample Dataset", tabName = "CmsySampleDataset")
      ),
      menuItem("Elefan by TropFishR",
               menuSubItem("Introduction", tabName = "ElefanIntro"),
               menuSubItem("Elefan GA", tabName = "ElefanGaWidget"),
               menuSubItem("Elefan SA", tabName = "ElefanSaWidget"),
               menuSubItem("Elefan", tabName = "ElefanWidget"),
               menuSubItem("Elefan Sample Dataset", tabName = "ElefanSampleDataset")
      ),
      menuItem("Fish Methods",
               menuSubItem("Introduction", tabName = "FishMethodsIntro"),
               menuSubItem("SBPR", tabName = "SBPRWidget"),
               menuSubItem("YPR", tabName = "YPRWidget"),
               menuSubItem("Fishmethods Sample Dataset", tabName = "FishMethodsSampleDataset")
      ),
      menuItem("Supporting Tools",
               menuSubItem("Schaefer logistic growth", tabName = "BasicSchaefer"),
               menuSubItem("Von Bertalanffy growth function", tabName = "BasicVonBertalannfy"),
               menuSubItem("Seasonal Von Bertalanffy", tabName = "SeasonalVonBertalannfy"),
               menuSubItem("Natural Mortality Estimators", tabName = "NaturalMortality")
      )
    )
  ),
  dashboardBody(
    tags$div(tags$span("Computing results"), tags$img(src = 'loading-circle.gif', height="20px"), class="loadingCustom"),
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
      tabItem("homeTab",
              htmlOutput("homeInfo")
      ),
      tabItem("CmsyIntro",
              htmlOutput("cmsyIntroOut")
      ),
      tabItem("CmsySampleDataset",
              htmlOutput("cmsySampleDataset")
      ),
      tabItem("ElefanIntro",
              htmlOutput("elefanIntroOut")
      ),
      tabItem("ElefanSampleDataset",
              htmlOutput("elefanSampleDataset")
      ),
      tabItem("FishMethodsIntro",
              htmlOutput("fishMethodsIntroOut")
      ),
      tabItem("FishMethodsSampleDataset",
              htmlOutput("fishMethodsSampleDataset")
      ),
      tabItem("cmsyMethod",
              htmlOutput("cmsyMethodTitle"),
              actionButton("cmsyDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
              fluidRow(
                bsModal("modalExampleCMSY", "CMSY Data Considerations", "cmsyDataConsiderations", size = "large", htmlOutput("cmsyDataConsiderationsText")),
                #box(title = "Data Considerations",
                #    width = NULL,
                #    collapsible = T, 
                #    class = "collapsed-box",
                #    checkboxInput("checkbox_cmsy", label = "Is your time-series at least 15 years in length from starting year to ending year? (Note that years with missing data should be filled with an 'NA' value.", value = F),
                #    p("**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of natural mortality (M) for the Optional Parameters section.")
                #),
                box(title = "Main Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    box(
                      fileInput("file1", "Choose Stock CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                      )
                    ),
                    box(
                      uiOutput("fill")
                    )
                ),
                box(title = "Optional Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("minOfYear", "Earliest year of the catch series", 1970, min = 1900, max = 2020, step=1),
                      numericInput("maxOfYear", "Latest year of the catch series", 2014, min = 1900, max = 2020, step=1),
                      selectInput("resiliance", "Resilience as qualitative information (Use information from FishBase or SeaLifeBase)", choices=c("Very low", "Low", "Medium", "High"), selected="Medium"),
                      textInput("r.low", "Lowest resilience (automatically calculated if not set)", "NA"),
                      textInput("r.hi", "Highest resilience (automatically calculated if not set)", "NA"),
                      p("**The user should take care when setting the prior estimates for depletion at the beginning and end of the time series. Depletion levels are assumptions about the initial and current state of the stock, and they have a strong influence on the results of CMSY, so careful evaluation of these parameters is recommended. These parameters are determined in CMSY using the relationship between current catch and maximum catch."),
                      numericInput("stb.low", "**Starting depletion range: Lowest possible relative biomass at the beginning of the catch time series (automatically calculated if not set)", 0, min = 0, max = 10, step=0.1),
                      numericInput("stb.hi", "**Starting depletion range: Highest possible relative biomass at the beginning of the catch time series (automatically calculated if not set)", 0, min = 0, max = 10, step=0.1),
                      textInput("int.yr", "Intermediate year (automatically calculated if not set)", "NA"),
                      textInput("intb.low", "Lowest possible relative biomass at the intermediate year of the catch time series (automatically calculated if not set)", "NA"),
                      textInput("intb.hi", "Highest possible relative biomass at the intermediate year of the catch time series (automatically calculated if not set)", "NA"),
                      numericInput("endb.low", "**Ending depletion range: Lowest possible relative biomass at the end of the catch time series (automatically calculated if not set)", 0.01, min = 0, max = 10, step=0.01),
                      numericInput("endb.hi", "**Ending depletion range: Highest possible relative biomass at the end of the catch time series (automatically calculated if not set)", 0.4, min = 0, max = 10, step=0.1),
                      textInput("q.start", "Prior for catchability (q) value at the beginning of a stable catch-biomass period of minimum 5 years", "NA"),
                      textInput("q.end", "Prior for q value at the end of a stable catch-biomass period of minimum 5 years", "NA")
                    ),
                    box(
                      numericInput("startYear", "Start year to process the catch series from", 1970, min = 1900, max = 2020, step=1),
                      numericInput("endYear", "End year to process the catch series up to", 2014, min = 1900, max = 2020, step=1),
                      textInput("blim", p("Biomass biological limit (", withMathJax("\\(B_{lim}\\)"), ")"), "NA"),
                      textInput("bpa", p("Biomass precautionary value (",withMathJax("\\(B_{pa}\\)") , ")"), "NA"),
                      textInput("bmsy", p("Biomass maximum sustainable yield (", withMathJax("\\(B_{MSY}\\)"), ")"), "NA"),
                      textInput("b40", p("Biomass at 40% over the unfished level (", withMathJax("\\(B_{40\\%}\\)"), ")"), "NA"),
                      textInput("fmsy", p("Fishing mortality at Maximum Sustainable Yield (",withMathJax("\\(F_{MSY}\\)") , "). If" 
                                          ,withMathJax("\\(F_{MSY}\\)") ,"is known, the resilience prior range (lowest and highest resilience estimates) 
                                          could be defined to include estimate of", withMathJax("\\(F_{MSY}\\)") , 
                                          "assuming that r", withMathJax("\\(\\approx\\)"),withMathJax("\\(F_{MSY}\\)")), "NA"),
                      textInput("flim", p("Fishing mortality biological limit (", withMathJax("\\(F_{lim}\\)"), ")"), "NA"),
                      textInput("fpa", p("Fishing mortality precautionary value (", withMathJax("\\(F_{pa}\\)"), ")"), "NA"),
                      textInput("fofl", p("Fishing mortality at overfishing level (", withMathJax("\\(F_{ofl}\\)"),")"), "NA"),
                      textInput("last_f", "Last known exploitation rate", "NA"),
                      textInput("msy", "Maximum Sustainable Yield (MSY)", "NA"),
                      textInput("msyBTrigger", p("Spawning Stock Biomass at MSY (", withMathJax("\\(SSB_{MSY}\\)"), ")"), "NA"),
                      textInput("m", "**Natural mortality (M)", "NA"),
                      p("**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M here."),
                      ##KK: it's not clear to me what the user input would be here if not "None". Suggest deleting (also for Comments.
                      #textInput("btype", "btype indicates if the catch file contains biomass, CPUE or no information associated with the catch time series", "None"),
                      #textInput("comments", "Comments on data and computation", "landings"),
                      checkboxInput("force.cmsy", "Check this if CMSY results are to be preferred over the Bayesian State Model results (only when biomass or CPUE is available)", FALSE)
                      
                    )
                      ),
                actionButton("go_cmsy", "Run CMSY Method"),
                htmlOutput("cmsyWarning"),
                hr(),
                box( width= 100, id = "box_cmsy_results",
                     title = "Results of CMSY Method",
                     tags$style(type="text/css",
                                ".recalculating {opacity: 1.0;}"
                     ),
                     fluidRow(
                       box(
                         uiOutput("downloadCmsyReportButton")
                       )
                     ),
                     fluidRow(
                       box(
                         htmlOutput("renderCmsyLog"),
                         htmlOutput("renderCmsyInfo")
                       ),
                       box(id = "box_cmsy_results_charts",
                           htmlOutput("titleCmsyManagementChart"),
                           imageOutput("renderCmsyManagementChart"),
                           htmlOutput("titleCmsyAnalisysChart"),
                           imageOutput("renderCmsyAnalysisChart")
                       )
                     )
                )
              )
      ),
      tabItem("ElefanGaWidget",
              htmlOutput("elefanGaTitle"),
              actionButton("elefanGADataConsiderations", "Data Considerations", class="topLevelInformationButton"),
              fluidRow(
                bsModal("modalExampleGA", "ELEFAN_GA Data Considerations", "elefanGADataConsiderations", size = "large", htmlOutput("elefanGADataConsiderationsText")),
                #box(title = "Data Considerations",
                #    width = NULL,
                #    collapsible = T, 
                #    class = "collapsed-box",
                #    checkboxInput("checkbox1", label = "Is your length-frequency data representative of the full population? (If this is not so, then estimates of fishing mortality will be biased.)", value = F),
                #    checkboxInput("checkbox2", label = "Were all age groups sampled?", value = F),
                #    checkboxInput("checkbox3", label = "Was the sample from a range of areas where different life histories might live? (e.g., if juveniles occupy nearshore habitat and adults are offshore)", value = F),
                #    checkboxInput("checkbox4", label = "Are a variety of gears with different selectivities used to collect the samples so that the samples contain multiple age groups?", value = F),
                #    p("**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of ", withMathJax("\\(L_\\infty\\)"), " and von Bertalanffy K in the Optional Parameters section in the ELEFAN_GA tool.")
                #),
                box(title = "Main Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    box(
                      fileInput("fileGa", "Choose Input CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                      )
                    )
                ),
                box(title = "Optional Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      checkboxInput("ELEFAN_GA_seasonalised", "Seasonalised", FALSE),
                      numericInput("ELEFAN_GA_popSize", "Population size:", 50, min = 0, max = 10000, step=1),
                      numericInput("ELEFAN_GA_maxiter", "Maximum number of iterations to run before the GA search is halted:", 10, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_GA_run", "Number of consecutive generations without any improvement in the best fitness value before the GA is stopped:", 100, min = 1, max = 1000, step=1),
                      checkboxInput("ELEFAN_GA_addl.sqrt", "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE)
                    ),
                    box(
                      numericInput("ELEFAN_GA_pmutation", "Probability of mutation in a parent chromosome. Usually mutation occurs with a small probability:", 0.1, min = 0.1, max = 1, step=0.1),
                      numericInput("ELEFAN_GA_pcrossover", "Probability of crossover between pairs of chromosomes. Typically this is a large value:", 0.8, min = 0.1, max = 1, step=0.1),
                      numericInput("ELEFAN_GA_elitism", "Number of best fitness individuals to survive at each generation:", 5, min = 0, max = 100, step=1),
                      numericInput("ELEFAN_GA_MA", "Number indicating over how many length classes the moving average should be performed:", 5, min = 0, max = 100, step=1)
                    )
                ),
                box(title = "Low Par Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("ELEFAN_GA_lowPar_Linf", p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 119, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_GA_lowPar_K", "Curving coefficient (K):", 0.01, min = 0, max = 1, step=0.01),
                      numericInput("ELEFAN_GA_lowPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 0, min = 0, max = 1, step=0.1)
                    ),
                    box(
                      numericInput("ELEFAN_GA_lowPar_C", "Amplitude of growth oscillation (C):", 0, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_GA_lowPar_ts", p("Summer point (", withMathJax("\\(t_s\\)"), "):"), 0, min = 0, max = 1, step=0.1)
                    )
                ),
                box(title = "Up Par Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("ELEFAN_GA_upPar_Linf", p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 129, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_GA_upPar_K", "Curving coefficient (K):", 1, min = 0, max = 1, step=0.01),
                      numericInput("ELEFAN_GA_upPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 1, min = 0, max = 1, step=0.1)
                    ),
                    box(
                      numericInput("ELEFAN_GA_upPar_C", "Amplitude of growth oscillation (C):", 1, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_GA_upPar_ts", p("Summer point (", withMathJax("\\(t_s\\)"), "):"), 1, min = 0, max = 1, step=0.1)
                    )
                ),
                actionButton("go_ga", "Run ELEFAN GA"),
                hr(),
                
                box( width= 100, id = "box_elefan_ga_results",
                     title = "Results of Elefan GA Computation",
                     tags$style(type="text/css",
                                ".recalculating {opacity: 1.0;}"
                     ),
                     fluidRow(
                       box(
                         uiOutput("downloadReport_ga")
                       )
                     ),
                     fluidRow(
                       box(
                         htmlOutput("titlePlot1_elefan_ga"),
                         plotOutput("plot_ga_1")
                       ),
                       box(
                         htmlOutput("titlePlot2_elefan_ga"),
                         plotOutput("plot_ga_2")
                       )
                     ),
                     fluidRow (
                       box(
                         htmlOutput("titlePlot3_elefan_ga"),
                         plotOutput("plot_ga_3"),
                         plotOutput("plot_ga_5")
                       ),
                       box(
                         htmlOutput("titlePlot4_elefan_ga"),
                         plotOutput("plot_ga_4"),
                         htmlOutput("rnMax_ga"),
                         htmlOutput("par_ga"),
                         htmlOutput("title_tbl1_ga"),
                         tableOutput("tbl1_ga"),
                         htmlOutput("title_tbl2_ga"),
                         tableOutput("tbl2_ga")
                       )
                     )
                )
              )
      ),
      tabItem("ElefanSaWidget",
              htmlOutput("elefanSaTitle"),
              actionButton("elefanSADataConsiderations", "Data Considerations", class="topLevelInformationButton"),
              fluidRow(
                bsModal("modalExampleSA", "ELEFAN_SA Data Considerations", "elefanSADataConsiderations", size = "large", htmlOutput("elefanSADataConsiderationsText")),
                #box(title = "Data Considerations",
                #    width = NULL,
                #    collapsible = T, 
                #    class = "collapsed-box",
                #    checkboxInput("checkbox1", label = "Is your length-frequency data representative of the full population? (If this is not so, then estimates of fishing mortality will be biased.)", value = F),
                #    checkboxInput("checkbox2", label = "Were all age groups sampled?", value = F),
                #    checkboxInput("checkbox3", label = "Was the sample from a range of areas where different life histories might live? (e.g., if juveniles occupy nearshore habitat and adults are offshore)", value = F),
                #    checkboxInput("checkbox4", label = "Are a variety of gears with different selectivities used to collect the samples so that the samples contain multiple age groups?", value = F),
                #    p("**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of ", withMathJax("\\(L_\\infty\\)"), " and von Bertalanffy K in the Optional Parameters section in the ELEFAN_SA tool.")
                #),
                box(title = "Main Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    box(
                      fileInput("fileSa", "Choose Input CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                      )
                    )
                ),
                box(title = "Optional Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      checkboxInput("ELEFAN_SA_seasonalised", "Seasonalised", FALSE),
                      numericInput("ELEFAN_SA_initPar_Linf", p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 119, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_SA_initPar_K", "Curving coefficient (K):", 0.5, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_SA_initPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 0.5, min = 0, max = 1, step=0.01),
                      checkboxInput("ELEFAN_SA_addl.sqrt", "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE)
                    ),
                    box(
                      numericInput("ELEFAN_SA_SA_time", "Maximum running time in seconds:", 60, min = 0, max = 10000, step=1),
                      numericInput("ELEFAN_SA_SA_temp", "Initial value for temperature:", 100000, min = 1, max = 10000000, step=100),
                      numericInput("ELEFAN_SA_MA", "Number indicating over how many length classes the moving average should be performed:", 5, min = 0, max = 100, step=1),
                      numericInput("ELEFAN_SA_agemax", "Maximum age of species:", 1, min = 0, max = 100, step=1)
                    )
                ),
                box(title = "Low Par Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("ELEFAN_SA_lowPar_Linf", p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 119, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_SA_lowPar_K", "Curving coefficient (K):", 0.01, min = 0, max = 1, step=0.01),
                      numericInput("ELEFAN_SA_lowPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 0, min = 0, max = 1, step=0.1)
                    ),
                    box(
                      numericInput("ELEFAN_SA_lowPar_C", "Amplitude of growth oscillation (C):", 0, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_SA_lowPar_ts", p("Summer point (", withMathJax("\\(t_s\\)"), "):"), 0, min = 0, max = 1, step=0.1)
                    )
                ),
                box(title = "Up Par Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("ELEFAN_SA_upPar_Linf", p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 129, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_SA_upPar_K", "Curving coefficient (K):", 1, min = 0, max = 1, step=0.01),
                      numericInput("ELEFAN_SA_upPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 1, min = 0, max = 1, step=0.1)
                    ),
                    box(
                      numericInput("ELEFAN_SA_upPar_C", "Amplitude of growth oscillation (C):", 1, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_SA_upPar_ts", p("Summer point (", withMathJax("\\(t_s\\)"), "):"), 1, min = 0, max = 1, step=0.1)
                    )
                ),
                actionButton("go_sa", "Run ELEFAN SA"),
                hr(),
                
                box( width= 100, id = "box_elefan_sa_results",
                     title = "Results of Elefan SA Computation",
                     tags$style(type="text/css",
                                ".recalculating {opacity: 1.0;}"
                     ),
                     fluidRow(
                       box(
                         uiOutput("downloadReport_sa")
                       )
                     ),
                     fluidRow(
                       box(
                         htmlOutput("titlePlot1_elefan_sa"),
                         plotOutput("plot_sa_1")
                       ),
                       box(
                         htmlOutput("titlePlot2_elefan_sa"),
                         plotOutput("plot_sa_2")
                       )
                     ),
                     fluidRow (
                       box(
                         htmlOutput("titlePlot3_elefan_sa"),
                         plotOutput("plot_sa_3"),
                         plotOutput("plot_sa_5")
                       ),
                       box(
                         htmlOutput("titlePlot4_elefan_sa"),
                         plotOutput("plot_sa_4"),
                         htmlOutput("rnMax_sa"),
                         htmlOutput("par_sa"),
                         htmlOutput("title_tbl1_sa"),
                         tableOutput("tbl1_sa"),
                         htmlOutput("title_tbl2_sa"),
                         tableOutput("tbl2_sa")
                       )
                     )
                )
              )
      ),
      tabItem("ElefanWidget",
              htmlOutput("elefanTitle"),
              actionButton("elefanDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
              fluidRow(
                bsModal("modalExampleElefan", "ELEFAN Data Considerations", "elefanDataConsiderations", size = "large", htmlOutput("elefanDataConsiderationsText")),
                #box(title = "Data Considerations",
                #    width = NULL,
                #    collapsible = T, 
                #    class = "collapsed-box",
                #    checkboxInput("checkbox1", label = "Is your length-frequency data representative of the full population? (If this is not so, then estimates of fishing mortality will be biased.)", value = F),
                #    checkboxInput("checkbox2", label = "Were all age groups sampled?", value = F),
                #    checkboxInput("checkbox3", label = "Was the sample from a range of areas where different life histories might live? (e.g., if juveniles occupy nearshore habitat and adults are offshore)", value = F),
                #    checkboxInput("checkbox4", label = "Are a variety of gears with different selectivities used to collect the samples so that the samples contain multiple age groups?", value = F),
                #    p("**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of ", withMathJax("\\(L_\\infty\\)"), " and von Bertalanffy K in the Optional Parameters section in the ELEFAN tool.")
                #),
                box(title = "Main Parameters",
                    width = NULL, 
                    collapsible = T, 
                    class = "collapsed-box",
                    box(
                      fileInput("fileElefan", "Choose Input CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                      )
                    )
                ),
                box(title = "Optional Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("ELEFAN_Linf_fix", p("\\(L_\\infty\\)", ": if used the K-Scan method is applied with a fixed ", withMathJax("\\(L_\\infty\\)")," value (i.e. varying K only):"), NA, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_Linf_range_from", p(withMathJax("\\(L_\\infty\\)"), "sequence from:"), NULL, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_Linf_range_to", p(withMathJax("\\(L_\\infty\\)"), "sequence to:"), NULL, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_Linf_range_by", p(withMathJax("\\(L_\\infty\\)"), "increment sequence by:"), 1, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_C", "Growth oscillation amplitude (C)", 0, min = 0, max = 100, step=1),
                      numericInput("ELEFAN_ts", p("Onset of the first oscillation relative to summer point (", withMathJax("\\(t_s\\)"), "):"), 0, min = 0, max = 100, step=1),
                      numericInput("ELEFAN_MA", "Number indicating over how many length classes the moving average should be performed:", 5, min = 0, max = 100, step=1)
                    ),
                    box(
                      numericInput("ELEFAN_K_Range_from", "K sequence from:", NULL, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_K_Range_to", "K sequence to:", NULL, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_K_Range_by", "K increment sequence by:", 1, min = 1, max = 1000, step=1),
                      checkboxInput("ELEFAN_addl.sqrt", "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE),
                      numericInput("ELEFAN_agemax", "Maximum age of species:", NULL, min = 0, max = 100, step=1),
                      checkboxInput("ELEFAN_contour", "If checked in combination with response surface analysis, contour lines are displayed rather than the score as text in each field of the score plot", FALSE)
                    )
                ),
                actionButton("go", "Run ELEFAN"),
                hr(),
                
                box( width= 100,  id = "box_elefan_results",
                     title = "Results of Elefan Computation",
                     tags$style(type="text/css",
                                ".recalculating {opacity: 1.0;}"
                     ),
                     fluidRow(
                       box(
                         uiOutput("downloadReport")
                       )
                     ),
                     fluidRow(
                       box(
                         htmlOutput("titlePlot1_elefan"),
                         plotOutput("plot_1")
                       ),
                       box(
                         htmlOutput("titlePlot2_elefan"),
                         plotOutput("plot_2")
                       )
                     ),
                     fluidRow (
                       box(
                         htmlOutput("titlePlot3_elefan"),
                         plotOutput("plot_3"),
                         plotOutput("plot_5")
                       ),
                       box(
                         htmlOutput("titlePlot4_elefan"),
                         plotOutput("plot_4"),
                         htmlOutput("rnMax"),
                         htmlOutput("par"),
                         htmlOutput("title_tbl1_e"),
                         tableOutput("tbl1_e"),
                         htmlOutput("title_tbl2_e"),
                         tableOutput("tbl2_e")
                       )
                     )
                )
              )
      ),
      tabItem("SBPRWidget",
              htmlOutput("sbprTitle"),
              actionButton("SBPRDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
              fluidRow(
                bsModal("modalExampleSBPR", "SBPR Data Considerations", "SBPRDataConsiderations", size = "large", htmlOutput("SBPRDataConsiderationsText")),
                #box(title = "Data Considerations",
                #    width = NULL,
                #    collapsible = T, 
                #    class = "collapsed-box",
                #    checkboxInput("checkbox1", label = "Is the weight-at-age data representative of the full population, i.e., are all age groups sampled?", value = F),
                #    p("**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M in the Optional Parameters section.")
                #),
                box(title = "Main Parameters",
                    width = NULL, 
                    collapsible = T, 
                    class = "collapsed-box",
                    box(
                      fileInput("fileSbpr", "Choose Input CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                      )
                    )
                ),
                box(title = "Optional Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("SBPR_M", "Single natural mortality (M) rate if M is assumed constant over all ages:", 0.2, min = 0, max = 10, step=0.1),
                      numericInput("SBPR_pM", "Proportion of natural mortality that occurs before spawning:", 0.1667, min = 0, max = 10, step=0.0001),
                      numericInput("SBPR_maxF", "Maximum value of F range over which SBPR will be calculated:", 2, min = 0, max = 100, step=1)
                    ),
                    box(
                      numericInput("SBPR_pF", "Proportion of fishing mortality that occurs before spawning:", 0.2, min = 0, max = 10, step=0.1),
                      numericInput("SBPR_MSP", "Percentage of maximum spawning potential (percent MSP reference point) for which F and SBPR should be calculated:", 30, min = 0, max = 1000, step=1),
                      numericInput("SBPR_incrF", "F increment for SBPR calculation:", 0.001, min = 0, max = 10, step=0.001)
                    )
                ),
                actionButton("go_sbpr", "Run SBPR"),
                hr(),
                
                box( width= 100,  id = "box_sbpr_results",
                     title = "Results of Fishmethods - SBPR Computation",
                     tags$style(type="text/css",
                                ".recalculating {opacity: 1.0;}"
                     ),
                     fluidRow(
                       box(
                         uiOutput("downloadSbprReport")
                       )
                     ),
                     fluidRow(
                       box(
                         plotOutput("sbprOutPlot1"),
                         htmlOutput("sbprMSPTableTitle"),
                         tableOutput("sbprOutTable")
                       ), 
                       box(
                         plotOutput("sbprOutPlot2")
                       )
                     )
                )
              )
      ),
      tabItem("YPRWidget",
              htmlOutput("yprTitle"),
              actionButton("YPRDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
              fluidRow(
                bsModal("modalExampleYPR", "YPR Data Considerations", "YPRDataConsiderations", size = "large", htmlOutput("YPRDataConsiderationsText")),
                #box(title = "Data Considerations",
                #    width = NULL,
                #    collapsible = T, 
                #    class = "collapsed-box",
                #    checkboxInput("checkbox1", label = "Is the weight-at-age data representative of the full population, i.e., are all age groups sampled?", value = F),
                #    p("**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M in the Optional Parameters section.")
                #),
                box(title = "Main Parameters",
                    width = NULL, 
                    collapsible = T, 
                    class = "collapsed-box",
                    box(
                      fileInput("fileYpr", "Choose Input CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                      )
                    )
                ),
                box(title = "Optional Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("YPR_M", "Single natural mortality (M) rate if M is assumed constant over all ages", 0.2, min = 0, max = 10, step=0.1),
                      checkboxInput("YPR_Plus", "Plus -  logical value indicating whether the last age is a plus-group", TRUE),
                      numericInput("YPR_oldest", "if plus is checked, a numeric value indicating the oldest age in the plus group", 100, min = 0, max = 1000, step=1)
                    ),
                    box(
                      numericInput("YPR_maxF", "Maximum value of F range over which YPR will be calculated. YPR is calculated for F = 0 to maxF", 2, min = 0, max = 100, step=1),
                      numericInput("YPR_incrF", "F increment for SBPR calculation", 0.01, min = 0, max = 10, step=0.01)
                    )
                ),
                actionButton("go_YPR", "Run YPR"),
                hr(),
                
                box( width= 100,  id = "box_ypr_results",
                     title = "Results of Fishmethods - YPR Computation",
                     tags$style(type="text/css",
                                ".recalculating {opacity: 1.0;}"
                     ),
                     fluidRow(
                       box(
                         uiOutput("downloadYprReport")
                       )
                     ),
                     fluidRow(
                       box(
                         plotOutput("yprOutPlot")
                       ), 
                       box(
                         htmlOutput("yprDifference"),
                         hr(),
                         htmlOutput("<b>Ans Matrix</b>"),
                         tableOutput("yprOutTable"),
                         htmlOutput("yprFishingMortality")
                       )
                     )
                )
              )
      ),
      tabItem("BasicSchaefer",
              htmlOutput("basicShaeferTitle"),
              actionButton("basicShaeferMoreInfo", "More Information", class="topLevelInformationButton"),
              fluidRow(id = "box_shaefer_x",
                       bsModal("modalExample3", "Surplus production model", "basicShaeferMoreInfo", size = "large", htmlOutput("basicShaeferInfoText")),
                       box( width= 50,  id = "box_shaefer",
                            fluidRow(
                              box( id="box_shaefer_in",
                                   sliderInput("r", "Intrinsic rate of growth (r):", 
                                               min=0.01, max=1, value=0.5),    
                                   sliderInput("K", "Carrying capacity (K):", 
                                               min=500, max=3500, value=1000),
                                   withMathJax(),
                                   uiOutput('shaefer_ex1'),
                                   helpText('Once the parameters have been estimated, fishery performance indicators useful to fisheries management can be calculated.'),
                                   uiOutput('shaefer_ex2'),
                                   uiOutput('shaefer_ex3'),
                                   uiOutput('shaefer_ex4')
                              ),
                              box(
                                plotOutput("Biomassplot"),
                                plotOutput("Growthplot")
                              )
                            )
                       )
              )
      ),
      tabItem("BasicVonBertalannfy",
              htmlOutput("basicVonBertalannfyTitle"),
              actionButton("vonBertalannfyInfo", "More Information", class="topLevelInformationButton"),
              fluidRow(id = "box_vonbertalannfy_x",
                       bsModal("modalExample", "Generalized Von Bertalanffy Growth Function (VBGF)", "vonBertalannfyInfo", size = "large", htmlOutput("vonBertalannfyInfoText")),
                       box( width= 50,  id = "box_vonbertalannfy",
                            fluidRow(
                              box( id="box_vonbertalannfy_in",
                                   sliderInput("amax", "Age classes:", 
                                               min=1, max=100, value=50),    
                                   sliderInput("Linf", withMathJax("$$L_\\infty:$$"), 
                                               min=1, max=1000, value=100),
                                   sliderInput("k", "k:", 
                                               min = 0.01, max = 1, value = 0.1,step=0.01),
                                   sliderInput("t0", withMathJax("$$t_0:$$"),
                                               min = -5, max = 5, value = 0,step=0.1)
                              ),
                              box(
                                plotOutput("VBGFplot"),
                                h3(withMathJax(helpText('$$L_t = L_\\infty(1-e^{(-k(t-t_0))})$$')))
                              )
                            )
                       )
              )
              
      ),
      tabItem("SeasonalVonBertalannfy",
              htmlOutput("SeasonalVonBertalannfyTitle"),
              actionButton("SeasonalVonBertalannfyInfo", "More Information", class="topLevelInformationButton"),
              fluidRow(id = "box_vonbertalannfy_x",
                       bsModal("modalExample4", "Seasonal Von Bertalanffy Growth Function (VBGF)", "SeasonalVonBertalannfyInfo", size = "large", htmlOutput("seasonalVonBertalannfyInfoText")),
                       box( width= 50,  id = "box_seasonal_vonbertalannfy",
                            fluidRow(
                              box( id="box_seasonal_vonbertalannfy_in",
                                   sliderInput("samax", "Age classes:", 
                                               min=1, max=50, value=5),    
                                   sliderInput("sLinf", withMathJax("$$L_\\infty:$$"), 
                                               min=1, max=500, value=21),
                                   sliderInput("sk", "k:", 
                                               min = 0.01, max = 1, value = 0.8,step=0.01),
                                   sliderInput("st0", withMathJax("$$t_0:$$"),
                                               min = -2, max = 2, value = 0,step=0.01) ,
                                   sliderInput("sts", withMathJax("$$t_s:$$"),
                                               min = 0, max = 1, value = 0.5,step=0.01),
                                   sliderInput("sC", "C (amplitude):",
                                               min = 0, max = 1, value = 1,step=0.01) 
                              ),
                              box(
                                plotOutput("SVBGFplot"),
                                h3(strong(withMathJax(helpText('$$L_t = L_\\infty(1-e^{(-k(t-t_0)-\\frac{Ck}{2\\pi}sin2\\pi(t-t_s))})$$'))))
                              )
                            )
                       )
              ) 
      ),
      tabItem("NaturalMortality",
              htmlOutput("naturalMortalityTitle"),
              actionButton("naturalMortalityInfo", "More Information", class="topLevelInformationButton"),
              fluidRow(id = "box_naturalMortality_x",
                       bsModal("modalExample2", "Estimating Natural Mortality (M) from FishBase life history parameters", "naturalMortalityInfo", size = "large", htmlOutput("naturalMortalityInfoText")),
                       box( width= 50,  id = "box_naturalMortality",
                            fluidRow(
                              box( id="box_naturalMortality_in",
                                   textInput("Species", label="Type genus and species name found in FishBase:", value = "Gadus morhua"),
                                   actionButton(label="Submit", inputId="nm_sub_fb"),
                                   numericInput("Amax", "Maximum age (years):", value=NA,min=1, max=300, step=0.1),
                                   numericInput("Linf","Linf (in cm):", value=NA,min=1, max=1000, step=0.01),
                                   numericInput("k", "VBGF Growth coeff. k:", value=NA,min = 0.001, max = 1,step=0.01),
                                   numericInput("t0", "VBGF age at size 0 (t_0)", value=NA,min = -15, max = 15,step=0.01),
                                   numericInput("Amat","Age at maturity (years)", value=NA,min = 0.01, max = 100,step=0.01),
                                   numericInput("Temp","Water temperature (in C):" , value=NA,min = 0.001, max = 60,step=0.01),
                                   numericInput("Bl","Body length (cm):",value=NA,min = 0.01, max = 10000,step=0.01),
                                   numericInput("User_M","User M input (Type value and hit 'Enter'):",value=NA,min = 0, max = 10,step=0.001),
                                   h4(em(tags$b("FishBase values for species of interest:"))),
                                   column(1,tableOutput("Ftable")),
                                   
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   h4("Composite M: method weighting"),
                                   h5(p(em("Allows for weighting of the contribution of each method in the composite M distribution"))),
                                   h5("Values range from 0 to 1. A value of 0 removes the contribution; a value of 1 is full weighting."),
                                   h5("Default values are based on redundancies of methods using similar information."),
                                   h5("For instance,the four max. age methods are given a weight of 0.25, so all weighted together equal 1"),
                                   wellPanel(
                                     fluidRow(
                                       column(4,numericInput("Then_Amax_1","Then_Amax 1",value=0.25,min = 0, max = 1,step=0.001)),
                                       column(4,numericInput("Then_Amax_2","Then_Amax 2",value=0.25,min = 0, max = 1,step=0.001)),
                                       column(4,numericInput("Then_Amax_3","Then_Amax 3",value=0.25,min = 0, max = 1,step=0.001))
                                     ),
                                     fluidRow(
                                       column(4,numericInput("Hamel_Amax","Hamel_Amax",value=0.25,min = 0, max = 1,step=0.001)),
                                       column(4,numericInput("AnC","AnC",value=0,min = 0, max = 1,step=0.001)),
                                       column(4,numericInput("Then_VBGF","Then_VBGF",value=0.34,min = 0, max = 1,step=0.001))
                                     ),
                                     fluidRow(
                                       column(4,numericInput("Jensen_VBGF_1","Jensen_VBGF 1",value=0.33,min = 0, max = 1,step=0.001)),
                                       column(4,numericInput("Jensen_VBGF_2","Jensen_VBGF 2",value=0.33,min = 0, max = 1,step=0.001)),
                                       column(4,numericInput("Pauly_lt","Pauly_lt",value=0.5,min = 0, max = 1,step=0.001))
                                     ),
                                     fluidRow(
                                       column(4,numericInput("Gislason","Gislason",value=1,min = 0, max = 1,step=0.001)),
                                       column(4,numericInput("Chen_Wat","Chen-Wat",value=0.5,min = 0, max = 1,step=0.001)),
                                       column(4,numericInput("Roff","Roff",value=0.5,min = 0, max = 1,step=0.001))
                                     ),
                                     fluidRow(
                                       column(4,numericInput("Jensen_Amat","Jensen_Amat",value=0.5,min = 0, max = 1,step=0.001)),
                                       column(4,numericInput("Ri_Ef_Amat","Ri_Ef_Amat",value=0.5,min = 0, max = 1,step=0.001))
                                     ),
                                     fluidRow(
                                       column(4,numericInput("UserM","User M",value=1,min = 0, max = 1,step=0.001)))
                                   )
                              ),
                              box(
                                h4("Natural mortality (M) estimates by method"),
                                plotOutput("Mplot"),
                                h4("Natural mortality (M) values"),
                                fluidRow(
                                  column(6,tableOutput("Mtable")),
                                  column(6,tableOutput("Mtable2")),
                                  div(class="divDividerMain",
                                      downloadButton('downloadMs', 'Download M values'),
                                      downloadButton('downloadCW_M_a', 'Download Chen-Wat. age-specific M values'),
                                      h4("Composite natural mortality"),
                                      h5(p(em("Blue vertical line indicates median value")))),
                                  div(class="divDividerMain2",
                                      plotOutput("Mcomposite"),
                                      downloadButton('downloadMcompositedensityplot', 'Download composite M density plot'),
                                      downloadButton('downloadMcompositedist', 'Download composite M for resampling'))
                                )
                              )
                            )
                       )
              )
      )
    )
  )
), tags$footer("This work has received funding from the European Union's Horizon 2020 research and innovation programme under the BlueBRIDGE project (Grant agreement No 675680) and in-kind from NOAA", 
               align = "center")
)


server <- function(input, output, session) {
  
  cmsy <- reactiveValues()
  elefan_ga <- reactiveValues()
  elefan_sa <- reactiveValues()
  elefan <- reactiveValues()
  sbprExec <- reactiveValues()
  yprExec <- reactiveValues()
  fishingMortality <- reactiveValues()
  
  fishingMortality$FcurrGA <- NA
  fishingMortality$FcurrSA <- NA
  fishingMortality$Fcurr <- NA
  
  ####### OBSERVERS #######
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
    
    vreToken <- "96d4f92a-ef32-42ab-a47a-425ee5618644-843339462"
    
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
    }
  })
  
  ### observeEvent(input$go, {
  ###   infile <- input$fileElefan
  
  ###if (is.null(infile)) {
  ### showModal(modalDialog(
  ###   title = "Error",
  ###   "No input file selected",
  ###   easyClose = TRUE,
  ###   footer = NULL
  ### ))
  ### return(NULL)
  ###}
  ###js$showComputing()
  ###inputCsvFile <- infile$datapath
  ###js$removeBox("box_elefan_results")
  ###js$disableAllButtons()
  ###dataset <- read_elefan_csv(inputCsvFile)
  ###ds <- lfqModify(lfqRestructure(dataset), bin_size = 4)
  
  ###   #ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
  
  ###elefan_linf_range <- NA
  ###if (!is.na(input$ELEFAN_Linf_range_from) && !is.na(input$ELEFAN_Linf_range_to)) {
  ###elefan_linf_range <- seq(from = input$ELEFAN_Linf_range_from, to = input$ELEFAN_Linf_range_to, by = input$ELEFAN_Linf_range_by)
  ###}
  
  ###elefan_k_range <- exp(seq(log(0.1), log(10), length.out=100))
  ###if (!is.na(input$ELEFAN_K_Range_from) && !is.na(input$ELEFAN_K_range_to)) {
  ###elefan_linf_range <- seq(from = input$ELEFAN_K_Range_from, to = input$ELEFAN_K_range_to, by = input$ELEFAN_K_range_by)
  ###}
  
  
  ###elefan_agemax <- input$ELEFAN_agemax 
  ###if (is.na(input$ELEFAN_agemax)) {
  ###elefan_agemax <- NULL
  ###}
  ###res <- run_elefan(ds, binSize = 4, Linf_fix = input$ELEFAN_Linf_fix, Linf_range = elefan_linf_range, K_range = elefan_k_range,
  ###C = input$ELEFAN_C, ts = input$ELEFAN_ts, MA = input$ELEFAN_MA, addl.sqrt = input$ELEFAN_addl.sqrt,
  ###agemax = elefan_agemax, contour = input$ELEFAN_contour)
  ###js$hideComputing()
  ###js$enableAllButtons()
  ###if ('error' %in% names(res)) {
  ###showModal(modalDialog(
  ###title = "Error",
  ###res$error,
  ###easyClose = TRUE,
  ###footer = NULL
  ###))
  ###} else {
  ###js$showBox("box_elefan_results")
  ###elefan$results <- res
  ###}
  ###})
  
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
    filename = paste("cmsy_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "cmsyReportSingle.Rmd")
      file.copy("assets/cmsy/cmsyReportSingle.Rmd", tempReport, overwrite = TRUE)
      
      
      if (!is.null(cmsy$method$analisysChartUrl)) {
        fileAnalisysChart <- tempfile(fileext=".jpg")
        download.file(cmsy$method$analisysChartUrl, fileAnalisysChart)
        cmsy$method$analisysChart <- fileAnalisysChart
      }
      if (!is.null(cmsy$method$analisysChartUrl)) {
        fileManagementChart <- tempfile(fileext=".jpg")
        download.file(cmsy$method$managementChartUrl, fileManagementChart)
        cmsy$method$managementChart <- fileManagementChart
      }
      
      # Set up parameters to pass to Rmd document
      params <- list(cmsy = cmsy)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file, params = params)
      
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
  output$cmsyDataConsiderationsText <- renderText({
    text <- "<h5><p><b>Please ensure your time-series at least 15 years in length from starting year to ending year.<br> (Note that years with missing data should be filled with an 'NA' value.</b></p></h5>"
    text <- paste0(text, "<h5>", "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of natural mortality (M) for the Optional Parameters section.", "</h5>")
    text
  })
  ####### END CMSY TEXT #######
  
  
  getDataConsiderationTextForElefan <- function() {
    text <- "<ul>"
    text <- paste0(text, "<li>", "Ensure that your length-frequency data is representative of the full population. (If this is not so, then estimates of fishing mortality will be biased.)", "</li>")
    text <- paste0(text, "<li>", "Ensure that all age groups were sampled.", "</li>")
    text <- paste0(text, "<li>", "Ensure that the sample was from a range of areas where different life histories might live. (e.g., if juveniles occupy nearshore habitat and adults are offshore)", "</li>")
    text <- paste0(text, "<li>", "Ensure that a variety of gears with different selectivities used to collect the samples so that the samples contain multiple age groups.", "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<h5>", "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of ", withMathJax("\\(L_\\infty\\)"), " and von Bertalanffy K in the Optional Parameters section in the %%ELEFAN%% tool.", "</h5>")
    return (text)
  }
  
  ######### ELEFAN GA METHOD #########
  observeEvent(input$go_ga, {
    infile <- input$fileGa
    
    if (is.null(infile)) {
      showModal(modalDialog(
        title = "Error",
        "No input file selected",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    js$showComputing()
    inputCsvFile <- infile$datapath
    js$removeBox("box_elefan_ga_results")
    js$disableAllButtons()
    dataset <- read_elefan_csv(inputCsvFile)
    ds <- lfqModify(lfqRestructure(dataset), bin_size = 4)
    
    res <- run_elefan_ga(ds,binSize =  4, seasonalised = input$ELEFAN_GA_seasonalised, 
                         low_par = list(Linf = input$ELEFAN_GA_lowPar_Linf, K = input$ELEFAN_GA_lowPar_K, t_anchor = input$ELEFAN_GA_lowPar_t_anchor, C = input$ELEFAN_GA_lowPar_C, ts = input$ELEFAN_GA_lowPar_ts),
                         up_par = list(Linf = input$ELEFAN_GA_upPar_Linf, K = input$ELEFAN_GA_upPar_K, t_anchor = input$ELEFAN_GA_upPar_t_anchor, C = input$ELEFAN_GA_upPar_C, ts = input$ELEFAN_GA_upPar_ts),
                         popSize = input$ELEFAN_GA_popSize, maxiter = input$ELEFAN_GA_maxiter, run = input$ELEFAN_GA_run, pmutation = input$ELEFAN_GA_pmutation, pcrossover = input$ELEFAN_GA_pcrossover,
                         elitism = input$ELEFAN_GA_elitism, MA = input$ELEFAN_GA_MA, addl.sqrt = input$ELEFAN_GA_addl.sqrt)
    
    js$hideComputing()
    js$enableAllButtons()
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      js$showBox("box_elefan_ga_results")
      elefan_ga$results <- res
      fishingMortality$FcurrGA <- round(elefan_ga$results$plot3$currents[4]$curr.F, 2)
    }
  })
  output$plot_ga_1 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot1, Fname = "catch", date.axis = "modern")
    }
  })
  output$plot_ga_2 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot2, Fname = "rcounts", date.axis = "modern")
    }
  })
  output$plot_ga_3 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot3, mark = TRUE)
      mtext("(a)", side = 3, at = -1, line = 0.6)
    }
  })
  output$plot_ga_4 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot4, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
      mtext("(b)", side = 3, at = -0.1, line = 0.6)
    }
  })
  output$plot_ga_5 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$data)
    }
  })
  output$par_ga <- renderText({
    if ("results" %in% names(elefan_ga)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity (", withMathJax("\\(L_\\infty\\)"), "in cm):</strong>&nbsp;", round(elefan_ga$results$data$par$Linf, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient (K):</strong>&nbsp;", round(elefan_ga$results$data$par$K, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month (t_anchor):</strong>&nbsp;", round(elefan_ga$results$data$par$t_anchor, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation (NOTE: only if 'Seasonalized' is checked; C):</strong>&nbsp;", ifelse(is.na(elefan_ga$results$data$par$C), NA, round(elefan_ga$results$data$par$C, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Winter point of oscillation (</strong>&nbsp;", withMathJax("\\(t_w\\)") , "<strong>)</strong>&nbsp;")
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (NOTE: only if 'Seasonalized' is checked; ", withMathJax("\\(ts\\)"),"=", withMathJax("\\(t_w\\)"), "- 0.5):</strong>&nbsp;", ifelse(is.na(elefan_ga$results$data$par$ts), NA, round(elefan_ga$results$data$par$ts, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", ifelse(is.na(elefan_ga$results$data$par$phiL), "--", round(elefan_ga$results$data$par$phiL, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<br>")
      title
    } else {  "" }
  })
  output$downloadReport_ga <- renderUI({
    if ("results" %in% names(elefan_ga)) {
      downloadButton('createElefanGAReport', 'Download Report')
    }
  })
  output$createElefanGAReport <- downloadHandler(
    filename = paste("ElefanGA_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "elefan_ga.Rmd")
      file.copy("assets/tropFishR/elefan_ga.Rmd", tempReport, overwrite = TRUE)
      params <- list(elefan = elefan_ga)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  output$tbl1_ga <- renderTable({
    if ('results' %in% names(elefan_ga)) {
      elefan_ga$results$plot3$df_Es
    }
  }, 
  include.rownames=TRUE)
  output$tbl2_ga <- renderTable({
    if ('results' %in% names(elefan_ga)) {
      CURR_GA<-elefan_ga$results$plot3$currents
      CURR_GA<-CURR_GA[,-7]
      names(CURR_GA)<-c("Length-at-1st-capture (Lc)", "Age-at-1st-capture (tc)", "Effort","Fishing mortality", "Catch", "Yield", "Biomass")
      CURR_GA
    }
  }, 
  include.rownames=TRUE)
  output$downloadReport_ga <- renderUI({
    if ("results" %in% names(elefan_ga)) {
      downloadButton('createElefanGAReport', 'Download Report')
    }
  })
  output$createElefanGAReport <- downloadHandler(
    filename = paste("ElefanGA_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "elefan_ga.Rmd")
      file.copy("assets/tropFishR/elefan_ga.Rmd", tempReport, overwrite = TRUE)
      params <- list(elefan = elefan_ga)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  output$downloadReport_ga <- renderUI({
    if ("results" %in% names(elefan_ga)) {
      downloadButton('createElefanGAReport', 'Download Report')
    }
  })
  output$createElefanGAReport <- downloadHandler(
    filename = paste("ElefanGA_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "elefan_ga.Rmd")
      file.copy("assets/tropFishR/elefan_ga.Rmd", tempReport, overwrite = TRUE)
      params <- list(elefan = elefan_ga)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  output$title_tbl1_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Biological reference levels:</p>"
      txt
    }
  })
  output$title_tbl2_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Current levels:</p>"
      txt
    }
  })
  output$titlePlot1_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Restructured LFQ data</p>"
      txt
    }
  })
  output$titlePlot3_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
      txt
    }
  })
  output$titlePlot4_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
      txt
    }
  })
  output$titleResultsOfTheComputation_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<h2>Results of the ELEFAN_GA computation</h2>"
      txt
    }
  })
  
  output$elefanGADataConsiderationsText <- renderText({
    text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getDataConsiderationTextForElefan())
    text
  })
  ####### END ELEFAN GA METHOD #######
  
  
  ######### ELEFAN SA METHOD #########
  observeEvent(input$go_sa, {
    infile <- input$fileSa
    
    if (is.null(infile)) {
      showModal(modalDialog(
        title = "Error",
        "No input file selected",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    js$showComputing()
    inputCsvFile <- infile$datapath
    js$removeBox("box_elefan_ga_results")
    js$disableAllButtons()
    dataset <- read_elefan_csv(inputCsvFile)
    #ds1 <- lfqModify(lfqRestructure(dataset), bin_size = 4)
    
    #ds2 <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    
    res <- run_elefan_sa(dataset,binSize =  4, seasonalised = input$ELEFAN_SA_seasonalised, 
                         init_par = list(Linf = input$ELEFAN_SA_initPar_Linf, K = input$ELEFAN_SA_initPar_K, t_anchor = input$ELEFAN_SA_initPar_t_anchor),
                         low_par = list(Linf = as.numeric(input$ELEFAN_SA_lowPar_Linf), K = as.numeric(input$ELEFAN_SA_lowPar_K), t_anchor = as.numeric(input$ELEFAN_SA_lowPar_t_anchor), C = as.numeric(input$ELEFAN_SA_lowPar_C), ts = as.numeric(input$ELEFAN_SA_lowPar_ts)),
                         up_par = list(Linf = as.numeric(input$ELEFAN_SA_upPar_Linf), K = as.numeric(input$ELEFAN_SA_upPar_K), t_anchor = as.numeric(input$ELEFAN_SA_upPar_t_anchor), C = as.numeric(input$ELEFAN_SA_upPar_C), ts = as.numeric(input$ELEFAN_SA_upPar_ts)),
                         SA_time = input$ELEFAN_SA_SA_time, SA_temp = input$ELEFAN_SA_SA_temp, MA = input$ELEFAN_SA_MA, addl.sqrt = input$ELEFAN_SA_addl.sqrt,
                         agemax = input$ELEFAN_SA_agemax)
    js$hideComputing()
    js$enableAllButtons()
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      print(res$plot3)
      js$showBox("box_elefan_sa_results")
      elefan_sa$results <- res
      fishingMortality$FcurrSA <- round(elefan_sa$results$plot3$currents[4]$curr.F, 2)
    }
  })
  output$tbl1_sa <- renderTable({
    if ('results' %in% names(elefan_sa)) {
      elefan_sa$results$plot3$df_Es
    }
  }, 
  include.rownames=TRUE)
  output$tbl2_sa <- renderTable({
    if ('results' %in% names(elefan_sa)) {
      CURR_SA<-elefan_sa$results$plot3$currents
      CURR_SA<-CURR_SA[,-7]
      names(CURR_SA)<-c("Length-at-1st-capture (Lc)", "Age-at-1st-capture (tc)", "Effort","Fishing mortality", "Catch", "Yield", "Biomass")
      CURR_SA
    }
  }, 
  include.rownames=TRUE)
  output$plot_sa_1 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot1, Fname = "catch", date.axis = "modern")
    }
  })
  output$plot_sa_2 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot2, Fname = "rcounts", date.axis = "modern")
    }
  })
  output$plot_sa_3 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot3, mark = TRUE)
      mtext("(a)", side = 3, at = -1, line = 0.6)
    }
  })
  output$plot_sa_4 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot4, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
      mtext("(b)", side = 3, at = -0.1, line = 0.6)
    }
  })
  output$plot_sa_5 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$data)
    }
  })
  output$par_sa <- renderText({
    if ("results" %in% names(elefan_sa)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity (", withMathJax("\\(L_\\infty\\)"), "in cm):</strong>&nbsp;", round(elefan_sa$results$data$par$Linf, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient (K):</strong>&nbsp;", round(elefan_sa$results$data$par$K, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month (t_anchor):</strong>&nbsp;", round(elefan_sa$results$data$par$t_anchor, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation (NOTE: only if 'Seasonalized' is checked; C):</strong>&nbsp;", ifelse(is.na(elefan_sa$results$data$par$C), NA, round(elefan_sa$results$data$par$C, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Winter point of oscillation (</strong>&nbsp;", withMathJax("\\(t_w\\)") , "<strong>)</strong>&nbsp;")
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (NOTE: only if 'Seasonalized' is checked; ", withMathJax("\\(ts\\)"),"=", withMathJax("\\(t_w\\)"), "- 0.5):</strong>&nbsp;", ifelse(is.na(elefan_sa$results$data$par$ts), NA, round(elefan_sa$results$data$par$ts, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", ifelse(is.na(elefan_sa$results$data$par$phiL), NA, round(elefan_sa$results$data$par$phiL, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<br>")
      title
    } else {  "" }
  })
  output$downloadReport_sa <- renderUI({
    if ("results" %in% names(elefan_sa)) {
      downloadButton('createElefanSAReport', 'Download Report')
    }
  })
  output$title_tbl1_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Biological reference levels:</p>"
      txt
    }
  })
  output$title_tbl2_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Current levels:</p>"
      txt
    }
  })
  output$titlePlot1_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Restructured LFQ data</p>"
      txt
    }
  })
  output$titlePlot3_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
      txt
    }
  })
  output$titlePlot4_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
      txt
    }
  })
  output$titleResultsOfTheComputation_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<h2>Results of the ELEFAN_SA computation</h2>"
      txt
    }
  })
  output$elefanSADataConsiderationsText <- renderText({
    text <- gsub("%%ELEFAN%%", "ELEFAN_SA", getDataConsiderationTextForElefan())
    text
  })
  ####### END ELEFAN SA METHOD #######
  
  
  ######### ELEFAN METHOD #########
  observeEvent(input$go, {
    infile <- input$fileElefan
    
    if (is.null(infile)) {
      showModal(modalDialog(
        title = "Error",
        "No input file selected",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    js$showComputing()
    inputCsvFile <- infile$datapath
    js$removeBox("box_elefan_results")
    js$disableAllButtons()
    dataset <- read_elefan_csv(inputCsvFile)
    ds <- lfqModify(lfqRestructure(dataset), bin_size = 4)
    
    #ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    
    elefan_linf_range <- NA
    if (!is.na(input$ELEFAN_Linf_range_from) && !is.na(input$ELEFAN_Linf_range_to)) {
      elefan_linf_range <- seq(from = input$ELEFAN_Linf_range_from, to = input$ELEFAN_Linf_range_to, by = input$ELEFAN_Linf_range_by)
    }
    
    elefan_k_range <- exp(seq(log(0.1), log(10), length.out=100))
    if (!is.na(input$ELEFAN_K_Range_from) && !is.na(input$ELEFAN_K_range_to)) {
      elefan_linf_range <- seq(from = input$ELEFAN_K_Range_from, to = input$ELEFAN_K_range_to, by = input$ELEFAN_K_range_by)
    }
    
    
    elefan_agemax <- input$ELEFAN_agemax 
    if (is.na(input$ELEFAN_agemax)) {
      elefan_agemax <- NULL
    }
    res <- run_elefan(ds, binSize = 4, Linf_fix = input$ELEFAN_Linf_fix, Linf_range = elefan_linf_range, K_range = elefan_k_range,
                      C = input$ELEFAN_C, ts = input$ELEFAN_ts, MA = input$ELEFAN_MA, addl.sqrt = input$ELEFAN_addl.sqrt,
                      agemax = elefan_agemax, contour = input$ELEFAN_contour)
    js$hideComputing()
    js$enableAllButtons()
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      js$showBox("box_elefan_results")
      elefan$results <- res
      fishingMortality$Fcurr <- round(elefan$results$plot3$currents[4]$curr.F, 2)
    }
  })
  output$plot_1 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$plot1, Fname = "catch", date.axis = "modern")
    }
  })
  output$plot_2 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$plot2, Fname = "rcounts", date.axis = "modern")
    }
  })
  output$plot_3 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$plot3, mark = TRUE)
      mtext("(a)", side = 3, at = -1, line = 0.6)
    }
  })
  output$plot_4 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$plot4, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
      mtext("(b)", side = 3, at = -0.1, line = 0.6)
    }
  })
  output$plot_5 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$data)
    }
  })
  output$tbl1_e <- renderTable({
    if ('results' %in% names(elefan)) {
      elefan$results$plot3$df_Es
    }
  }, 
  include.rownames=TRUE)
  output$tbl2_e <- renderTable({
    if ('results' %in% names(elefan)) {
      CURR<-elefan$results$plot3$currents
      CURR<-CURR[,-7]
      names(CURR)<-c("Length-at-1st-capture (Lc)", "Age-at-1st-capture (tc)", "Effort","Fishing mortality", "Catch", "Yield", "Biomass")
      CURR
    }
  }, 
  include.rownames=TRUE)
  output$downloadReport <- renderUI({
    if ("results" %in% names(elefan)) {
      downloadButton('createElefanReport', 'Download Report')
    }
  })
  output$createElefanReport <- downloadHandler(
    filename = paste("Elefan_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "elefan.Rmd")
      file.copy("assets/tropFishR/elefan.Rmd", tempReport, overwrite = TRUE)
      params <- list(elefan = elefan)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  output$createElefanSAReport <- downloadHandler(
    filename = paste("ElefanSA_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "elefan_sa.Rmd")
      file.copy("assets/tropFishR/elefan_sa.Rmd", tempReport, overwrite = TRUE)
      params <- list(elefan = elefan_sa)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  output$par <- renderText({
    if ("results" %in% names(elefan)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity (", withMathJax("\\(L_\\infty\\)"), "in cm):</strong>&nbsp;", elefan$results$data$par$Linf)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient (K):</strong>&nbsp;", elefan$results$data$par$K)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month (t_anchor):</strong>&nbsp;", round(elefan$results$data$par$t_anchor, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation (C):</strong>&nbsp;", elefan$results$data$par$C)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Winter point of oscillation (</strong>&nbsp;", withMathJax("\\(t_w\\)") , "<strong>)</strong>&nbsp;")
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (", withMathJax("\\(ts\\)"),"=", withMathJax("\\(t_w\\)"), "- 0.5):</strong>&nbsp;", round(elefan$results$data$par$ts, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", round(elefan$results$data$par$phiL, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<br>")
      title
    } else {  "" }
  })
  output$elefanDataConsiderationsText <- renderText({
    text <- gsub("%%ELEFAN%%", "ELEFAN", getDataConsiderationTextForElefan())
    text
  })
  ####### END ELEFAN METHOD #######
  
  
  ######### SBPR METHOD #########
  observeEvent(input$go_sbpr, {
    infile <- input$fileSbpr
    
    if (is.null(infile)) {
      showModal(modalDialog(
        title = "Error",
        "No input file selected",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    js$showComputing()
    js$removeBox("box_sbpr_results")
    inputCsvFile <- infile$datapath
    dat <- read.csv(inputCsvFile)
    
    res <- sbpr_shinyApp(age=dat$age,ssbwgt=dat$ssbwgt,partial=dat$partial,pmat=dat$pmat,M=input$SBPR_M,pF=input$SBPR_pF, pM=input$SBPR_pM,MSP=input$SBPR_MSP,plus=FALSE,maxF=input$SBPR_maxF,incrF=input$SBPR_incrF, graph=FALSE)
    js$hideComputing()
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      sbprExec$results <- res
      js$showBox("box_sbpr_results")
    }
  })
  
  #output$sbprFishingMortality <- renderText({
  #  if (is.na(fishingMortality$FcurrGA) && is.na(fishingMortality$FcurrSA) && is.na(fishingMortality$Fcurr)) {
  #    text <- "&nbsp;&nbsp;<strong>To calculate the fishing mortality run an ELEFAN method before SBPR</strong>"
  #  } else {
  #    text <- ""
  #    if (!is.na(fishingMortality$Fcurr)) {
  #      text <- paste0(text, fishingMortality$Fcurr, "<br/>")
  #    }
  #    if (!is.na(fishingMortality$FcurrGA)) {
  #      text <- paste0(text, fishingMortality$FcurrGA, "<br/>")
  #    }
  #    if (!is.na(fishingMortality$FcurrSA)) {
  #      text <- paste0(text, fishingMortality$FcurrSA, "<br/>")
  #    }
  #  }
  #  text
  #})
  
  output$sbprOutPlot1 <- renderPlot({
    if ('results' %in% names(sbprExec)) {
      plot(sbprExec$results$F_vs_SPR[,2]~sbprExec$results$F_vs_SPR[,1],ylab="SPR",xlab="Fishing Mortality (F)",type="l")
      abline(h=sbprExec$results$Reference_Point[1,2], col = "red", lty = 2)
      legend(1.4, 8, legend=c("SSB_per_recruit"),col=c("red"), lty=2, cex=0.9)
    }
  })
  output$sbprOutPlot2 <- renderPlot({
    if ('results' %in% names(sbprExec)) {
      plot(sbprExec$results$F_vs_SPR[,3]~sbprExec$results$F_vs_SPR[,1],ylab="% Max SPR",xlab="Fishing Mortality (F)",type="l")
      abline(h=input$SBPR_MSP, v = sbprExec$results$Reference_Point[1,1], col = "red", lty = 2)
      leg <- paste0(input$SBPR_MSP, "% MSP")
      legend(1.5, 85, legend=c(leg),col=c("red"), lty=2, cex=0.9)
    }
  })
  output$sbprMSPTableTitle <- renderText({
    if ('results' %in% names(sbprExec)) {
      title <- paste0("&nbsp;&nbsp;&nbsp;&nbsp;<b>F", input$SBPR_MSP, "% MSP</b>")
      title
    }
  })
  output$sbprOutTable <- renderTable({
    if ('results' %in% names(sbprExec)) {
      df <- as.data.frame(sbprExec$results$Reference_Point)
      
      if (!is.na(fishingMortality$Fcurr)) {
        df$Fcurr <- fishingMortality$Fcurr
      }
      else if (!is.na(fishingMortality$FcurrGA)) {
        df$Fcurr <- fishingMortality$FcurrGA
      }
      else if (!is.na(fishingMortality$FcurrSA)) {
        df$Fcurr <- fishingMortality$FcurrSA
      } else {
        df$Fcurr <- "You need to estimate Fcurrent before calculating F30%MSPR, using ELEFAN method if you have lengnth frequency data. The data used for ELEFAN and SBPR analysis should come from the same fish stock."  
      }
      colnames(df) <- c("F", "SSB per recruit", "Fishing mortality")
      df
    }
  }, 
  include.rownames=FALSE, align="c")
  output$downloadSbprReport <- renderUI({
    if ("results" %in% names(sbprExec)) {
      downloadButton('createSbprReport', 'Download Report')
    }
  })
  output$createSbprReport <- downloadHandler(
    filename = paste("Sbpr_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "sbpr.Rmd")
      file.copy("assets/fishmethods/sbpr.Rmd", tempReport, overwrite = TRUE)
      sbprExec$perc <- input$SBPR_MSP
      params <- list(sbprExec = sbprExec)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  output$SBPRDataConsiderationsText <- renderText({
    text <- "<h5><b>Ensure that spawning stock weight-at-age data is representative of the full population, i.e., are all age groups sampled?</b></h5>"
    text <- paste0(text, "<h5>", "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M in the Optional Parameters section.", "</h5>")
    text
  })
  ######### END SBPR METHOD #########
  
  
  ######### YPR METHOD #########
  observeEvent(input$go_YPR, {
    infile <- input$fileYpr
    
    if (is.null(infile)) {
      showModal(modalDialog(
        title = "Error",
        "No input file selected",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    js$showComputing()
    js$removeBox("box_ypr_results")
    inputCsvFile <- infile$datapath
    dat <- read.csv(inputCsvFile)
    
    res <- ypr_shinyApp(age=dat$age,wgt=dat$ssbwgt,partial=dat$partial,M=input$YPR_M,plus=input$YPR_Plus,oldest=input$YPR_oldest,maxF=input$YPR_maxF,incrF=input$YPR_incrF, graph = FALSE)
    js$hideComputing()
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      yprExec$results <- res
      js$showBox("box_ypr_results")
    }
  })
  output$yprFishingMortality <- renderText({
    if (is.na(fishingMortality$FcurrGA) && is.na(fishingMortality$FcurrSA) && is.na(fishingMortality$Fcurr)) {
      text <- "<strong>You need to estimate Fcurrent before calculating YPR, using ELEFAN method if you have lengnth frequency data. The data used for ELEFAN and YPR analysis should come from the same fish stock.</strong>"
    } else {
      text <- ""
      if (!is.na(fishingMortality$Fcurr)) {
        text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN method: </strong>", fishingMortality$Fcurr, "<br/>")
      }
      if (!is.na(fishingMortality$FcurrGA)) {
        text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN GA method: </strong>", fishingMortality$FcurrGA, "<br/>")
      }
      if (!is.na(fishingMortality$FcurrSA)) {
        text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN SA method: </strong>", fishingMortality$FcurrSA, "<br/>")
      }
    }
    text
  })
  output$yprOutPlot <- renderPlot({
    if ('results' %in% names(yprExec)) {
      YPR <- yprExec$results$F_vs_YPR
      plot(YPR[,2]~YPR[,1],ylab="Yield-Per-Recruit",xlab="Fishing Mortality (F)",type="l")
      abline(h = yprExec$results$Reference_Points[2,2], v = yprExec$results$Reference_Points[2,1], col = "black", lty = 2)
      abline(h = yprExec$results$Reference_Points[1,2], v = yprExec$results$Reference_Points[1,1], col = "red", lty = 2)
      legend(1.7, 0.09, legend=c("F-0.1", "F-Max"),col=c("red", "black"), lty=2:2, cex=0.9)
    }
  })
  output$yprOutTable <- renderTable({
    if ('results' %in% names(yprExec)) {
      yprExec$results$Reference_Points
    }
  }, 
  include.rownames=TRUE, align="c")
  output$yprDifference <- renderText({
    if ('results' %in% names(yprExec)) {
      differenceinYPR = round(yprExec$results$Reference_Points[2,2] - yprExec$results$Reference_Points[1,2], 6)
      text <- paste0("<b>Difference in YPR: </b>",round(differenceinYPR, 4))
      text
    }
  })
  output$downloadYprReport <- renderUI({
    if ("results" %in% names(yprExec)) {
      colnames(yprExec$results$Reference_Points) <- c("F", "Yield Per Recruit")
      downloadButton('createYprReport', 'Download Report')
    }
  })
  output$createYprReport <- downloadHandler(
    filename = paste("Ypr_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "ypr.Rmd")
      file.copy("assets/fishmethods/ypr.Rmd", tempReport, overwrite = TRUE)
      params <- list(yprExec = yprExec)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  
  output$YPRDataConsiderationsText <- renderText({
    text <- "<h5><b>Ensure that spawning stock weight-at-age data is representative of the full population, i.e., are all age groups sampled?</b></h5>"
    text <- paste0(text, "<h5>", "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M in the Optional Parameters section.", "</h5>")
    text
  })
  ######### END YPR METHOD #########
  
  
  ########### SHAEFER METHOD ###########
  output$Biomassplot <- renderPlot({
    sbio<-c(0:input$K)
    B.out<-0
    for (i in 1:length(sbio)){
      B.out[i]<-Schaefer(input$K, input$r, sbio[i])
    }
    # plot stock-growth curve:
    plot(sbio, B.out,xlab=expression('Stock biomass B'[t]),
         ylab="Stock growth",xlim=c(0,max(sbio)),type="l",lwd=3)
    Bmsy=max(B.out)
    arrows(input$K, Bmsy/3, input$K, 2, lwd = 1, lty=1, col="blue")
    text(input$K, Bmsy/2.6, "K", col="blue", cex=1)
    abline(v=(max(sbio)/2), lty=1, col="blue", lwd=1)
    text((input$K/1.9), (max(B.out)/2), "K/2", cex=1, col="blue")
    abline(h=max(B.out), lty=1, col="red", lwd=2)
    text((max(sbio)*0.1), (max(B.out)*0.9), "Maximum production", cex=1, col="red")
  })
  output$Growthplot <- renderPlot({
    hold_stocks<-rep(0, 50)
    hold_stocks[1] = .05
    hold_growth<-rep(0, 50)
    for (i in 1:50){
      hold_growth[i] =input$r * hold_stocks[i] * (1 - hold_stocks[i]/input$K)
      hold_stocks[i+1] = hold_stocks[i] + hold_growth[i]
    }
    
    # plot Schaefer growth:
    plot(1:51, hold_stocks, xlab="Time", ylab=expression('Stock biomass B'[t]),type="l",lwd=2, col="red")
    abline(h=input$K, col="blue", lwd=1, lty=2)
    text(10, (input$K-0.1*input$K), "K: Asymptotic carrying capacity", 
         col = "blue", cex=1)
    text(40, (input$K-0.1*input$K), "Unharvested curve", col = "red", cex=1)
  })
  ######### END SHAEFER METHOD #########
  
  ########### VBGF METHOD ###########
  output$VBGFplot <- renderPlot({
    ages<-c(1:input$amax)
    lengths.out<-GVBGF(input$Linf,input$k,input$t0, ages)
    # plot VBGF
    plot(ages, lengths.out, col = rgb(0/255,86/255,149/255),xlab="Age",ylab="Length",xlim=c(0,input$amax),ylim=c(0,input$Linf*1.1),type="l",lwd=3)
  })
  ######### END VBGF METHOD #########
  
  ########### SEASONAL VBGF METHOD ###########
  output$SVBGFplot <- renderPlot({
    tw<-input$sts+0.5
    ages<-seq(0,input$samax, 0.1)
    tw_line<-seq(tw, input$samax, 1)
    ts_line<-seq(input$sts, input$amax-input$sts, 1)
    solengths.out<-soGVBGF(input$sLinf,input$sk,input$st0, ages, input$sC, input$sts)
    lengths.out<-GVBGF(input$sLinf,input$sk,input$st0, ages)
    # plot VBGF
    plot(ages, lengths.out, col="black",xlab="Age",ylab="Length",xlim=c(0,input$samax),ylim=c(0,input$sLinf*1.1),type="l",lwd=2)
    lines(ages, solengths.out, col="red", type="l",lwd=2)
    abline(v=tw_line, lty=3, lwd=2, col="blue")
    abline(v=ts_line, lty=3, lwd=2, col="green")
    legend("topright", c("Generalized VBGF", "Seasonal VBGF", expression('t'[s]), expression('t'[w])), col=c("black", "red", "green", "blue"),
           lty = c(1, 1, 3, 3), lwd=3)
  })
  ######### END SEASONAL VBGF METHOD #########
  
  
  ########### NATURAL MORTALITY METHOD ###########
  SPEC <- reactive({
    Amax1<-popchar(input$Species, field="tmax")
    Amax<-mean(Amax1$tmax, na.rm=T) 
    Amat1<-maturity(input$Species, field=c("tm", "Lm"))
    Amat<-mean(Amat1$tm, na.rm=T)
    Bl<-mean(Amat1$Lm, na.rm=T)
    PopGwth<-popgrowth(input$Species, field=c("Loo", "TLinfinity", "K", "to", "Temperature"))
    PopGwth$TLinfinity<-ifelse(is.na(PopGwth$TLinfinity), PopGwth$Loo, PopGwth$TLinfinity)
    PopGwth$phi1<-2*log10(PopGwth$TLinfinity)+log10(PopGwth$K)
    Linf<-mean(PopGwth$TLinfinity, na.rm=T)
    logTLinf<-2*log10(Linf)
    avephi1<-mean(PopGwth$phi1, na.rm=T)
    k<-10^(avephi1-logTLinf)
    t0<-mean(PopGwth$to, na.rm=T)
    Temp<-mean(PopGwth$Temp, na.rm=T)
    SPEC<-cbind.data.frame(Amax,Amat,Bl,Linf,k,t0,Temp)
    names(SPEC)<-c("Amax", "Amat", "Bl", "Linf", "k", "t0", "Temp")
    SPEC
  })
  M_vals_all<- reactive({
    SPEC<-SPEC()
    Amax<-SPEC$Amax
    Amat<-SPEC$Amat
    Bl<-SPEC$Bl
    Linf<-SPEC$Linf
    k<-SPEC$k
    t0<-SPEC$t0
    Temp<-SPEC$Temp
    # Show the first "n" observations
    output$Ftable <- renderTable({
      input$nm_sub_fb
      F_table<-data.frame(cbind(Amax, Amat, Bl, Linf, k, t0, Temp))
      colnames(F_table)<-c("Max Age","Age Mat.", "Mean Length", "VB Linf", "VB k", "VB t0", "Temp.")
      F_table
    })
    Pauly80lt_M<-Pauly80wt_M<-AnC75_M<-Roff_M<-GnD_GSI_M<-PnW_M<-Lorenzen96_M<-Gislason_M<-NA
    if(!anyNA(c(input$Amax))){Then_M_Amax<-Then_M(input$Amax)}
    else{
      Then_M_Amax<-Then_M(Amax)
    }
    if(!(anyNA(c(input$k,input$Amax)))) {
      AnC75_M<-M.empirical(Kl=input$k,tmax=input$Amax,method=4)[1]
      Then_M_VBGF<-Then_VBGF(input$Linf*10,input$k)
      Jensen_M_VBGF<-Jensen_M_k(input$k)
    }
    else {
      if(!(anyNA(c(k,Amax)))){AnC75_M<-M.empirical(Kl=k,tmax=Amax,method=4)[1]
      Then_M_VBGF<-Then_VBGF(Linf*10,k)
      Jensen_M_VBGF<-Jensen_M_k(k)}
    }
    if(!(anyNA(c(input$Linf,input$k,input$Bl)))) {
      Gislason_M<-M.empirical(Linf=input$Linf,Kl=input$k,Bl=input$Bl,method=9)[1]
      CnW_M_VBGF<-Chen_N_Wat_M(input$Amax,input$Amat,input$k,input$t0)
      CnW_M_a_VBGF<-Chen_N_Wat_M(input$Amax,input$Amat,input$k,input$t0,out.type = 0)
      maxage<-input$Amax
    } else {
      if(!(anyNA(c(Linf,k,Bl)))){Gislason_M<-M.empirical(Linf=Linf,Kl=k,Bl=Bl,method=9)[1]
      CnW_M_VBGF<-Chen_N_Wat_M(Amax,Amat,k,t0)
      CnW_M_a_VBGF<-Chen_N_Wat_M(Amax,Amat,k,t0,out.type = 0)
      maxage<-Amax}
    }
    
    if(!is.na(maxage)){
      CnW_M_a_VBGF_table<-cbind(c(1:maxage),CnW_M_a_VBGF)
      colnames(CnW_M_a_VBGF_table)<-c("Age","M")
    }
    
    if(!(anyNA(c(input$k,input$Amat)))) {
      Roff_M<-M.empirical(Kl=input$k,tm=input$Amat,method=5)[1]
      Jensen_M_Amat<-Jensen_M_amat(input$Amat)
      Rikhter_Efanov_Amat<-Rikhter_Efanov_Amat_M(input$Amat)
    } else {
      if(!(anyNA(c(k,Amat)))) {
        Roff_M<-M.empirical(Kl=k,tm=Amat,method=5)[1]
        Jensen_M_Amat<-Jensen_M_amat(Amat)
        Rikhter_Efanov_Amat<-Rikhter_Efanov_Amat_M(Amat)
      }
    }
    if(!(anyNA(c(input$Linf,input$k,input$Temp)))) {
      Pauly80lt_M<-M.empirical(Linf=input$Linf,Kl=input$k,T=input$Temp,method=1)[1]
    } else {
      if(!(anyNA(c(Linf,k,Temp)))){Pauly80lt_M<-M.empirical(Linf=Linf,Kl=k,T=Temp,method=1)[1]}
    }
    User_M<-input$User_M
    M_vals_all<-c(Then_M_Amax,AnC75_M,Then_M_VBGF,Jensen_M_VBGF,Pauly80lt_M,Gislason_M,CnW_M_VBGF,Roff_M,Jensen_M_Amat,Rikhter_Efanov_Amat,User_M) #Pauly80wt_M,PnW_M,Lorenzen96_M,GnD_GSI_M,
    output$downloadCW_M_a <- downloadHandler(
      filename = function() {paste0("CW_M_a_values", '.csv') },
      content = function(file) {write.csv(CnW_M_a_VBGF_table, file=file)}
    )  
    M_vals_all
  })
  output$Mplot <- renderPlot({
    input$nm_sub_fb
    M_vals_all<-M_vals_all()
    M_methods<-c("Then_Amax 1","Then_Amax 2","Then_Amax 3","Hamel_Amax","AnC","Then_VBGF","Jensen_VBGF 1","Jensen_VBGF 2","Pauly_lt","Gislason","Chen-Wat","Roff","Jensen_Amat","Ri_Ef_Amat","User input") #"Pauly_wt","PnW","Lorenzen","GSI",
    # plot M
    if(all(is.na(M_vals_all))){ymax<-0.5}
    if(!(all(is.na(M_vals_all)))){ymax<-ceiling((max(M_vals_all,na.rm=TRUE)*1.1*10))/10}
    par(mar=c(8,4,3,6),xpd =TRUE)
    plot(M_vals_all, col = "black",bg=c("blue","blue","blue","blue","green","green","green","green","yellow","yellow","orange","red","red","red","brown"),xlab=" ",ylab="Natural mortality",ylim=c(0,ymax),pch=22,cex=1.5,axes=F) #"black","black","black","purple",
    box()
    axis(1,at=1:length(M_vals_all),labels=M_methods,las=3)
    axis(2)
    legend(x="topright",legend=c("Amax","VBGF","VBGF:Temp","VBGF;Amat","Amat","User input"),pch=22,col="black",pt.bg=c("blue","green","yellow","orange","red","brown"),bty="n",horiz=FALSE,cex=1,inset=c(-0.08,0)) #"Weight","GSI",  #"black","purple",
    M_table<-data.frame(cbind(M_methods,M_vals_all))
    colnames(M_table)<-c("Method","M")
    # if(all(is.na(M_vals()))){return(NULL)}
    output$downloadMs <- downloadHandler(
      filename = function() {paste0("M_values", '.csv') },
      content = function(file) {write.csv(M_table, file=file)}
    )
  })
  # Show the first "n" observations
  output$Mtable <- renderTable({
    input$nm_sub_fb
    SPEC<-SPEC()
    Amax<-SPEC$Amax
    Amat<-SPEC$Amat
    Bl<-SPEC$Bl
    Linf<-SPEC$Linf
    k<-SPEC$k
    t0<-SPEC$t0
    Temp<-SPEC$Temp
    Pauly80lt_M<-Pauly80wt_M<-AnC75_M<-Roff_M<-Gislason_M<-NA #<-Pauly80wt_M<-PnW_M<-Lorenzen96_M<-GnD_GSI_M
    Then_M_Amax<-Then_M(Amax)
    if(!(anyNA(c(k,Amax)))){AnC75_M<-M.empirical(Kl=k,tmax=Amax,method=4)[1]}
    Then_M_VBGF<-Then_VBGF(Linf*10,k)
    Jensen_M_VBGF<-Jensen_M_k(k) 
    if(!(anyNA(c(Linf,k,Bl)))){Gislason_M<-M.empirical(Linf=Linf,Kl=k,Bl=Bl,method=9)[1]}
    CnW_M_VBGF<-Chen_N_Wat_M(Amax,Amat,k,t0)
    if(!(anyNA(c(k,Amat)))){Roff_M<-M.empirical(Kl=k,tm=Amat,method=5)[1]}
    Jensen_M_Amat<-Jensen_M_amat(Amat)
    Rikhter_Efanov_Amat<-Rikhter_Efanov_Amat_M(Amat)
    if(!(anyNA(c(Linf,k,Temp)))){Pauly80lt_M<-M.empirical(Linf=Linf,Kl=k,T=Temp,method=1)[1]}
    M_vals_all<-c(Then_M_Amax,AnC75_M,Then_M_VBGF,Jensen_M_VBGF)
    M_methods<-c("Then_Amax 1","Then_Amax 2","Then_Amax 3","Hamel_Amax","AnC","Then_VBGF","Jensen_VBGF 1","Jensen_VBGF 2")
    M_table<-data.frame(cbind(M_methods,signif(M_vals_all,3)))
    colnames(M_table)<-c("Methods","M")
    M_table
  })
  # Show the first "n" observations
  output$Mtable2 <- renderTable({
    input$nm_sub_fb
    SPEC<-SPEC()
    Amax<-SPEC$Amax
    Amat<-SPEC$Amat
    Bl<-SPEC$Bl
    Linf<-SPEC$Linf
    k<-SPEC$k
    t0<-SPEC$t0
    Temp<-SPEC$Temp
    Pauly80lt_M<-AnC75_M<-Roff_M<-Gislason_M<-NA #<-Pauly80wt_M<-PnW_M<-Lorenzen96_M<-GnD_GSI_M
    Then_M_Amax<-Then_M(Amax)
    if(!(anyNA(c(k,Amax)))){AnC75_M<-M.empirical(Kl=k,tmax=Amax,method=4)[1]}
    Then_M_VBGF<-Then_VBGF(Linf*10,k)
    Jensen_M_VBGF<-Jensen_M_k(k) 
    if(!(anyNA(c(Linf,k,Bl)))){Gislason_M<-M.empirical(Linf=Linf,Kl=k,Bl=Bl,method=9)[1]}
    CnW_M_VBGF<-Chen_N_Wat_M(Amax,Amat,k,t0)
    if(!(anyNA(c(k,Amat)))){Roff_M<-M.empirical(Kl=k,tm=Amat,method=5)[1]}
    Jensen_M_Amat<-Jensen_M_amat(Amat)
    Rikhter_Efanov_Amat<-Rikhter_Efanov_Amat_M(Amat)
    if(!(anyNA(c(Linf,k,Temp)))){Pauly80lt_M<-M.empirical(Linf=Linf,Kl=k,T=Temp,method=1)[1]}
    User_M<-input$User_M
    M_vals_all<-c(Pauly80lt_M,Gislason_M,CnW_M_VBGF,Roff_M,Jensen_M_Amat,Rikhter_Efanov_Amat,User_M) #Pauly80wt_M,PnW_M,Lorenzen96_M,GnD_GSI_M,
    M_methods<-c("Pauly_lt","Gislason","Chen-Wat","Roff","Jensen_Amat","Ri_Ef_Amat","User input") #"Pauly_wt","PnW","Lorenzen","GSI",
    M_table<-data.frame(M_vals_all)
    M_table<-data.frame(cbind(M_methods,signif(M_vals_all,3)))
    colnames(M_table)<-c("Methods","M")
    M_table
  })
  #Plot Composite M
  output$Mcomposite<- renderPlot({    
    input$nm_sub_fb
    if(all(is.na(M_vals_all()))){return(NULL)}
    else{
      M.wts<-c(input$Then_Amax_1,input$Then_Amax_2,input$Then_Amax_3,input$Hamel_Amax,input$AnC,input$Then_VBGF,input$Jensen_VBGF_1,input$Jensen_VBGF_2,input$Pauly_lt,input$Gislason,input$Chen_Wat,input$Roff,input$Jensen_Amat,input$Ri_Ef_Amat,input$UserM) #input$Pauly_wt,input$PnW,input$Lorenzen,input$Gonosoma,
      #remove NAs
      if(any(is.na(M_vals_all()))){
        NA.ind<-attributes(na.omit(M_vals_all()))$na.action
        M.sub<-M_vals_all()[-NA.ind]
        M.wts.sub<-M.wts[-NA.ind]
      }
      else{
        M.sub<-M_vals_all()
        M.wts.sub<-M.wts
      }
      #remove 0 weight
      M.sub.n0<-M.sub[M.wts.sub>0]
      M.wts.sub.n0<-M.wts.sub[M.wts.sub>0]
      M.wts.sub.stand<-M.wts.sub.n0/sum(M.wts.sub.n0)
      M.densum<-density(M.sub.n0,weights=M.wts.sub.stand,from=0,cut=0)
      #Approximate the denisty function
      f<- approxfun(M.densum$x, M.densum$y, yleft=0, yright=0)
      #Standardize densities
      pdf_counts<-round(1000000*(M.densum$y/sum(M.densum$y)))
      #Expand densities to samples
      pdf.samples<-unlist(mapply(rep,M.densum$x,pdf_counts))
      #Calculate the cdf
      cdf.out<-ecdf(pdf.samples)
      #Plot the density function
      M.densum.plot<- data.frame(x = M.densum$x, y = M.densum$y)
      Mcomposite.densityplot<- ggplot(data=M.densum.plot,aes(x,y,fill="blue"))+
        geom_line(col="black")+
        labs(x="Natural Mortality",y="Density")+ 
        geom_area(fill="gray")+ 
        geom_vline(xintercept = quantile(cdf.out,0.5),color="darkblue",size=1.2)
      print(Mcomposite.densityplot)
      output$downloadMcompositedensityplot <- downloadHandler(
        filename = function() { paste0('Mcomposite_densityplot',Sys.time(), '.png')},
        content = function(file) {
          png(file, type='cairo',width=800,height=720)
          print(Mcomposite.densityplot)
          dev.off()},contentType = 'image/png') 
      output$downloadMcompositedist <- downloadHandler(
        filename = function() {  paste0("Mcomposite_samples",Sys.time(),".DMP") },
        content = function(file) {save(pdf.samples,file=file)}) 
    }
  })
  ########### END NATURAL MORTALITY METHOD ###########
  
  
  
  
  ########################### ALL LABELS ###########################
  output$homeInfo <- renderText({
    text <- "<h3>Stock Monitoring Tools for limited data</h3>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "This is a development version")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "On the left hand side you can choose between <b>CMSY</b>, <b>ELEFAN</b> and <b>YPR</b>/<b>SBPR</b> methods.")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<p>")
    text <- paste0(text, "The <b>CMSY</b> method is provided by the <a href='http://www.bluebridge-vres.eu/' target='blank_'>BlueBridge Infrastructure</a>")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<p>")
    text <- paste0(text, "For <b>ELEFAN</b> you can choose between ELEFAN_GA, ELEFAN_SA and ELEFAN")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<p>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>ELEFAN GA:</b>&nbsp;Electronic LEngth Frequency ANalysis with genetic algorithm used for estimating growth parameters.</li>")
    text <- paste0(text, "<li><b>ELEFAN SA:</b>&nbsp;Electronic LEngth Frequency ANalysis with simulated annealing for estimating growth parameters.</li>")
    text <- paste0(text, "<li><b>ELEFAN:</b>&nbsp;Electronic LEngth Frequency ANalysis for estimating growth parameter.</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>YPR</b> and <b>SBPR</b> are provided by the <a href='https://cran.r-project.org/web/packages/fishmethods/index.html' target='_blank'>fishmethods</a> R library")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<p>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>YPR:</b>&nbsp;Yield-per-recruit.</li>")
    text <- paste0(text, "<li><b>SBPR:</b>&nbsp;Spawning stock biomass-per-recruit.</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    
    text
  })
  output$cmsyIntroOut <- renderText({
    text <- "<h3><b>CMSY - Catch-Maximum Sustainable Yield</b></h3>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "The <b>CMSY</b> method for data-limited stock assessment. Described in Froese, R., Demirel, N., Coro, G., Kleisner, K. M., Winker, H. (2016). Estimating fisheries reference points from catch and resilience. Fish and Fisheries.")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<a href='http://onlinelibrary.wiley.com/doi/10.1111/faf.12190/full' target='_blank'>Click here to read the paper.</a>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "The Schaefer production model parameters are r and k. Different combinations of these parameters will produce different time series of biomass. In CMSY, the Schaefer model is run many times to calculate annual biomasses for r-k pairs randomly drawn from the prior distributions. The model determines which r-k pairs are valid: e.g., those pairs that result in a biomass time series that does not (1) result in a stock collapse or (2) allow the stock to exceeded carrying capacity. Also, those r-k pairs that result in a final relative biomass estimate between the values specified in the inputs (the final depletion range), are accepted and used to calculate MSY (rk/4) and biomass over time.")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The geometric means of the resulting density distributions of r, k and MSY are taken as the most probable values.")
    #text <- paste0(text, "<br/>")
    #text <- paste0(text, "<p>")
    #text <- paste0(text, "<span><b>CMSY Vectorized</b> is a new adaptation of CMSY designed to increase fitting speed to enable implementation in management strategy evaluation. <br/>This is achieved by adding adaptive parameter search bounds to restrict the inspected r-K space and automatically increase depletion priors if necessary.</span>")
    #text <- paste0(text, "</p>")
    #text <- paste0(text, "<br/>")
    #text <- paste0(text, "<p>")
    #text <- paste0(text, "<span>You can download the <b>CMSY Vectorized</b> source code for <b>R</b> by clicking <a href='https://goo.gl/fjfsCL' target='_blank'>here</a></span>")
    #text <- paste0(text, "</p>")
    text
  })
  output$elefanIntroOut <- renderText({
    text <- "<h3><b>ELEFAN methods by TropFishR</b></h3>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>Elefan</b> is a computational method designed to estimate life history parameters using a time series of length frequency observations.")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "These methods are provided by the <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR library</a>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "The study of fish growth involves a determination of body size as a function of age. Most stock assessment methods work essentially with age composition data. In temperate waters it is easier to acquire data to estimate age by counting of year rings on hard parts such as scales and otoliths (ear bones). These rings are formed due to strong fluctuations in environmental conditions from summer to winter and vice versa. In tropical areas such drastic changes do not occur and it is therefore very difficult, if not impossible to use this kind of seasonal rings for age determination.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "ELEFAN is one of a number of numerical methods that have been developed, which allow the conversion of length-frequency data into age composition. Although these methods do not require the reading of rings on hard parts, the final interpretation of the results becomes much more reliable if at least some direct age readings are available.")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The TropFishR has three different Elefan methods: <b>ELEFAN</b>, <b>ELEFAN GA</b> and <b>ELEFAN SA</b>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<h4>ELEFAN</h4>")
    text <- paste0(text, "<b>E</b>lectronic <b>LE</b>ngth <b>F</b>requency <b>AN</b>alysis for estimating growth parameters.<br/>This function performs the K-Scan and Response surface analyses to estimate growth parameters. It combines the step of restructuring length-frequency data (lfqRestructure) followed by the fitting of seasonal von Bertalanffy Growth Function (VBGF) curves (see Supporting tools: Seasonal VBGF) through the restructured data (lfqFitCurves). K-Scan is a method used to search for the K parameter with the best fit while keeping ", withMathJax("\\(L_\\infty\\)"), "fixed. In contrast, with response surface analysis both parameters are estimated and the fits are displayed in a heatmap. Both methods use an optimisation to find the best time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month (t_anchor) value for each combination of K and ", withMathJax("\\(L_\\infty\\)"),". To find out more about t_anchor, please refer to the Details description of lfqFitCurves. The score value Rn_max is not comparable with the score values of the other ELEFAN functions (ELEFAN_SA or ELEFAN_GA).")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<h4>ELEFAN GA (generic algorithm)</h4>")
    text <- paste0(text, "<b>E</b>lectronic <b>LE</b>ngth <b>F</b>requency <b>AN</b>alysis with simulated annealing for estimating growth parameters.<br/>A more detailed description of the generic algorithm (GA) can be found in Scrucca (2013). The score value fitnessValue is not comparable with the score value of the other ELEFAN functions (ELEFAN or ELEFAN_SA).")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<h4>ELEFAN SA (simulated annealing)</h4>")
    text <- paste0(text, "<b>E</b>lectronic <b>LE</b>ngth <b>F</b>requency <b>AN</b>alysis with simulated annealing for estimating growth parameters.<br/>A more detailed description of the simulated annealing (SA) can be found in Xiang et al. (2013). The score value cost_value is not comparable with the score value of the other ELEFAN functions (ELEFAN or ELEFAN_GA).")
    text <- paste0(text, "</p>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<h4>Method of operation:</h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<li>ELEFAN fits a seasonal version (see Supporting Tools) of the von Bertalanffy Growth Function (VBGF).</li>")
    text <- paste0(text, "<li>The first step is the restructuring of the length-frequency data using a procedure that scores the length bins based on their deviation from a moving average across neighboring bins.</li>")
    text <- paste0(text, "<li>The second step is the calculation of the cumulative score for a given set of VBGF parameters based on the bin scores that are intersected by resulting growth curves.</li>")
    text <- paste0(text, "<li>Finally, a search for VBGF parameters results in the maximum score value.</li>")
    text <- paste0(text, "<li>The ELEFAN method uses a Thompsen and Bell model to determine the F reference points and current F.</li>")
    text <- paste0(text, "<br/>")
    link <- "<a href='https://goo.gl/tsqt64' target='_blank'>Click Here</a>"
    text <- paste0(text, "<h4>Information on the dataset used</h4>")
    text <- paste0(text, "<p><h5>", link,"&nbsp; to download a sample dataset that can be used with <b>Elefan</b> methods", "</h5></p>")
    text <- paste0(text, "<span class=\"elefan_info\">The dataset used by this example is the <b>synLFQ7</b></span><br><br>")
    text <- paste0(text, "<span class=\"elefan_info\">Synthetic length-frequency data as generated by the function lfqGen from the fishdynr package (Taylor 2016). Can be used by <b>ELEFAN</b>, <b>ELEFAN_SA</b>, or <b>ELEFAN_GA</b>. <br>The data are generated with the following VBGF parameters:</span>")
    text <- paste0(text, "<ul style=\"margin-top: 10px;\">")
    text <- paste0(text, "<li>Curving coefficient: K = 0.2 +/- 0.1 (CV)</li>")
    text <- paste0(text, "<li>Length infinity: ", withMathJax("\\(L_\\infty\\)")," = 123 +/- 0.05 (CV)</li>")
    text <- paste0(text, "<li>Amplitude of growth oscillation: C = 0.3</li>")
    text <- paste0(text, "<li>Summer point of oscillation: ", withMathJax("\\(t_s\\)")," = 0</li>")
    text <- paste0(text, "<li>Time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month: t_anchor between 0.16 and 0.34 (Time when yearly recruitment pulse occurs; e.g. 0 = Jan 1, 0.25 = Apr 1, 0.5 = Jul 1, 0.75 = Oct 1; repro_wt = c(0, 0, 0.2, 1, 0.6, 0, 0, 0, 0, 0, 0, 0))</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<h4>Glossary</h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>lfqRestructure:</b> First step of the Electronic LEngth Frequency ANalysis (ELEFAN), which is restructuring length frequency data (lfq). This is done according to a certain protocol, described by many authors (see Details or References for more information).")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>lfqFitCurves:</b> This function estimates seasonal von Bertalanffy growth function (VBGF) curves for a set of growth parameters.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text
  })
  output$fishMethodsIntroOut <- renderText({
    text <- "<h3><b>FishMethods</b></h3>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>Fishmethods: </b>Fishery science methods and models from published literature and contributions from colleagues.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "In data-limited situations where long-term, comprehensive catch data does not exist, per-recruit models can be used to determine estimates of optimal fishing mortality. Yield-per-recruit (YPR) and spawning biomass-per-recruit (SBPR) models calculate the equilibrium yield per recruit and spawning stock biomass per recruit, respectively, for a given value of fishing mortality (F) and a given length or age at first capture. Since F and Tc or Lc are values that a fishery manager can control (in theory), the idea is that by focusing on YPR or SBPR, managers can maintain a stock's population by preserving its reproductive capability. These models help to determine the optimum yield to prevent overfishing by instituting management controls on effort and length or age at first capture.")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<h4>Methods used</h4>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>SBPR</b> Spawning stock biomass-per-recruit(SBPR) analysis is conducted following Gabriel et al. (1989). Reference points of fishing mortality (F) and SBPR for a percentage of maximum spawning potential are calculated.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>YPR</b> Yield-per-recruit (YPR) analysis is conducted following the modified Thompson-Bell algorithm. Reference points",  withMathJax("\\(F_{max}\\)"), "and",  withMathJax("\\(F_{0.1}\\)") ,"are calculated.")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>",withMathJax("\\(F_{0.1}\\)"),"</b> Fishing mortality rate corresponding to 10% of the slope of the YPR curve as a function of F when F=0. This is the F at which the marginal increase in equilibrium yield has dropped to 1/10 of its value when the stock was first exploited.")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>", withMathJax("\\(F_{max}\\)"), "</b>Fishing mortality rate that produces the maximum yield per recruit.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>SBPR Options:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Input file: </b> Input data file</li>")
    text <- paste0(text, "<li><b>M: </b> Single natural mortality (M) rate if M is assumed constant over all ages</li>")
    text <- paste0(text, "<li><b>pF: </b> Proportion of fishing mortality (F) that occurs before spawning</li>")
    text <- paste0(text, "<li><b>pM: </b> Proportion of natural mortality (M) that occurs before spawning</li>")
    text <- paste0(text, "<li><b>MSP: </b> Percentage of maximum spawning potential (%MSP) for which fishing mortality (F) and SBPR should be calculated</li>")
    text <- paste0(text, "<li><b>maxF: </b> Maximum value of fishing mortality (F) range over which SBPR will be calculated</li>")
    text <- paste0(text, "<li><b>incrF: </b> Fishing mortality (F) increment for SBPR calculation</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>YPR Options:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Input file: </b> Input data file</li>")
    text <- paste0(text, "<li><b>M: </b> Single natural mortality (M) rate if M is assumed constant over all ages</li>")
    text <- paste0(text, "<li><b>maxF: </b> Maximum value of fishing mortality (F) range over which YPR will be calculated. YPR is calculated for F = 0 to maxF</li>")
    text <- paste0(text, "<li><b>plus: </b> logical value indicating whether the last age is a plus-group</li>")
    text <- paste0(text, "<li><b>oldest: </b> if plus is checked, a numeric value indicating the oldest age in the plus group</li>")
    text <- paste0(text, "<li><b>incrF: </b> Fishing mortality (F) increment for YPR calculation</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    text
  })
  
  output$cmsySampleDataset <- renderText({
    link <- "<a href='https://goo.gl/GPrVRD' target='_blank'>Click Here</a>"
    text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>CMSY</b> methods", "</h4></p>")
    text
  })
  output$elefanSampleDataset <- renderText({
    link <- "<a href='https://goo.gl/tsqt64' target='_blank'>Click Here</a>"
    text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>Elefan</b> methods", "</h4></p>")
    text
  })
  output$fishMethodsSampleDataset <- renderText({
    link <- "<a href='https://goo.gl/24FuzG' target='_blank'>Click Here</a>"
    text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>FishMethods</b>", "</h4></p>")
    text
  })
  
  output$vonBertalannfyInfoText <- renderText({
    text <- "<b>The VBGF expresses the length, L, as a function of the age of the fish, t. K is a parameter that controls the curvature</b>"
    text <- paste0(text, "<h5>", withMathJax("\\(L_\\infty\\)"), "is interpreted as 'the mean length of very old (strictly: infinitely old) fish'. It is also called the 'asymptotic length'.", "</h5>")
    text <- paste0(text, "<h5>", "K is a 'curvature parameter' which determines how fast the fish approaches its ", withMathJax("\\(L_\\infty.\\)"), "</h5>")
    text <- paste0(text, "<h5>", "Some species, most of them short-lived, almost reach their ", withMathJax("\\(L_\\infty\\)"),  "in a year or two and have a high value of K.")
    text <- paste0(text, "Other species have a flat growth curve with a low K-value and need many years to reach anything like their ", withMathJax("\\(L_\\infty.\\)"),  "</h5>")
    text <- paste0(text, "<h5>", "Increasing K with the slider bar will result in a growth curve that has more 'bend'.", "</h5>")
    text <- paste0(text, "<h5>", "The third parameter, ", withMathJax("\\(t_0,\\)"),  "sometimes called 'the initial condition parameter', determines the point in time when the fish has zero length.", "</h5>")
    text <- paste0(text, "<h5>",  "Biologically, this has no meaning, because the growth begins at hatching when the larva already has a certain length, which may be called L(0) when we put t = 0 at the day of birth.", "</h5>")
    text <- paste0(text, "<h5>",  "It is easily identified by inserting t = 0 into the equation.", "</h5>")
    text <- paste0(text, "<h5>",  "Growth parameters differ from species to species, but they may also vary from stock to stock within the same species, i.e. growth parameters of a particular species may take different values in different parts of its range. Also successive cohorts may grow differently depending on environmental conditions.", "</h5>")
    text <- paste0(text, "<h5>", "Further growth parameters often take different values for the two sexes. If there are pronounced differences between the sexes in their growth parameters, the input data should be separated by sex and values of K, ", withMathJax("\\(L_\\infty,\\)"), " and ", withMathJax("\\(t_0\\)"), "should be estimated for each sex separately.", "</h5>")
  })
  output$seasonalVonBertalannfyInfoText <- renderText({
    text <- "<h4>Like the generalized VBGF, the seasonal VBGF expresses the length, L, as a function of the age of the fish, t. K is a parameter that controls the curvature.</h4>"
    text <- paste0(text, "<h4>", "The addition of the term:", withMathJax("\\(\\frac{Ck}{2\\pi}sin2\\pi(t-t_s)\\)"), "produces seasonal oscillations of the growth rate, by changing ", withMathJax("\\(t_0,\\)"), "during the year. The parameter ", withMathJax("\\(t_s,\\)"))
    text <- paste0(text, "is called the 'summer point', and takes values between 0 and 1. At the time of the year when ")
    text <- paste0(text, "the fraction ", withMathJax("\\(t_s,\\)"), "has elapsed, the growth rate is the highest. At time")
    text <- paste0(text, " ", withMathJax("\\(t_w = t_s+0.5,\\)"), "which is the 'winter point', the growth rate is the lowest.")
    text <- paste0(text, "</h4>")
    text <- paste0(text, "<h4>")
    text <- paste0(text, "If C = 0, then the equation reduces to the generalized VBGF. In other words, C = 0 implies that there is no ")
    text <- paste0(text, "seasonality in the growth rate. The higher the value of C the more pronounced are the seasonal oscillations. ")
    text <- paste0(text, "If C = 1, the growth rate becomes zero at the winter point.")
    text <- paste0(text, "</h4>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "As with the generalized VBGF:")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, withMathJax("\\(L_\\infty\\)"), "is is interpreted as 'the mean length of very old (strictly: infinitely old) fish'. it is also called the 'asymptotic length'.")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "K is a 'curvature parameter' which determines how fast the fish approaches its ", withMathJax("\\(L_\\infty.\\)"))
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "Some species, most of them short-lived, almost reach their ", withMathJax("\\(L_\\infty\\)"), "in a year or two and have a high value of K. Other species have a flat growth curve with a low K-value and need many years to reach anything like their ", withMathJax("\\(L_\\infty.\\)"))
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "Increasing K with the slider bar will result in a growth curve that has more 'bend'.")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "The third parameter, ", withMathJax("\\(t_0,\\)"), "sometimes called 'the initial condition parameter', determines the point in time when the fish has zero length.")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "Biologically, this has no meaning, because the growth begins at hatching when the larva already has a certain length, which may be called L(0) when we put t = 0 at the day of birth.")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "It is easily identified by inserting t = 0 into the equation.")
    text <- paste0(text, "</h5>")
    text
  })
  output$naturalMortalityInfoText <- renderText({
    text <- "<h5>This tool employs various empirical estimators of natural mortality.</h5>"
    text <- paste0(text, "<h5>", "When the user enters the scientific name for a fish species, FishBase will be queried for:", "</h5>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>", "(1) Maximum age (Amax)", "</li>")
    text <- paste0(text, "<li>", "(2) Age at maturity (Amat)", "</li>")
    text <- paste0(text, "<li>", "(3) L-infinity (in cm) (Linf)", "</li>")
    text <- paste0(text, "<li>", "(4) Von Bertalanffy Growth Function (VBGF) growth coefficient (k)", "</li>")
    text <- paste0(text, "<li>", "(5) VBGF age at size 0 (t0)", "</li>")
    text <- paste0(text, "<li>", "(6) Body length in cm (Bl)", "</li>")
    text <- paste0(text, "<li>", "(7) Mean water temperature in Celsius (Temp)", "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<h5>", "Averaging of Von Bertalanffy (VBFG) parameters is done following the specifications of Pauly, D. (1991). Growth performance in fishes: rigorous description of patterns as a basis for understanding causal mechanisms. ICLARM Contribution No. 793.", "</h5>")
    text <- paste0(text, "<h5>", "Estimates will be displayed in the main panel.",  "</h5>")
    text <- paste0(text, "<h5>", "Four methods use Amax, three methods use the VBGF parameters, two methods use the VBGF parameters & Temp, one method uses the VBGF parameters & Amat, and three methods use only Amat. These groupings are indicated by the different colors in the top plot.", "</h5>")
    text <- paste0(text, "<h5>", "<em>The user can also choose to input their own parameters if they have inputs from local studies. These input values will override the FishBase calculations if all the necessary parameters for a particular method are available (e.g., all the VBGF parameters are available for those methods that require them).</em>", "</h5>")
    text <- paste0(text, "<h5>","The individual estimates of M are combined with defined weightings below (that the user can modify) and a single average M is provided. This average M can be used as input in the YPR/SBPR and ELEFAN applications.",  "</h5>")
    text <- paste0(text, "<h5>", "References for each method can be found", " <a href=\"javascript:window.open('References_M.html', '_blank','width=600,height=400')\">here</a>", "</h5>")
    text <- paste0(text, "<h5>",  "</h5>")
    text
  })
  
  
  output$elefanGaTitle <- renderText({
    text <- "<span><h3><b>Elefan GA (Generic Algorithm)</b></h3></span>"
    text
  })
  output$elefanSaTitle <- renderText({
    text <- "<span><h3><b>Elefan SA (Simulated Annealing)</b></h3></span>"
    text
  })
  output$elefanTitle <- renderText({
    text <- "<span><h3><b>Elefan</b></h3></span>"
    text
  })
  output$SBPRTitle <- renderText({
    text <- "<span><h3><b>Spawning stock biomass-per-recruit(SBPR)</b></h3></span>"
    text
  })
  output$sbprTitle <- renderText({
    text <- "<span><h3><b>Spawning stock biomass-per-recruit (SBPR)</b></h3></span>"
    text
  })
  output$yprTitle <- renderText({
    text <- "<span><h3><b>Yield-per-recruit (YPR)</b></h3></span>"
    text
  })
  output$basicShaeferTitle <- renderText({
    text <- "<span><h3><b>Run surplus production model</b></h3></span>"
    text
  })
  output$basicVonBertalannfyTitle <- renderText({
    text <- "<span><h3><b>Generalized Von Bertalanffy Growth Function (VBGF)</b></h3></span>"
    text
  })
  output$SeasonalVonBertalannfyTitle <- renderText({
    text <- "<span><h3><b>Seasonal Von Bertalanffy Growth Function (soVBGF)</b></h3></span>"
    text
  })
  output$naturalMortalityTitle <- renderText({
    text <- "<div style='width: 100%;position: relative;height: 100px; margin-bottom:3px;'>"
    text <- paste0(text, "<div style='float: left; width: 70%;'><span><h3><b>Estimating Natural Mortality (M) from FishBase life history parameters</b></h3><br>This application is a modified version of the Barefoot Ecologist tool developed by Jason Cope: <a target='_blank' href='http://barefootecologist.com.au/shiny_m.html'>http://barefootecologist.com.au/shiny_m.html</a></span></div>")
    text <- paste0(text, "</div>")
    text
  })
  
  output$rnMax_ga <- renderText({
    if ("results" %in% names(elefan_ga)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", round(elefan_ga$results$data$Rn_max, 3))
      title
    } else {  "" }
  })
  output$rnMax_sa <- renderText({
    if ("results" %in% names(elefan_sa)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", round(elefan_sa$results$data$Rn_max, 3))
      title
    } else {  "" }
  })
  output$rnMax <- renderText({
    if ("results" %in% names(elefan)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", round(elefan$results$data$Rn_max, 3))
      title
    } else {  "" }
  })
  
  output$titlePlot1_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Restructured LFQ data</p>"
      txt
    }
  })
  output$titlePlot3_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
      txt
    }
  })
  output$titlePlot4_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
      txt
    }
  })
  output$title_tbl1_e <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Biological reference levels:</p>"
      txt
    }
  })
  output$title_tbl2_e <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Current levels:</p>"
      txt
    }
  })
  output$titleResultsOfTheComputation_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<h2>Results of the ELEFAN computation</h2>"
      txt
    }
  })
  
  output$shaefer_ex1 <- renderUI({
    withMathJax(helpText('Classic Schaefer (logistic) form  $$B_t = rB_t\\left(1 +
               \\frac{r}{K}\\right)\\!$$'))
  })
  output$shaefer_ex2 <- renderUI({
    withMathJax(helpText('Biomass giving maximum sustainable yield:  $$B_{MSY} = \\frac{K}{2}\\!$$'))
  })
  output$shaefer_ex3 <- renderUI({
    withMathJax(helpText('Maximum sustainable yield:  $$MSY = \\frac{rK}{4}\\!$$'))
  })
  output$shaefer_ex4 <- renderUI({
    withMathJax(helpText('Fishing mortality rate at MSY:  $$F_{MSY} = \\frac{r}{2}\\!$$'))
  })
  output$basicShaeferInfoText <- renderText({
    text <- "<p><h5>The surplus production model is an example of a 'holistic model', wherein the stock is considered as one unit of biomass and no attempt is made to model on an age- or length-base. These models deal with the entire stock, the entire fishing effort and the total yield obtained from the stock, without incorporating details such as growth and mortality-at-age or the effect of the gear on the age of fish capture.</h5></p>"
    text <- paste0(text, "<p><h5>", "The objective of the application of 'surplus production models' is to determine the optimum level of effort, that is the effort that produces the maximum yield that can be sustained without affecting the long-term productivity of the stock, the so-called maximum sustainable yield (MSY).", "</h5></p>")
  })
  
  output$cmsyLegacyWarning <- renderText({
    text <- "<span style='margin-left: 20px;'>This computation may take several minutes to complete.</span>"
    text
  })
}
# Run the application 
shinyApp(ui = ui, server = server)