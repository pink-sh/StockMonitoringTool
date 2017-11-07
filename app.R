#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinysky)
library(shinythemes)
library(shinydashboard)
library(RCurl)
library(fishmethods)
library(TropFishR)

buildUrl <- function(session, path) {
  port <- session$clientData$url_port
  host <- session$clientData$url_hostname
  protocol <- session$clientData$url_protocol
  
  url <- paste0(protocol, "//", host, ":", port, "/", path)
  return (url);
}

source("assets/tropFishR/elefan_common.R")

source("assets/tropFishR/run_elefan_ga.R")
source("assets/tropFishR/run_elefan_sa.R")
source("assets/tropFishR/run_elefan.R")

source("assets/cmsy/CmsyFunction.R")

source("assets/fishmethods/methods.R")

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
"

pdf(NULL)
dev.off()
#dev.list()

set.seed(1)
d <- data(package = "TropFishR")
parallel <- FALSE


ui <- tagList(dashboardPage(
  dashboardHeader(title = 'Stock Monitoring Tools'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="homeTab"),
      menuItem("CMSY",
               menuSubItem("Introduction", tabName = "CmsyIntro"),
               menuSubItem("CMSY Vectorized", tabName = "cmsyMethod"),
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
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/eqcss/1.7.0/EQCSS.min.js")),
    tags$head(tags$script(type="text/eqcss", src="styles.eqcss")),
    busyIndicator(wait = 7000),
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
              fluidRow(
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
                      textInput("group", "Group", "Plankton feeders"),
                      textInput("name", "Name", "Sand smelt in Adriatic Sea"),
                      textInput("englishName", "English Name", "Big scale sand smelt"),
                      textInput("scientificName", "Scientific Name", "Atherina boyeri"),
                      textInput("flim", "Fishing mortality biological limit", "NA"),
                      textInput("fpa", "Fishing mortality precautionary value", "NA"),
                      textInput("blim", "Biomass biological limit", "NA"),
                      textInput("bpa", "Biomass precautionary value", "NA"),
                      textInput("bmsy", "Biomass maximum sustainable yield", "NA"),
                      textInput("resiliance", "Resilience as qualitative information", "Medium"),
                      textInput("r.low", "r.low lowest resilience (automatically calculated if not set)", "NA"),
                      textInput("r.hi", "r.hi highest resilience (automatically calculated if not set)", "NA"),
                      
                      numericInput("stb.low", "stb.low lowest possible relative biomass at the beginning of the catch time series", 0.2, min = 0, max = 10, step=0.1),
                      numericInput("stb.hi", "stb.hi highest possible relative biomass at the beginning of the catch time series", 0.6, min = 0, max = 10, step=0.1),
                      
                      textInput("int.yr", "int.yr intermediate year (automatically calculated if not set)", "NA"),
                      textInput("intb.low", "intb.low lowest possible relative biomass at the intermediate year of the catch time series (automatically calculated if not set)", "NA"),
                      textInput("intb.hi", "intb.hi highest possible relative biomass at the intermediate year of the catch time series (automatically calculated if not set)", "NA"),
                      
                      numericInput("endb.low", "endb.low lowest possible relative biomass at the end of the catch time series", 0.01, min = 0, max = 10, step=0.01),
                      numericInput("endb.hi", "endb.hi highest possible relative biomass at the end of the catch time series", 0.4, min = 0, max = 10, step=0.1)
                    ),
                    box(
                      textInput("region", "Region", "Mediterranean"),
                      textInput("subregion", "SubRegion", "Adriatic sea"),
                      numericInput("minOfYear", "Min Year of the catch series", 1970, min = 1900, max = 2020, step=1),
                      numericInput("maxOfYear", "Max Year of the catch series", 2014, min = 1900, max = 2020, step=1),
                      numericInput("startYear", "Start Year to process the catch series from", 1970, min = 1900, max = 2020, step=1),
                      numericInput("endYear", "End Year to process the catch series up to", 2014, min = 1900, max = 2020, step=1),
                      textInput("source", "Source", "-"),
                      textInput("fmsy", "Fishing mortality maximum sustainable yield", "NA"),
                      textInput("msy", "Maximum sustainable yield", "NA"),
                      textInput("msyBTrigger", "Spawning stock biomass at MSY", "NA"),
                      textInput("b40", "Biomass at 40% over the unfished level", "NA"),
                      textInput("m", "Natural mortality", "NA"),
                      textInput("fofl", "Fishing mortality at overfishing level", "NA"),
                      textInput("last_f", "Last known exploitation rate", "NA"),
                      
                      textInput("q.start", "q.start, prior for q value at the beginning of a stable catch-biomass period of min 5 years", "NA"),
                      textInput("q.end", "q.end, prior for q value at the end of a stable catch-biomass period of min 5 years", "NA"),
                      textInput("btype", "btype indicates if the catch file contains biomass, CPUE or no information associated to the catch time series", "None"),
                      
                      textInput("comments", "Comments on data and computation", "landings"),
                      
                      checkboxInput("force.cmsy", "Check this if CMSY results are to be preferred over the Bayesian State Model results when biomass or CPUE is available", FALSE)
                    )
                ),
                actionButton("go_cmsy", "Run CMSY Method"),
                htmlOutput("cmsyWarning"),
                hr(),
                box( width= 100, id = "box_cmsy_results",
                     title = "Results of CMSY Method",
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
              fluidRow(
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
                      numericInput("ELEFAN_GA_popSize", "Population size", 50, min = 0, max = 10000, step=1),
                      numericInput("ELEFAN_GA_maxiter", "Maximum number of iterations to run before the GA search is halted", 10, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_GA_run", "Number of consecutive generations without any improvement in the best fitness value before the GA is stopped", 100, min = 1, max = 1000, step=1),
                      checkboxInput("ELEFAN_GA_addl.sqrt", "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE)
                    ),
                    box(
                      numericInput("ELEFAN_GA_pmutation", "Probability of mutation in a parent chromosome. Usually mutation occurs with a small probability", 0.1, min = 0.1, max = 1, step=0.1),
                      numericInput("ELEFAN_GA_pcrossover", "Probability of crossover between pairs of chromosomes. Typically this is a large value", 0.8, min = 0.1, max = 1, step=0.1),
                      numericInput("ELEFAN_GA_elitism", "Number of best fitness individuals to survive at each generation", 5, min = 0, max = 100, step=1),
                      numericInput("ELEFAN_GA_MA", "Number indicating over how many length classes the moving average should be performed", 5, min = 0, max = 100, step=1)
                    )
                ),
                box(title = "Low Par Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("ELEFAN_GA_lowPar_Linf", "Length of infinity in CM", 119, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_GA_lowPar_K", "Curving coefficient", 0.01, min = 0, max = 1, step=0.01),
                      numericInput("ELEFAN_GA_lowPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 0, min = 0, max = 1, step=0.1)
                    ),
                    box(
                      numericInput("ELEFAN_GA_lowPar_C", "Amplitude of growth oscillation", 0, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_GA_lowPar_ts", "Summer point", 0, min = 0, max = 1, step=0.1)
                    )
                ),
                box(title = "Up Par Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("ELEFAN_GA_upPar_Linf", "Length of infinity in CM", 129, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_GA_upPar_K", "Curving coefficient", 1, min = 0, max = 1, step=0.01),
                      numericInput("ELEFAN_GA_upPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 1, min = 0, max = 1, step=0.1)
                    ),
                    box(
                      numericInput("ELEFAN_GA_upPar_C", "Amplitude of growth oscillation", 1, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_GA_upPar_ts", "Summer point", 1, min = 0, max = 1, step=0.1)
                    )
                ),
                actionButton("go_ga", "Run ELEFAN GA"),
                hr(),
                
                box( width= 100, id = "box_elefan_ga_results",
                     title = "Results of Elefan GA Computation",
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
                         htmlOutput("par_ga")
                       )
                     )
                )
              )
      ),
      tabItem("ElefanSaWidget",
              htmlOutput("elefanSaTitle"),
              fluidRow(
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
                      numericInput("ELEFAN_SA_initPar_Linf", "Length of infinity in CM", 119, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_SA_initPar_K", "Curving coefficient", 0.5, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_SA_initPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 0.5, min = 0, max = 1, step=0.01),
                      checkboxInput("ELEFAN_SA_addl.sqrt", "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE)
                    ),
                    box(
                      numericInput("ELEFAN_SA_SA_time", "Maximum running time in seconds", 60, min = 0, max = 10000, step=1),
                      numericInput("ELEFAN_SA_SA_temp", "Initial value for temperature", 100000, min = 1, max = 10000000, step=100),
                      numericInput("ELEFAN_SA_MA", "Number indicating over how many length classes the moving average should be performed", 5, min = 0, max = 100, step=1),
                      numericInput("ELEFAN_SA_agemax", "Maximum age of species", 1, min = 0, max = 100, step=1)
                    )
                ),
                box(title = "Low Par Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("ELEFAN_SA_lowPar_Linf", "Length of infinity in CM", 119, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_SA_lowPar_K", "Curving coefficient", 0.01, min = 0, max = 1, step=0.01),
                      numericInput("ELEFAN_SA_lowPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 0, min = 0, max = 1, step=0.1)
                    ),
                    box(
                      numericInput("ELEFAN_SA_lowPar_C", "Amplitude of growth oscillation", 0, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_SA_lowPar_ts", "Summer point", 0, min = 0, max = 1, step=0.1)
                    )
                ),
                box(title = "Up Par Parameters",
                    width = NULL,
                    collapsible = T, 
                    class = "collapsed-box",
                    collapsed = T,
                    box(
                      numericInput("ELEFAN_SA_upPar_Linf", "Length of infinity in CM", 129, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_SA_upPar_K", "Curving coefficient", 1, min = 0, max = 1, step=0.01),
                      numericInput("ELEFAN_SA_upPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 1, min = 0, max = 1, step=0.1)
                    ),
                    box(
                      numericInput("ELEFAN_SA_upPar_C", "Amplitude of growth oscillation", 1, min = 0, max = 1, step=0.1),
                      numericInput("ELEFAN_SA_upPar_ts", "Summer point", 1, min = 0, max = 1, step=0.1)
                    )
                ),
                actionButton("go_sa", "Run ELEFAN SA"),
                hr(),
                
                box( width= 100, id = "box_elefan_sa_results",
                     title = "Results of Elefan SA Computation",
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
                         htmlOutput("par_sa")
                       )
                     )
                )
              )
      ),
      tabItem("ElefanWidget",
              htmlOutput("elefanTitle"),
              fluidRow(
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
                      numericInput("ELEFAN_Linf_fix", "Linf: if used the K-Scan method is applied with a fixed Linf value (i.e. varying K only)", NA, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_Linf_range_from", "Linf sequence from", NULL, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_Linf_range_to", "Linf sequence to", NULL, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_Linf_range_by", "Linf increment sequence by", 1, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_C", "Growth oscillation amplitude", 0, min = 0, max = 100, step=1),
                      numericInput("ELEFAN_ts", "Onset of the first oscillation relative to summer point", 0, min = 0, max = 100, step=1),
                      numericInput("ELEFAN_MA", "Number indicating over how many length classes the moving average should be performed", 5, min = 0, max = 100, step=1)
                    ),
                    box(
                      numericInput("ELEFAN_K_Range_from", "Linf sequence from", NULL, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_K_Range_to", "Linf sequence to", NULL, min = 1, max = 1000, step=1),
                      numericInput("ELEFAN_K_Range_by", "Linf increment sequence by", 1, min = 1, max = 1000, step=1),
                      checkboxInput("ELEFAN_addl.sqrt", "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE),
                      numericInput("ELEFAN_agemax", "Maximum age of species", NULL, min = 0, max = 100, step=1),
                      checkboxInput("ELEFAN_contour", "if checked in combination with response surface analysis, contour lines are displayed rather than the score as text in each field of the score plot", FALSE)
                    )
                ),
                actionButton("go", "Run ELEFAN"),
                hr(),
                
                box( width= 100,  id = "box_elefan_results",
                     title = "Results of Elefan SA Computation",
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
                         htmlOutput("par")
                       )
                     )
                )
              )
      ),
      tabItem("SBPRWidget",
              htmlOutput("sbprTitle"),
              fluidRow(
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
                      numericInput("SBPR_M", "Single natural mortality (M) rate if M is assumed constant over all ages", 0.2, min = 0, max = 10, step=0.1),
                      numericInput("SBPR_pM", "Proportion of natural mortality that occurs before spawning", 0.1667, min = 0, max = 10, step=0.0001),
                      numericInput("SBPR_maxF", "Maximum value of F range over which SBPR will be calculated", 2, min = 0, max = 100, step=1)
                    ),
                    box(
                      numericInput("SBPR_pF", "Proportion of fishing mortality that occurs before spawning", 0.2, min = 0, max = 10, step=0.1),
                      numericInput("SBPR_MSP", "Percentage of maximum spawning potential (percent MSP reference point) for which F and SBPR should be calculated", 30, min = 0, max = 1000, step=1),
                      numericInput("SBPR_incrF", "F increment for SBPR calculation", 0.001, min = 0, max = 10, step=0.001)
                    )
                ),
                actionButton("go_sbpr", "Run SBPR"),
                hr(),
                
                box( width= 100,  id = "box_sbpr_results",
                     title = "Results of Fishmethods - SBPR Computation",
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
              fluidRow(
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
                         tableOutput("yprOutTable")
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
  output$fill <- renderUI({
    inFile1 <- input$file1
    
    if (is.null(inFile1)) {
      return(NULL)
    }
    a <- read.csv(inFile1$datapath)
    
    selectInput("stock", "Select a stock", sort(unique(a$Stock)))
  })
  
  
  output$cmsyMethodTitle <- renderText({
    text <- "<span><h3><b>CMSY (Catch Maximum, Sustainable Yield) Method</b></h3></span>"
    text
  })
  
  output$downloadCmsyReportButton <- renderUI({
    if (!is.null(cmsy$dlmTools) || !is.null(cmsy$legacy) || !is.null(cmsy$fast)) {
      downloadButton("downloadCmsyReport", label = "Download Report")
    }
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
        title <- "<h2> Management Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$titleCmsyAnalisysChart <- renderText({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) {
        title <- "<h2> Analysis Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$renderCmsyInfo <- renderText({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) {
        cmsy$method$text <- gsub("\n\r", "<br/>", cmsy$method$text)
        cmsy$method$text <- gsub("\n", "<br/>", cmsy$method$text)
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
  
  observeEvent(input$go_cmsy, {
    
    query <- parseQueryString(session$clientData$url_search)
    
    if (is.null(query[['serviceToken']])) {
      showModal(modalDialog(
        title = "Error",
        "serviceToken parameter is missing",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    vreToken <- query[['serviceToken']]
    
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
    print(inputCsvFile)
    
    templateFileDlmTools <- paste0(getwd(), "/assets/cmsy/cmsyFastTemplate.xml")
    
    cmsy$fast <- list()
    js$disableAllButtons()
    ret <- runCmsy(input$region,input$subregion,input$stock,input$group,input$name,input$englishName,input$scientificName,input$source,input$minOfYear,input$maxOfYear,input$startYear,input$endYear,input$flim,input$fpa,input$blim,input$bpa,input$bmsy,input$fmsy,input$msy,input$msyBTrigger,input$b40,input$m,input$fofl,input$last_f,input$resiliance,input$r.low,input$r.hi,input$stb.low,input$stb.hi,input$int.yr,input$intb.low,input$intb.hi,input$endb.low,input$endb.hi,input$q.start,input$q.end,input$btype,input$force.cmsy,input$comments, vreToken, inputCsvFile, templateFileDlmTools)
    js$enableAllButtons()
    js$showBox("box_cmsy_results")
    for(i in 1:nrow(ret)) {
      row <- ret[i,]
      if (row$description == "estimates") {
        contents <- getURL(row$url)
        cmsy$method$textRaw <- contents
        contents <- gsub("\n\r", "<br/>", contents)
        contents <- gsub("\n", "<br/>", contents)
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
  })
  ### ---- ELEFAN METHODS -----------
  
  elefan_ga <- reactiveValues()
  elefan_sa <- reactiveValues()
  elefan <- reactiveValues()
  
  observeEvent(input$go_expand, {
    
  })
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
    inputCsvFile <- infile$datapath
    js$removeBox("box_elefan_ga_results")
    js$disableAllButtons()
    #ds <- lfqModify(get(input$ELEFAN_GA_dataset, asNamespace('TropFishR')), bin_size = input$ELEFAN_GA_binSize)
    #ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    dataset <- read_elefan_csv(inputCsvFile)
    ds <- lfqModify(dataset, bin_size = 4)
    
    res <- run_elefan_ga(ds,binSize =  4, seasonalised = input$ELEFAN_GA_seasonalised, 
                         low_par = list(Linf = input$ELEFAN_GA_lowPar_Linf, K = input$ELEFAN_GA_lowPar_K, t_anchor = input$ELEFAN_GA_lowPar_t_anchor, C = input$ELEFAN_GA_lowPar_C, ts = input$ELEFAN_GA_lowPar_ts),
                         up_par = list(Linf = input$ELEFAN_GA_upPar_Linf, K = input$ELEFAN_GA_upPar_K, t_anchor = input$ELEFAN_GA_upPar_t_anchor, C = input$ELEFAN_GA_upPar_C, ts = input$ELEFAN_GA_upPar_ts),
                         popSize = input$ELEFAN_GA_popSize, maxiter = input$ELEFAN_GA_maxiter, run = input$ELEFAN_GA_run, pmutation = input$ELEFAN_GA_pmutation, pcrossover = input$ELEFAN_GA_pcrossover,
                         elitism = input$ELEFAN_GA_elitism, MA = input$ELEFAN_GA_MA, addl.sqrt = input$ELEFAN_GA_addl.sqrt)
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
  
  output$rnMax_ga <- renderText({
    if ("results" %in% names(elefan_ga)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", elefan_ga$results$data$Rn_max)
      title
    } else {  "" }
  })
  output$par_ga <- renderText({
    if ("results" %in% names(elefan_ga)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity in cm:</strong>&nbsp;", elefan_ga$results$data$par$Linf)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient:</strong>&nbsp;", elefan_ga$results$data$par$K)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month:</strong>&nbsp;", elefan_ga$results$data$par$t_anchor)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation:</strong>&nbsp;", elefan_ga$data$results$par$C)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (ts = WP - 0.5):</strong>&nbsp;", elefan_ga$results$data$par$ts)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", elefan_ga$results$data$par$phiL)
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
    inputCsvFile <- infile$datapath
    js$removeBox("box_elefan_ga_results")
    js$disableAllButtons()
    #ds <- lfqModify(get(input$ELEFAN_GA_dataset, asNamespace('TropFishR')), bin_size = input$ELEFAN_GA_binSize)
    #ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    dataset <- read_elefan_csv(inputCsvFile)
    ds <- lfqModify(dataset, bin_size = 4)
    
    #ds <<- lfqModify(get(input$ELEFAN_GA_dataset, asNamespace('TropFishR')), bin_size = input$ELEFAN_GA_binSize)
    ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    
    res <- run_elefan_sa(ds,binSize =  4, seasonalised = input$ELEFAN_GA_seasonalised, 
                         init_par = list(Linf = input$ELEFAN_SA_initPar_Linf, K = input$ELEFAN_SA_initPar_K, t_anchor = input$ELEFAN_SA_initPar_t_anchor),
                         low_par = list(Linf = as.numeric(input$ELEFAN_SA_lowPar_Linf), K = as.numeric(input$ELEFAN_SA_lowPar_K), t_anchor = as.numeric(input$ELEFAN_SA_lowPar_t_anchor), C = as.numeric(input$ELEFAN_SA_lowPar_C), ts = as.numeric(input$ELEFAN_SA_lowPar_ts)),
                         up_par = list(Linf = as.numeric(input$ELEFAN_SA_upPar_Linf), K = as.numeric(input$ELEFAN_SA_upPar_K), t_anchor = as.numeric(input$ELEFAN_SA_upPar_t_anchor), C = as.numeric(input$ELEFAN_SA_upPar_C), ts = as.numeric(input$ELEFAN_SA_upPar_ts)),
                         SA_time = input$ELEFAN_SA_SA_time, SA_temp = input$ELEFAN_SA_SA_temp, MA = input$ELEFAN_SA_MA, addl.sqrt = input$ELEFAN_SA_addl.sqrt,
                         agemax = input$ELEFAN_SA_agemax)
    js$enableAllButtons()
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      js$showBox("box_elefan_sa_results")
      elefan_sa$results <- res
    }
  })
  
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
  
  output$rnMax_sa <- renderText({
    if ("results" %in% names(elefan_sa)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", elefan_sa$results$data$Rn_max)
      title
    } else {  "" }
  })
  output$par_sa <- renderText({
    if ("results" %in% names(elefan_sa)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity in cm:</strong>&nbsp;", elefan_sa$results$data$par$Linf)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient:</strong>&nbsp;", elefan_sa$results$data$par$K)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month:</strong>&nbsp;", elefan_sa$results$data$par$t_anchor)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation:</strong>&nbsp;", elefan_sa$results$data$par$C)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (ts = WP - 0.5):</strong>&nbsp;", elefan_sa$results$data$par$ts)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", elefan_sa$results$data$par$phiL)
      title
    } else {  "" }
  })
  output$downloadReport_sa <- renderUI({
    if ("results" %in% names(elefan_sa)) {
      downloadButton('createElefanSAReport', 'Download Report')
    }
  })
  output$createElefanSAReport <- downloadHandler(
    filename = paste("ElefanSA_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "elefan_sa.Rmd")
      file.copy("assets/tropFishR/elefan_sa.Rmd", tempReport, overwrite = TRUE)
      params <- list(elefan = elefan_sa)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  
  
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
    inputCsvFile <- infile$datapath
    js$removeBox("box_elefan_results")
    js$disableAllButtons()
    #ds <- lfqModify(get(input$ELEFAN_GA_dataset, asNamespace('TropFishR')), bin_size = input$ELEFAN_GA_binSize)
    #ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    dataset <- read_elefan_csv(inputCsvFile)
    ds <- lfqModify(dataset, bin_size = 4)
    
    #ds <<- lfqModify(get(input$ELEFAN_GA_dataset, asNamespace('TropFishR')), bin_size = input$ELEFAN_GA_binSize)
    ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    
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
  
  output$rnMax <- renderText({
    if ("results" %in% names(elefan)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", elefan$results$data$Rn_max)
      title
    } else {  "" }
  })
  output$par <- renderText({
    if ("results" %in% names(elefan)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity in cm:</strong>&nbsp;", elefan$results$data$par$Linf)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient:</strong>&nbsp;", elefan$results$data$par$K)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month:</strong>&nbsp;", elefan$results$data$par$t_anchor)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation:</strong>&nbsp;", elefan$results$data$par$C)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (ts = WP - 0.5):</strong>&nbsp;", elefan$results$data$par$ts)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", elefan$results$data$par$phiL)
      title
    } else {  "" }
  })
  
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
  
  
  output$titlePlot1_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Reconstructed LFQ data</p>"
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
  
  output$titleResultsOfTheComputation_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<h2>Results of the ELEFAN computation</h2>"
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
      txt <- "<p class=\"pheader_elefan\">Reconstructed LFQ data</p>"
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
  
  output$titlePlot1_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Reconstructed LFQ data</p>"
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
  
  output$cmsySampleDataset <- renderText({
    link <- "<a href='https://goo.gl/YgAfGu' target='_blank'>Click Here</a>"
    text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>CMSY</b> methods", "</h4></p>")
    text
  })
  output$elefanSampleDataset <- renderText({
    link <- "<a href='https://goo.gl/tsqt64' target='_blank'>Click Here</a>"
    text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>Elefan</b> methods", "</h4></p>")
    text
  })
  
  output$cmsyForDlmToolsTitle <- renderText({
    text <- "<span><h3><b>CMSY for DLMTools</b></h3>"
    text
  })
  output$cmsyLegacyTitle <- renderText({
    text <- "<span><h3><b>CMSY Legacy</b></h3></span>"
    text
  })
  
  output$elefanGaTitle <- renderText({
    text <- "<span><h3><b>Elefan GA (Generic Algorithm)</b></h3></span>"
    text
  })
  output$elefanSaTitle <- renderText({
    text <- "<span><h3><b>Elefan GA (Simulated Annealing)</b></h3></span>"
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
  output$cmsyLegacyWarning <- renderText({
    text <- "<span style='margin-left: 20px;'>This computation may take several minutes to complete.</span>"
    text
  })
  
  output$cmsyIntroOut <- renderText({
    text <- "<h3><b>CMSY - Catch Maximum, Sustainable Yield</b></h3>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "The <b>CMSY</b> method for data-limited stock assessment. Described in Froese, R., Demirel, N., Coro, G., Kleisner, K. M., Winker, H. (2016). Estimating fisheries reference points from catch and resilience. Fish and Fisheries.")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<a href='http://onlinelibrary.wiley.com/doi/10.1111/faf.12190/full' target='_blank'>Click here to read the paper.</a>")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<span><b>CMSY Vectorized</b> is a new adaptation of CMSY designed to increase fitting speed to enable implementation in management strategy evaluation. <br/>This is achieved by adding adaptive parameter search bounds to restrict the inspected r-K space and automatically increase depletion priors if necessary.</span>")
    text <- paste0(text, "</p>")
    
    
    text
  })
  output$elefanIntroOut <- renderText({
    text <- "<h3><b>Elefan methods by TropFishR</b></h3>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>Elefan</b> is a computational method designed to estimate life history parameters using a time series of length frequency observations.")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "These methods are provided by the <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR library</a>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The TropFishR has three different Elefan methods: <b>ELEFAN</b>, <b>ELEFAN GA</b> and <b>ELEFAN SA</b>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<p>")
    
    text <- paste0(text, "<p>")
    text <- paste0(text, "<h4>ELEFAN</h4>")
    text <- paste0(text, "<b>E</b>lectronic <b>LE</b>ngth <b>F</b>requency <b>AN</b>alysis for estimating growth parameter.<br/>This functions allows to perform the K-Scan and Response surface analysis to estimate growth parameters. It combines the step of restructuring length-frequency data (lfqRestructure) followed by the fitting of VBGF curves through the restructured data (lfqFitCurves). K-Scan is a method used to search for the K parameter with the best fit while keeping the Linf fixed. In contrast, with response surface analysis both parameters are estimated and the fits are displayed in a heatmap. Both methods use optimise to find the best t_anchor value for each combination of K and Linf. To find out more about t_anchor, please refer to the Details description of lfqFitCurves. The score value Rn_max is not comparable with the score value of the other ELEFAN functions (ELEFAN_SA or ELEFAN_GA).")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<h4>ELEFAN GA (generic algorithm)</h4>")
    text <- paste0(text, "<b>E</b>lectronic <b>LE</b>ngth <b>F</b>requency <b>AN</b>alysis with simulated annealing for estimating growth parameters.<br/>A more detailed description of the generic algorithm (GA) can be found in Scrucca (2013). The score value fitnessValue is not comparable with the score value of the other ELEFAN functions (ELEFAN or ELEFAN_SA).")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<p>")
    text <- paste0(text, "<h4>ELEFAN SA (simulated annealing)</h4>")
    text <- paste0(text, "<b>E</b>lectronic <b>LE</b>ngth <b>F</b>requency <b>AN</b>alysis with simulated annealing for estimating growth parameters.<br/>A more detailed description of the simulated annealing (SA) can be found in Xiang et al. (2013). The score value cost_value is not comparable with the score value of the other ELEFAN functions (ELEFAN or ELEFAN_GA).")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<br/>")
    
    
    link <- "<a href='https://goo.gl/tsqt64' target='_blank'>Click Here</a>"
    text <- paste0(text, "<h4>Information on the dataset used</h4>")
    text <- paste0(text, "<p><h5>", link,"&nbsp; to download a sample dataset that can be used with <b>Elefan</b> methods", "</h5></p>")
    text <- paste0(text, "<span class=\"elefan_info\">The dataset used by this example is the <b>synLFQ7</b></span><br><br>")
    text <- paste0(text, "<span class=\"elefan_info\">Synthetic length-frequency data as generated by the function lfqGen from the fishdynr package (Taylor 2016). Can be used by <b>ELEFAN</b>, <b>ELEFAN_SA</b>, or <b>ELEFAN_GA</b>. <br>The data is generated with the following von Bertalanffy growth parameters:</span>")
    text <- paste0(text, "<ul style=\"margin-top: 10px;\">")
    text <- paste0(text, "<li>K = 0.2 +/- 0.1 (CV)</li>")
    text <- paste0(text, "<li>Linf = 123 +/- 0.05 (CV)</li>")
    text <- paste0(text, "<li>C = 0.3</li>")
    text <- paste0(text, "<li>ts = 0</li>")
    text <- paste0(text, "<li>t_anchor between 0.16 and 0.34 (Time when yearly recruitment pulse occurs; e.g. 0 = Jan 1, 0.25 = Apr 1, 0.5 = Jul 1, 0.75 = Oct 1; repro_wt = c(0, 0, 0.2, 1, 0.6, 0, 0, 0, 0, 0, 0, 0))</li>")
    text <- paste0(text, "</ul>")
    
    text <- paste0(text, "<br/>")
    
    text <- paste0(text, "<h4>Glossary</h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>lfqRestructure:</b> First step of the Electronic LEngth Frequency ANalysis (ELEFAN), which is restructuring lengthfrequency data (lfq). This is done according to a certain protocol, described by many authors (see Details or References for more information).")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>lfqFitCurves:</b> This function estimates von Bertalanffy growth function (VBGF) curves for a set of growth parameters.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>lfqFitCurves:</b> This function estimates von Bertalanffy growth function (VBGF) curves for a set of growth parameters.")
    text <- paste0(text, "</p>")
    
    
    text
  })
  
  #### SBPR Functions
  
  sbprExec <- reactiveValues()

  output$sbprTitle <- renderText({
    text <- "<span><h3><b>Spawning stock biomass-per-recruit (SBPR)</b></h3></span>"
    text
  })
  
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
    js$removeBox("box_sbpr_results")
    inputCsvFile <- infile$datapath
    dat <- read.csv(inputCsvFile)

    res <- sbpr_shinyApp(age=dat$age,ssbwgt=dat$ssbwgt,partial=dat$partial,pmat=dat$pmat,M=input$SBPR_M,pF=input$SBPR_pF, pM=input$SBPR_pM,MSP=input$SBPR_MSP,plus=FALSE,maxF=input$SBPR_maxF,incrF=input$SBPR_incrF, graph=FALSE)
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
  
  output$sbprOutPlot1 <- renderPlot({
    if ('results' %in% names(sbprExec)) {
      plot(sbprExec$results$F_vs_SPR[,2]~sbprExec$results$F_vs_SPR[,1],ylab="SPR",xlab="Fishing Mortality",type="l")
      abline(h=sbprExec$results$Reference_Point[1,2], col = "red", lty = 2)
      legend(1.4, 8, legend=c("SSB_per_recruit"),col=c("red"), lty=2, cex=0.8)
    }
  })
  output$sbprOutPlot2 <- renderPlot({
    if ('results' %in% names(sbprExec)) {
      plot(sbprExec$results$F_vs_SPR[,3]~sbprExec$results$F_vs_SPR[,1],ylab="% Max SPR",xlab="Fishing Mortality",type="l")
      abline(h=input$SBPR_MSP, v = sbprExec$results$Reference_Point[1,1], col = "red", lty = 2)
      leg <- paste0(input$SBPR_MSP, "% MSP")
      legend(1.5, 85, legend=c(leg),col=c("red"), lty=2, cex=0.8)
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
      df
    }
  }, 
  include.rownames=FALSE)
  
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
  
  
  #### YPR Functions
  
  yprExec <- reactiveValues()
  
  output$yprTitle <- renderText({
    text <- "<span><h3><b>Yield-per-recruit (YPR)</b></h3></span>"
    text
  })
  
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
    js$removeBox("box_ypr_results")
    inputCsvFile <- infile$datapath
    dat <- read.csv(inputCsvFile)
    
    res <- ypr_shinyApp(age=dat$age,wgt=dat$ssbwgt,partial=dat$partial,M=input$YPR_M,plus=input$YPR_Plus,oldest=input$YPR_oldest,maxF=input$YPR_maxF,incrF=input$YPR_incrF, graph = FALSE)
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
  
  output$yprOutPlot <- renderPlot({
    if ('results' %in% names(yprExec)) {
      YPR <- yprExec$results$F_vs_YPR
      plot(YPR[,2]~YPR[,1],ylab="Yield-Per-Recruit",xlab="Fishing Mortality",type="l")
      abline(h = yprExec$results$Reference_Points[2,2], v = yprExec$results$Reference_Points[2,1], col = "black", lty = 2)
      abline(h = yprExec$results$Reference_Points[1,2], v = yprExec$results$Reference_Points[1,1], col = "red", lty = 2)
      legend(1.7, 0.09, legend=c("F-0.1", "F-Max"),col=c("red", "black"), lty=2:2, cex=0.8)
    }
  })
  
  output$yprOutTable <- renderTable({
    if ('results' %in% names(yprExec)) {
      yprExec$results$Reference_Points
    }
  }, 
  include.rownames=TRUE)
  
  output$yprDifference <- renderText({
    if ('results' %in% names(yprExec)) {
      differenceinYPR = round(yprExec$results$Reference_Points[2,2] - yprExec$results$Reference_Points[1,2], 6)
      text <- paste0("<b>Difference in YPR: </b>",differenceinYPR)
      text
    }
  })
  
  output$downloadYprReport <- renderUI({
    if ("results" %in% names(yprExec)) {
      colnames(yprExec$results$Reference_Points) <- c("F", "Yield_Per_Recruit")
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
  
  output$fishMethodsIntroOut <- renderText({
    text <- "<h3><b>FishMethods</b></h3>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>Fishmethods: </b>Fishery science methods and models from published literature and contributions from colleagues.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<h4>Methods used</h4>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>SBPR</b> Spawning stock biomass-per-recruit(SBPR) analysis is conducted following Gabriel et al. (1989). Reference points of F and SBPR for a percentage of maximum spawning potential are calculated.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>YPR</b> Yield-per-recruit (YPR) analysis is conducted following the modified Thompson-Bell algorithm. Reference points Fmax and F0.1 are calculated.")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>SBPR Options:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Input file: </b> Input data file</li>")
    text <- paste0(text, "<li><b>M: </b> Single natural mortality (M) rate if M is assumed constant over all ages</li>")
    text <- paste0(text, "<li><b>pF: </b> Proportion of fishing mortality that occurs before spawning</li>")
    text <- paste0(text, "<li><b>pM: </b> Proportion of natural mortality that occurs before spawning</li>")
    text <- paste0(text, "<li><b>MSP: </b> Percentage of maximum spawning potential (percent MSP reference point) for which F and SBPR should be calculated</li>")
    text <- paste0(text, "<li><b>maxF: </b> Maximum value of F range over which SBPR will be calculated</li>")
    text <- paste0(text, "<li><b>incrF: </b> F increment for SBPR calculation</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>YPR Options:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Input file: </b> Input data file</li>")
    text <- paste0(text, "<li><b>M: </b> Single natural mortality (M) rate if M is assumed constant over all ages</li>")
    text <- paste0(text, "<li><b>maxF: </b> Maximum value of F range over which YPR will be calculated. YPR is calculated for F = 0 to maxF</li>")
    text <- paste0(text, "<li><b>plus: </b> logical value indicating whether the last age is a plus-group</li>")
    text <- paste0(text, "<li><b>oldest: </b> if plus is checked, a numeric value indicating the oldest age in the plus group</li>")
    text <- paste0(text, "<li><b>incrF: </b> F increment for YPR calculation</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    
    text
  })
  
  output$fishMethodsSampleDataset <- renderText({
    link <- "<a href='https://goo.gl/24FuzG' target='_blank'>Click Here</a>"
    text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>FishMethods</b>", "</h4></p>")
    text
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)