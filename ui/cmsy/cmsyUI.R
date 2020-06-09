tabCmsyIntro <- tabItem("cmsyIntro",htmlOutput("cmsyIntroOut"))
tabCmsySampleDataset <- tabItem("cmsySampleDataset",htmlOutput("cmsySampleDataset"))

tabCmsy <- function(id) {
  ns <- NS(id)
  tabItem("cmsyWidget",
          htmlOutput(ns("cmsyMethodTitle")),
          actionButton("cmsyDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
          fluidRow(
            bsModal("modalExampleCMSY", "CMSY Data Considerations", "cmsyDataConsiderations", size = "large", htmlOutput(ns("cmsyDataConsiderationsText"))),
            box(title = "Main Parameters",
                width = NULL,
                collapsible = T, 
                class = "collapsed-box",
                box(
                  fileInput(ns("fileCmsy"), "Choose Stock CSV File",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv", id="fileCmsy")
                  )
                ),
                box(
                  tags$div(id="stockSelectorContainer")
                )
            ),
            box(title = "Optional Parameters",
                width = NULL,
                collapsible = T, 
                class = "collapsed-box",
                collapsed = T,
                box(
                  numericInput(ns("minOfYear"), "Earliest year of the catch series", 2005, min = 1900, max = 2030, step=1),
                  numericInput(ns("maxOfYear"), "Latest year of the catch series", 2016, min = 1900, max = 2030, step=1),
                  selectInput(ns("resiliance"), "Resilience as qualitative information (Use information from FishBase or SeaLifeBase)", choices=c("Very low", "Low", "Medium", "High"), selected="Medium"),
                  textInput(ns("r.low"), "Lowest resilience (automatically calculated if not set)", "NA"),
                  textInput(ns("r.hi"), "Highest resilience (automatically calculated if not set)", "NA"),
                  p("**The user should take care when setting the prior estimates for depletion at the beginning and end of the time series. Depletion levels are assumptions about the initial and current state of the stock, and they have a strong influence on the results of CMSY, so careful evaluation of these parameters is recommended. These parameters are determined in CMSY using the relationship between current catch and maximum catch."),
                  numericInput(ns("stb.low"), "**Starting depletion range: Lowest possible relative biomass at the beginning of the catch time series (automatically calculated if not set)", 0, min = 0, max = 10, step=0.1),
                  numericInput(ns("stb.hi"), "**Starting depletion range: Highest possible relative biomass at the beginning of the catch time series (automatically calculated if not set)", 0, min = 0, max = 10, step=0.1),
                  textInput(ns("int.yr"), "Intermediate year (automatically calculated if not set)", "NA"),
                  textInput(ns("intb.low"), "Lowest possible relative biomass at the intermediate year of the catch time series (automatically calculated if not set)", "NA"),
                  textInput(ns("intb.hi"), "Highest possible relative biomass at the intermediate year of the catch time series (automatically calculated if not set)", "NA"),
                  numericInput(ns("endb.low"), "**Ending depletion range: Lowest possible relative biomass at the end of the catch time series (automatically calculated if not set)", 0.01, min = 0, max = 10, step=0.01),
                  numericInput(ns("endb.hi"), "**Ending depletion range: Highest possible relative biomass at the end of the catch time series (automatically calculated if not set)", 0.4, min = 0, max = 10, step=0.1),
                  textInput(ns("q.start"), "Prior for catchability (q) value at the beginning of a stable catch-biomass period of minimum 5 years", "NA"),
                  textInput(ns("q.end"), "Prior for q value at the end of a stable catch-biomass period of minimum 5 years", "NA")
                ),
                box(
                  numericInput(ns("startYear"), "Start year to process the catch series from", 2005, min = 1900, max = 2030, step=1),
                  numericInput(ns("endYear"), "End year to process the catch series up to", 2016, min = 1900, max = 2030, step=1),
                  textInput(ns("blim"), p("Biomass biological limit (", withMathJax("\\(B_{lim}\\)"), ")"), "NA"),
                  textInput(ns("bpa"), p("Biomass precautionary value (",withMathJax("\\(B_{pa}\\)") , ")"), "NA"),
                  textInput(ns("bmsy"), p("Biomass maximum sustainable yield (", withMathJax("\\(B_{MSY}\\)"), ")"), "NA"),
                  textInput(ns("b40"), p("Biomass at 40% over the unfished level (", withMathJax("\\(B_{40\\%}\\)"), ")"), "NA"),
                  textInput(ns("fmsy"), p("Fishing mortality at Maximum Sustainable Yield (",withMathJax("\\(F_{MSY}\\)") , "). If" 
                                          ,withMathJax("\\(F_{MSY}\\)") ,"is known, the resilience prior range (lowest and highest resilience estimates) 
              could be defined to include estimate of", withMathJax("\\(F_{MSY}\\)") , 
                                          "assuming that r", withMathJax("\\(\\approx\\)"),withMathJax("\\(F_{MSY}\\)")), "NA"),
                  textInput(ns("flim"), p("Fishing mortality biological limit (", withMathJax("\\(F_{lim}\\)"), ")"), "NA"),
                  textInput(ns("fpa"), p("Fishing mortality precautionary value (", withMathJax("\\(F_{pa}\\)"), ")"), "NA"),
                  textInput(ns("fofl"), p("Fishing mortality at overfishing level (", withMathJax("\\(F_{ofl}\\)"),")"), "NA"),
                  textInput(ns("last_f"), "Last known exploitation rate", "NA"),
                  textInput(ns("msy"), "Maximum Sustainable Yield (MSY)", "NA"),
                  textInput(ns("msyBTrigger"), p("Spawning Stock Biomass at MSY (", withMathJax("\\(SSB_{MSY}\\)"), ")"), "NA"),
                  textInput(ns("m"), "**Natural mortality (M)", "NA"),
                  p("**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M here."),
                  ##KK: it's not clear to me what the user input would be here if not "None". Suggest deleting (also for Comments.
                  #textInput("btype", "btype indicates if the catch file contains biomass, CPUE or no information associated with the catch time series", "None"),
                  #textInput("comments", "Comments on data and computation", "landings"),
                  checkboxInput(ns("force.cmsy"), "Check this if CMSY results are to be preferred over the Bayesian State Model results (only when biomass or CPUE is available)", FALSE)
                )
            ),
            tags$div(disabled(actionButton(ns("go_cmsy"), "Run CMSY Method", class="topLevelInformationButton")),
                     actionButton(ns("reset_cmsy"), "Reset", class="topLevelInformationButton"), style="margin-left: 15px;")
            ,
            htmlOutput("cmsyWarning"),
            hr(),
            box( width= 100, id = "box_cmsy_results",
                 title = "Results of CMSY Method",
                 tags$style(type="text/css",
                            ".recalculating {opacity: 1.0;}"
                 ),
                 fluidRow(
                   box(
                     uiOutput(ns("downloadCmsyReportButton")),
                     uiOutput(ns("CmsyVREUpload"))
                   )
                 ),
                 fluidRow(
                   box(
                     htmlOutput(ns("renderCmsyLog")),
                     htmlOutput(ns("renderCmsyInfo"))
                   ),
                   box(id = "box_cmsy_results_charts",
                       htmlOutput(ns("titleCmsyManagementChart")),
                       imageOutput(ns("renderCmsyManagementChart")),
                       htmlOutput(ns("titleCmsyAnalisysChart")),
                       imageOutput(ns("renderCmsyAnalysisChart"))
                   )
                 )
            )
          )
  )
}

resetCmsyInputValues <- function() {
  shinyjs::reset("fileCmsy")
  shinyjs::reset("minOfYear")
  shinyjs::reset("maxOfYear")
  shinyjs::reset("resiliance")
  shinyjs::reset("r.low")
  shinyjs::reset("r.hi")
  shinyjs::reset("stb.low")
  shinyjs::reset("stb.hi")
  shinyjs::reset("int.yr")
  shinyjs::reset("intb.low")
  shinyjs::reset("intb.hi")
  shinyjs::reset("endb.low")
  shinyjs::reset("endb.hi")
  shinyjs::reset("q.start")
  shinyjs::reset("q.end")
  shinyjs::reset("startYear")
  shinyjs::reset("endYear")
  shinyjs::reset("blim")
  shinyjs::reset("bpa")
  shinyjs::reset("bmsy")
  shinyjs::reset("b40")
  shinyjs::reset("fmsy")
  shinyjs::reset("flim")
  shinyjs::reset("fpa")
  shinyjs::reset("fofl")
  shinyjs::reset("last_f")
  shinyjs::reset("msy")
  shinyjs::reset("msyBTrigger")
  shinyjs::reset("m")
  shinyjs::reset("force.cmsy")
  removeUI(selector="#stockSelectorContainerInner")
  shinyjs::disable("go_cmsy")
  clearResults("box_cmsy_results")
}