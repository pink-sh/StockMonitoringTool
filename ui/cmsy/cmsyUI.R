tabCmsyIntro <- tabItem("CmsyIntro",htmlOutput("cmsyIntroOut"))
tabCmsySampleDataset <- tabItem("CmsySampleDataset",htmlOutput("cmsySampleDataset"))

tabCmsy <- 
  tabItem("cmsyMethod",
        htmlOutput("cmsyMethodTitle"),
        actionButton("cmsyDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
        fluidRow(
        bsModal("modalExampleCMSY", "CMSY Data Considerations", "cmsyDataConsiderations", size = "large", htmlOutput("cmsyDataConsiderationsText")),
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
            numericInput("minOfYear", "Earliest year of the catch series", 2005, min = 1900, max = 2030, step=1),
            numericInput("maxOfYear", "Latest year of the catch series", 2016, min = 1900, max = 2030, step=1),
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
            numericInput("startYear", "Start year to process the catch series from", 2005, min = 1900, max = 2030, step=1),
            numericInput("endYear", "End year to process the catch series up to", 2016, min = 1900, max = 2030, step=1),
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
            uiOutput("downloadCmsyReportButton"),
            uiOutput("CmsyVREUpload")
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
  )