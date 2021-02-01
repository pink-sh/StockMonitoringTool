tabElefanGa <- function(id) {
  ns <- NS(id)
  tabItem("ElefanGaWidget",
    htmlOutput(ns("elefanGaTitle")),
    htmlOutput("tropFishRLibVersion1", class="subTitle"),
    actionButton(ns("elefanGADataConsiderations"), "Data Considerations", class="topLevelInformationButton"),
      fluidRow(
        bsModal("modalExampleGA", "ELEFAN_GA Data Considerations", ns("elefanGADataConsiderations"), size = "large", htmlOutput(ns("elefanGADataConsiderationsText"))),
          box(title = "Main Parameters",
            width = NULL,
            collapsible = T, 
            class = "collapsed-box",
              box(
                fileInput(ns("fileGa"), "Choose Input CSV File",
                  accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                  )
                ),
              box(
                selectInput(ns("elefanGaDateFormat"), "Choose CSV date format", choices = c("Automatic guess" = "auto", "Year Month Day" = "ymd", "Year Day Month" = "ydm", "Day Month Year" = "dmy", "Month Day Year" = "mdy" ))
                #selectInput(ns("elefanGaDateFormat"), "Choose CSV date format", choices = c("Year Month Day" = "ymd", "Year Day Month" = "ydm", "Day Month Year" = "dmy", "Month Day Year" = "mdy" ))
                 )
            ),
            box(title = "Optional Parameters Initial Values",
              width = NULL,
              collapsible = T, 
              class = "collapsed-box",
              collapsed = T,
              box(
                checkboxInput(ns("ELEFAN_GA_seasonalised"), p("Seasonalised : allows method to calculate the seasonal growth parameters, ", withMathJax("\\(C\\)"), " and ", withMathJax("\\(t_{s}\\)")), FALSE),
                numericInput(ns("ELEFAN_GA_binSize"), "Bin size : length interval over which the length frequency data are aggregated", 1, min = 0.1, max = 100, step=0.1),
                numericInput(ns("ELEFAN_GA_popSize"), "Population size:", 50, min = 0, max = 10000, step=1),
                numericInput(ns("ELEFAN_GA_maxiter"), "Maximum number of iterations to run before the GA search is halted (note this affects the run time):", 10, min = 1, max = 1000, step=1),
                numericInput(ns("ELEFAN_GA_run"), p("Number of consecutive generations without any improvement in the best fitness value before the GA is stopped (", withMathJax("\\(maxiter\\)"), ") (note that this affects the run time):"), 100, min = 1, max = 1000, step=1),
                checkboxInput(ns("ELEFAN_GA_addl.sqrt"), "Additional squareroot transformation of positive values according to Brey et al. (1988), reduces the weighting of large individuals", FALSE)
              ),
              box(
                numericInput(ns("ELEFAN_GA_pmutation"), "Probability of mutation in a parent chromosome. Usually mutation occurs with a small probability:", 0.1, min = 0.1, max = 1, step=0.1),
                numericInput(ns("ELEFAN_GA_pcrossover"), "Probability of crossover between pairs of chromosomes. Typically this is a large value:", 0.8, min = 0.1, max = 1, step=0.1),
                numericInput(ns("ELEFAN_GA_elitism"), "Number of best fitness individuals to survive at each generation:", 5, min = 0, max = 100, step=1),
                numericInput(ns("ELEFAN_GA_MA"), p("Number indicating over how many length classes the moving average should be performed  (", withMathJax("\\(MA\\)"), ") (must be an odd number):"), 5, min = 1, max = 101, step=2),
                numericInput(ns("ELEFAN_GA_PLUS_GROUP"), "Plus group: the largest length class with only a few individuals after which the method will pool together (important for cohort analysis later)", 0, min = 0, max = 100000, step=1)
              )
            ),
            box(title = "Lower Limits Of Parameter Ranges",
              width = NULL,
              collapsible = T, 
              class = "collapsed-box",
              collapsed = T,
              box(
                numericInput(ns("ELEFAN_GA_lowPar_Linf"), p("Asymptotic length/length infinity of the von Bertalanffy growth function (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 119, min = 1, max = 1000, step=1),
                numericInput(ns("ELEFAN_GA_lowPar_K"), p("The growth coefficient (", withMathJax("\\(K\\)"), ") of the von Bertalanffy growth function"), 0.01, min = 0, max = 10, step=0.01),
                numericInput(ns("ELEFAN_GA_lowPar_t_anchor"), p("Time point anchoring the growth curves in the year-length coordinate system, corresponds to the peak spawning month. The fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year (", withMathJax("\\(t_{anchor}\\)"), ")"), 0, min = 0, max = 1, step=0.01)
              ),
              uiOutput(ns("ELEFAN_GA_SeasonLowPar"))
            ),
            box(title = "Upper Limits Of Parameter Ranges",
              width = NULL,
              collapsible = T, 
              class = "collapsed-box",
              collapsed = T,
              box(
                numericInput(ns("ELEFAN_GA_upPar_Linf"), p("Asymptotic length/length infinity of the von Bertalanffy growth function (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 129, min = 1, max = 1000, step=1),
                numericInput(ns("ELEFAN_GA_upPar_K"), p("The growth coefficient (", withMathJax("\\(K\\)"), ") of the von Bertalanffy growth function"), 1, min = 0, max = 10, step=0.01),
                numericInput(ns("ELEFAN_GA_upPar_t_anchor"), p("Time point anchoring the growth curves in the year-length coordinate system, corresponds to the peak spawning month. The fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year (", withMathJax("\\(t_{anchor}\\)"), ")"), 1, min = 0, max = 1, step=0.01)
              ),
              uiOutput(ns("ELEFAN_GA_SeasonUpPar"))
            ),
            tags$div( disabled(actionButton(ns("go_ga"), "Run ELEFAN GA", class="topLevelInformationButton")),
                  actionButton(ns("reset_ga"), "Reset", class="topLevelInformationButton"), style="margin-left: 15px;"),
            hr(),
            box( width= 100, id = "box_elefan_ga_results",
              title = "Results of Elefan GA Computation",
              tags$style(type="text/css",
                ".recalculating {opacity: 1.0;}"
              ),
              fluidRow(
                box(
                  uiOutput(ns("downloadReport_ga")),
                  uiOutput(ns("ElefanGaVREUpload"))
                )
              ),
              fluidRow(
                box(
                  htmlOutput(ns("titlePlot1_elefan_ga")),
                  "Length frequency data visualised in terms of catches.",
                  plotOutput(ns("plot_ga_1"))
                ),
                box(
                  htmlOutput(ns("titlePlot2_elefan_ga")),
                  "Restructured data with bin sizes and the number of bins over which the moving average is calculated as defined in the optional parameters.",
                  plotOutput(ns("plot_ga_2"))
                )
              ),
              fluidRow (
                box("Graphical fit of growth curves plotted through the length frequency data.",
                  plotOutput(ns("plot_ga_5"))),
                box(
                  htmlOutput(ns("rnMax_ga")),
                  htmlOutput(ns("par_ga")),
                  htmlOutput(ns("title_tbl1_ga")),
                  tableOutput(ns("tbl1_ga")),
                  htmlOutput(ns("title_tbl2_ga")),
                  tableOutput(ns("tbl2_ga"))
                )
              ),
              fluidRow (
                box(
                htmlOutput(ns("titlePlot3_elefan_ga")),
                "Results of the Thompson and Bell model: Curves of yield and biomass per recruit. The black dot represents yield and biomass under current fishing pressure. The yellow and red dashed lines represent fishing mortality for maximum sustainable yield (Fmsy) and fishing mortality to fish the stock at 50% of the virgin biomass (F0.5).",
                plotOutput(ns("plot_ga_3"))
              ),
              box(
                htmlOutput(ns("titlePlot4_elefan_ga")),
                "Exploration of impact of different exploitation rates and Lc values on the relative yield per recruit.",
                plotOutput(ns("plot_ga_4"))
              )
            )
          )
        )
      )
}

resetElefanGaInputValues <- function() {
  shinyjs::reset("fileGa")
  shinyjs::reset("ELEFAN_GA_seasonalised")
  shinyjs::reset("ELEFAN_GA_binSize")
  shinyjs::reset("ELEFAN_GA_popSize")
  shinyjs::reset("ELEFAN_GA_maxiter")
  shinyjs::reset("ELEFAN_GA_run")
  shinyjs::reset("ELEFAN_GA_addl.sqrt")
  shinyjs::reset("ELEFAN_GA_pmutation")
  shinyjs::reset("ELEFAN_GA_pcrossover")
  shinyjs::reset("ELEFAN_GA_elitism")
  shinyjs::reset("ELEFAN_GA_MA")
  shinyjs::reset("ELEFAN_GA_PLUS_GROUP")
  shinyjs::reset("ELEFAN_GA_lowPar_Linf")
  shinyjs::reset("ELEFAN_GA_lowPar_K")
  shinyjs::reset("ELEFAN_GA_lowPar_t_anchor")
  shinyjs::reset("ELEFAN_GA_lowPar_C")
  shinyjs::reset("ELEFAN_GA_lowPar_ts")
  shinyjs::reset("ELEFAN_GA_upPar_Linf")
  shinyjs::reset("ELEFAN_GA_upPar_K")
  shinyjs::reset("ELEFAN_GA_upPar_t_anchor")
  shinyjs::reset("ELEFAN_GA_upPar_C")
  shinyjs::reset("ELEFAN_GA_upPar_ts")
  shinyjs::disable("go_ga")
  clearResults("box_elefan_ga_results")
  shinyjs::reset("elefanGaDateFormat")
}