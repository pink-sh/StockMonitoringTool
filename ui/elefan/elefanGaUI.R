tabElefanGa <- function(id) {
  ns <- NS(id)
  tabItem("ElefanGaWidget",
    htmlOutput("elefanGaTitle"),
    htmlOutput("tropFishRLibVersion1", class="subTitle"),
    actionButton("elefanGADataConsiderations", "Data Considerations", class="topLevelInformationButton"),
      fluidRow(
        bsModal("modalExampleGA", "ELEFAN_GA Data Considerations", "elefanGADataConsiderations", size = "large", htmlOutput("elefanGADataConsiderationsText")),
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
              )
            ),
            box(title = "Optional Parameters",
              width = NULL,
              collapsible = T, 
              class = "collapsed-box",
              collapsed = T,
              box(
                checkboxInput(ns("ELEFAN_GA_seasonalised"), "Seasonalised", FALSE),
                numericInput(ns("ELEFAN_GA_popSize"), "Population size:", 50, min = 0, max = 10000, step=1),
                numericInput(ns("ELEFAN_GA_maxiter"), "Maximum number of iterations to run before the GA search is halted:", 10, min = 1, max = 1000, step=1),
                numericInput(ns("ELEFAN_GA_run"), "Number of consecutive generations without any improvement in the best fitness value before the GA is stopped:", 100, min = 1, max = 1000, step=1),
                checkboxInput(ns("ELEFAN_GA_addl.sqrt"), "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE)
              ),
              box(
                numericInput(ns("ELEFAN_GA_pmutation"), "Probability of mutation in a parent chromosome. Usually mutation occurs with a small probability:", 0.1, min = 0.1, max = 1, step=0.1),
                numericInput(ns("ELEFAN_GA_pcrossover"), "Probability of crossover between pairs of chromosomes. Typically this is a large value:", 0.8, min = 0.1, max = 1, step=0.1),
                numericInput(ns("ELEFAN_GA_elitism"), "Number of best fitness individuals to survive at each generation:", 5, min = 0, max = 100, step=1),
                numericInput(ns("ELEFAN_GA_MA"), "Number indicating over how many length classes the moving average should be performed:", 5, min = 0, max = 100, step=1),
                numericInput(ns("ELEFAN_GA_PLUS_GROUP"), "Plus group", 0, min = 0, max = 100000, step=1)
              )
            ),
            box(title = "Low Par Parameters",
              width = NULL,
              collapsible = T, 
              class = "collapsed-box",
              collapsed = T,
              box(
                numericInput(ns("ELEFAN_GA_lowPar_Linf"), p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 119, min = 1, max = 1000, step=1),
                numericInput(ns("ELEFAN_GA_lowPar_K"), "Curving coefficient (K):", 0.01, min = 0, max = 1, step=0.01),
                numericInput(ns("ELEFAN_GA_lowPar_t_anchor"), "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 0, min = 0, max = 1, step=0.1)
              ),
              box(
                numericInput(ns("ELEFAN_GA_lowPar_C"), "Amplitude of growth oscillation (C):", 0, min = 0, max = 1, step=0.1),
                numericInput(ns("ELEFAN_GA_lowPar_ts"), p("Summer point (", withMathJax("\\(t_s\\)"), "):"), 0, min = 0, max = 1, step=0.1)
              )
            ),
            box(title = "Up Par Parameters",
              width = NULL,
              collapsible = T, 
              class = "collapsed-box",
              collapsed = T,
              box(
                numericInput(ns("ELEFAN_GA_upPar_Linf"), p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 129, min = 1, max = 1000, step=1),
                numericInput(ns("ELEFAN_GA_upPar_K"), "Curving coefficient (K):", 1, min = 0, max = 1, step=0.01),
                numericInput(ns("ELEFAN_GA_upPar_t_anchor"), "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 1, min = 0, max = 1, step=0.1)
              ),
              box(
                numericInput(ns("ELEFAN_GA_upPar_C"), "Amplitude of growth oscillation (C):", 1, min = 0, max = 1, step=0.1),
                numericInput(ns("ELEFAN_GA_upPar_ts"), p("Summer point (", withMathJax("\\(t_s\\)"), "):"), 1, min = 0, max = 1, step=0.1)
              )
            ),
            tags$div( actionButton(ns("go_ga"), "Run ELEFAN GA", class="topLevelInformationButton"),
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
                  plotOutput(ns("plot_ga_1"))
                ),
                box(
                  htmlOutput(ns("titlePlot2_elefan_ga")),
                  plotOutput(ns("plot_ga_2"))
                )
              ),
              fluidRow (
                box(plotOutput(ns("plot_ga_5"))),
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
                plotOutput(ns("plot_ga_3"))
              ),
              box(
                htmlOutput(ns("titlePlot4_elefan_ga")),
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
}