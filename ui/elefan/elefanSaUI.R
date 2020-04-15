tabElefanSa <- function(id) {
  ns <- NS(id)
  tabItem("ElefanSaWidget",
      htmlOutput("elefanSaTitle"),
      htmlOutput("tropFishRLibVersion2", class="subTitle"),
      actionButton(ns("elefanSADataConsiderations"), "Data Considerations", class="topLevelInformationButton"),
      fluidRow(
        bsModal("modalExampleSA", "ELEFAN_SA Data Considerations", ns("elefanSADataConsiderations"), size = "large", htmlOutput(ns("elefanSADataConsiderationsText"))),
        box(title = "Main Parameters",
          width = NULL,
          collapsible = T, 
          class = "collapsed-box",
          box(
            fileInput(ns("fileSa"), "Choose Input CSV File",
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
              checkboxInput(ns("ELEFAN_SA_seasonalised"), "Seasonalised", FALSE),
              numericInput(ns("ELEFAN_SA_initPar_Linf"), p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 119, min = 1, max = 1000, step=1),
              numericInput(ns("ELEFAN_SA_initPar_K"), "Curving coefficient (K):", 0.5, min = 0, max = 1, step=0.1),
              numericInput(ns("ELEFAN_SA_initPar_t_anchor"), "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 0.5, min = 0, max = 1, step=0.01),
              checkboxInput(ns("ELEFAN_SA_addl.sqrt"), "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE)
            ),
            box(
              numericInput(ns("ELEFAN_SA_SA_time"), "Maximum running time in seconds:", 60, min = 0, max = 10000, step=1),
              numericInput(ns("ELEFAN_SA_SA_temp"), "Initial value for temperature:", 100000, min = 1, max = 10000000, step=100),
              numericInput(ns("ELEFAN_SA_MA"), "Number indicating over how many length classes the moving average should be performed:", 5, min = 0, max = 100, step=1),
              numericInput(ns("ELEFAN_SA_agemax"), "Maximum age of species:", 1, min = 0, max = 100, step=1),
              numericInput(ns("ELEFAN_SA_PLUS_GROUP"), "Plus group", 0, min = 0, max = 100000, step=1)
            )
          ),
          box(title = "Low Par Parameters",
            width = NULL,
            collapsible = T, 
            class = "collapsed-box",
            collapsed = T,
            box(
              numericInput(ns("ELEFAN_SA_lowPar_Linf"), p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 119, min = 1, max = 1000, step=1),
              numericInput(ns("ELEFAN_SA_lowPar_K"), "Curving coefficient (K):", 0.01, min = 0, max = 1, step=0.01),
              numericInput(ns("ELEFAN_SA_lowPar_t_anchor"), "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 0, min = 0, max = 1, step=0.1)
            ),
            box(
              numericInput(ns("ELEFAN_SA_lowPar_C"), "Amplitude of growth oscillation (C):", 0, min = 0, max = 1, step=0.1),
              numericInput(ns("ELEFAN_SA_lowPar_ts"), p("Summer point (", withMathJax("\\(t_s\\)"), "):"), 0, min = 0, max = 1, step=0.1)
            )
          ),
          box(title = "Up Par Parameters",
            width = NULL,
            collapsible = T, 
            class = "collapsed-box",
            collapsed = T,
          box(
            numericInput(ns("ELEFAN_SA_upPar_Linf"), p("Length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm):"), 129, min = 1, max = 1000, step=1),
            numericInput(ns("ELEFAN_SA_upPar_K"), "Curving coefficient (K):", 1, min = 0, max = 1, step=0.01),
            numericInput(ns("ELEFAN_SA_upPar_t_anchor"), "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month (t_anchor):", 1, min = 0, max = 1, step=0.1)
          ),
          box(
            numericInput("ELEFAN_SA_upPar_C", "Amplitude of growth oscillation (C):", 1, min = 0, max = 1, step=0.1),
            numericInput("ELEFAN_SA_upPar_ts", p("Summer point (", withMathJax("\\(t_s\\)"), "):"), 1, min = 0, max = 1, step=0.1)
          )
        ),
        tags$div( disabled(actionButton(ns("go_sa"), "Run ELEFAN SA", class="topLevelInformationButton")),
                  actionButton(ns("reset_sa"), "Reset", class="topLevelInformationButton"), style="margin-left: 15px;"),
        hr(),
        box( width= 100, id = "box_elefan_sa_results",
          title = "Results of Elefan SA Computation",
          tags$style(type="text/css",
          ".recalculating {opacity: 1.0;}"
        ),
        fluidRow(
          box(
            uiOutput(ns("downloadReport_sa")),
            uiOutput(ns("ElefanSaVREUpload"))
          )
        ),
        fluidRow(
          box(
            htmlOutput(ns("titlePlot1_elefan_sa")),
            plotOutput(ns("plot_sa_1"))
          ),
          box(
            htmlOutput(ns("titlePlot2_elefan_sa")),
            plotOutput(ns("plot_sa_2"))
          )
        ),
        fluidRow (
          box(plotOutput(ns("plot_sa_5"))),
          box(
            htmlOutput(ns("rnMax_sa")),
            htmlOutput(ns("par_sa")),
            htmlOutput(ns("title_tbl1_sa")),
            tableOutput(ns("tbl1_sa")),
            htmlOutput(ns("title_tbl2_sa")),
            tableOutput(ns("tbl2_sa"))
          )
        ),
        fluidRow (
          box(
            htmlOutput(ns("titlePlot3_elefan_sa")),
            plotOutput(ns("plot_sa_3"))
          ),
          box(
            htmlOutput(ns("titlePlot4_elefan_sa")),
            plotOutput(ns("plot_sa_4"))
          )
        )
      )
    )
  )
}


resetElefanSaInputValues <- function() {
  shinyjs::reset("fileSa")
  shinyjs::reset("ELEFAN_SA_seasonalised")
  shinyjs::reset("ELEFAN_SA_initPar_Linf")
  shinyjs::reset("ELEFAN_SA_initPar_K")
  shinyjs::reset("ELEFAN_SA_initPar_t_anchor")
  shinyjs::reset("ELEFAN_SA_addl.sqrt")
  shinyjs::reset("ELEFAN_SA_SA_time")
  shinyjs::reset("ELEFAN_SA_SA_temp")
  shinyjs::reset("ELEFAN_SA_MA")
  shinyjs::reset("ELEFAN_SA_agemax")
  shinyjs::reset("ELEFAN_SA_PLUS_GROUP")
  shinyjs::reset("ELEFAN_SA_lowPar_Linf")
  shinyjs::reset("ELEFAN_SA_lowPar_K")
  shinyjs::reset("ELEFAN_SA_lowPar_t_anchor")
  shinyjs::reset("ELEFAN_SA_lowPar_C")
  shinyjs::reset("ELEFAN_SA_lowPar_ts")
  shinyjs::reset("ELEFAN_SA_upPar_Linf")
  shinyjs::reset("ELEFAN_SA_upPar_K")
  shinyjs::reset("ELEFAN_SA_upPar_t_anchor")
  shinyjs::reset("ELEFAN_SA_upPar_C")
  shinyjs::reset("ELEFAN_SA_upPar_ts")
}