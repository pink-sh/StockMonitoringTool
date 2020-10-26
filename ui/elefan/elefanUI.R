tabElefan <- function(id) {
  ns <- NS(id)
  tabItem("ElefanWidget",
    htmlOutput(ns("elefanTitle")),
    htmlOutput("tropFishRLibVersion3", class="subTitle"),
    actionButton(ns("elefanDataConsiderations"), "Data Considerations", class="topLevelInformationButton"),
    fluidRow(
      bsModal("modalExampleElefan", "ELEFAN Data Considerations", ns("elefanDataConsiderations"), size = "large", htmlOutput(ns("elefanDataConsiderationsText"))),
      box(title = "Main Parameters",
        width = NULL, 
        collapsible = T, 
        class = "collapsed-box",
          box(
            fileInput(ns("fileElefan"), "Choose Input CSV File",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")
            )
          ),
          box(
            selectInput(ns("elefanDateFormat"), "Choose CSV date format", choices = c("Automatic guess" = "auto", "Year Month Day" = "ymd", "Year Day Month" = "ydm", "Day Month Year" = "dmy", "Month Day Year" = "mdy" ))
            #selectInput(ns("elefanDateFormat"), "Choose CSV date format", choices = c("Year Month Day" = "ymd", "Year Day Month" = "ydm", "Day Month Year" = "dmy", "Month Day Year" = "mdy" ))
            )
      ),
      box(title = "Optional Parameters",
        width = NULL,
        collapsible = T, 
        class = "collapsed-box",
        collapsed = T,
        box(
          numericInput(ns("ELEFAN_Linf_fix"), p("Asymptotic length/length infinity ( ", withMathJax("\\(L_\\infty\\)"), " in cm). If ", withMathJax("\\(L_\\infty\\)")," is defined here, then the K-Scan method is applied with a fixed ", withMathJax("\\(L_\\infty\\)")," value (i.e. varying K only):"), NA, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_Linf_range_from"), p("Lower limit of ",withMathJax("\\(L_\\infty\\)"), "sequence range of potential ",withMathJax("\\(L_\\infty\\)")," values (in cm). Default is the last length class minus 5 cm. (size of ", withMathJax("\\(L_\\infty\\)"),"  range can impact method run time)"), NULL, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_Linf_range_to"), p("Upper limit of ",withMathJax("\\(L_\\infty\\)"), " sequence range (in cm).  Default is the last length class plus 5 cm."), NULL, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_Linf_range_by"), p("Increment (in cm) of the ",withMathJax("\\(L_\\infty\\)"), " sequence range"), 1, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_C"), p("Amplitude of growth oscillation (", withMathJax("\\(C\\)"),"): The higher the value of C the more pronounced are the seasonal oscillations. C = 0 implies that there is no seasonality in the growth rate;  if C = 1, the growth rate becomes zero at the winter point."), 0, min = 0, max = 100, step=1),
          numericInput(ns("ELEFAN_ts"), p("Summer point ", withMathJax("\\(t_s\\)"),". Values between 0 and 1. At the time of the year when the fraction, ", withMathJax("\\(t_{s}\\)"), ", has elapsed, the growth rate is the highest."), 0, min = 0, max = 100, step=1),
          numericInput(ns("ELEFAN_MA"), p("Number indicating over how many length classes the moving average (", withMathJax("\\(MA\\)"),") should be performed (must be an odd number):"), 5, min = 1, max = 101, step=2)
        ),
        box(
         # numericInput(ns("ELEFAN_binSize"), "Bin size : length interval over which the length frequency data are aggregated", 4, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_K_Range_from"), p("Lower limit of the sequence range of the growth coefficient (", withMathJax("\\(K\\)"),") of the von Bertalanffy growth function (size of K  range can impact method run time)"), NULL, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_K_Range_to"), p("Upper limit of the sequence range of the growth coefficient (", withMathJax("\\(K\\)"),") of the von Bertalanffy growth function"), NULL, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_K_Range_by"), p("Increment of the ", withMathJax("\\(K\\)")," sequence range"), 1, min = 1, max = 1000, step=1),
          checkboxInput(ns("ELEFAN_addl.sqrt"), "Additional squareroot transformation of positive values according to Brey et al. (1988), reduces the weighting of large individuals", FALSE),
          numericInput(ns("ELEFAN_agemax"), p("Maximum age of species (if not specified, estimated from ", withMathJax("\\(t_s\\)"),"):"), 1, min = 0, max = 100, step=1),
          numericInput(ns("ELEFAN_PLUS_GROUP"), "Plus group: the largest length class with only a few individuals after which the method will pool together (important for cohort analysis later)", 0, min = 0, max = 100000, step=1),
          checkboxInput(ns("ELEFAN_contour"), "If checked in combination with response surface analysis, contour lines are displayed rather than the score as text in each field of the score plot", FALSE)
        )
      ),
      tags$div( disabled(actionButton(ns("go"), "Run ELEFAN", class="topLevelInformationButton")),
                actionButton(ns("reset_elefan"), "Reset", class="topLevelInformationButton"), style="margin-left: 15px;"),
      hr(),
      box( width= 100,  id = "box_elefan_results",
        title = "Results of Elefan Computation",
        tags$style(type="text/css",
          ".recalculating {opacity: 1.0;}"
      ),
      fluidRow(
        box(
          uiOutput(ns("downloadReport")),
          uiOutput(ns("ElefanVREUpload"))
        )
      ),
      fluidRow(
        box(
          htmlOutput(ns("titlePlot1_elefan")),
          "Length frequency data visualised in terms of catches.",
          plotOutput(ns("plot_1"))
        ),
          box(
            htmlOutput(ns("titlePlot2_elefan")),
            "Restructured data with bin sizes and the number of bins over which the moving average is calculated as defined in the optional parameters.",
            plotOutput(ns("plot_2"))
          )
        ),
        fluidRow (
          box("Graphical fit of growth curves plotted through the length frequency data.",
            plotOutput(ns("plot_5"))),
            box(
              htmlOutput(ns("rnMax")),
              htmlOutput(ns("par")),
              htmlOutput(ns("title_tbl1_e")),
              tableOutput(ns("tbl1_e")),
              htmlOutput(ns("title_tbl2_e")),
              tableOutput(ns("tbl2_e"))
            )
          ),
          fluidRow (
            box(
              htmlOutput(ns("titlePlot3_elefan")),
              "Results of the Thompson and Bell model: Curves of yield and biomass per recruit. The black dot represents yield and biomass under current fishing pressure. The yellow and red dashed lines represent fishing mortality for maximum sustainable yield (Fmsy) and fishing mortality to fish the stock at 50% of the virgin biomass (F0.5).",
              plotOutput(ns("plot_3"))
            ),
            box(
              htmlOutput(ns("titlePlot4_elefan")),
              "Exploration of impact of different exploitation rates and Lc values on the relative yield per recruit.",
              plotOutput(ns("plot_4"))
            )
          )
        )
      )
    )
}


resetElefanInputValues <- function() {
  shinyjs::reset("fileElefan")
  shinyjs::reset("ELEFAN_Linf_fix")
  shinyjs::reset("ELEFAN_Linf_range_from")
  shinyjs::reset("ELEFAN_Linf_range_to")
  shinyjs::reset("ELEFAN_Linf_range_by")
  shinyjs::reset("ELEFAN_C")
  shinyjs::reset("ELEFAN_ts")
  shinyjs::reset("ELEFAN_MA")
 # shinyjs::reset("ELEFAN_binSize")
  shinyjs::reset("ELEFAN_K_Range_from")
  shinyjs::reset("ELEFAN_K_Range_to")
  shinyjs::reset("ELEFAN_K_Range_by")
  shinyjs::reset("ELEFAN_addl.sqrt")
  shinyjs::reset("ELEFAN_agemax")
  shinyjs::reset("ELEFAN_PLUS_GROUP")
  shinyjs::reset("ELEFAN_contour")
  shinyjs::disable("go")
  clearResults("box_elefan_results")
  shinyjs::reset("elefanDateFormat")
}