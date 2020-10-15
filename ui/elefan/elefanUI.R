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
          numericInput(ns("ELEFAN_Linf_fix"), p("\\(L_\\infty\\)", ": if used the K-Scan method is applied with a fixed ", withMathJax("\\(L_\\infty\\)")," value (i.e. varying K only):"), NA, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_Linf_range_from"), p(withMathJax("\\(L_\\infty\\)"), "sequence from:"), NULL, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_Linf_range_to"), p(withMathJax("\\(L_\\infty\\)"), "sequence to:"), NULL, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_Linf_range_by"), p(withMathJax("\\(L_\\infty\\)"), "increment sequence by:"), 1, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_C"), "Growth oscillation amplitude (C)", 0, min = 0, max = 100, step=1),
          numericInput(ns("ELEFAN_ts"), p("Onset of the first oscillation relative to summer point (", withMathJax("\\(t_s\\)"), "):"), 0, min = 0, max = 100, step=1),
          numericInput(ns("ELEFAN_MA"), "Number indicating over how many length classes the moving average (MA) should be performed (must be a odd number):", 5, min = 1, max = 101, step=2)
        ),
        box(
         # numericInput(ns("ELEFAN_binSize"), "Bin size:", 4, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_K_Range_from"), "K sequence from:", NULL, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_K_Range_to"), "K sequence to:", NULL, min = 1, max = 1000, step=1),
          numericInput(ns("ELEFAN_K_Range_by"), "K increment sequence by:", 1, min = 1, max = 1000, step=1),
          checkboxInput(ns("ELEFAN_addl.sqrt"), "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE),
          numericInput(ns("ELEFAN_agemax"), "Maximum age of species:", NULL, min = 0, max = 100, step=1),
          numericInput(ns("ELEFAN_PLUS_GROUP"), "Plus group", 0, min = 0, max = 100000, step=1),
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