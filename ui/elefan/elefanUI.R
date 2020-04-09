tabElefan <-
tabItem("ElefanWidget",
  htmlOutput("elefanTitle"),
  htmlOutput("tropFishRLibVersion3", class="subTitle"),
  actionButton("elefanDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
  fluidRow(
    bsModal("modalExampleElefan", "ELEFAN Data Considerations", "elefanDataConsiderations", size = "large", htmlOutput("elefanDataConsiderationsText")),
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
        numericInput("ELEFAN_PLUS_GROUP", "Plus group", 0, min = 0, max = 100000, step=1),
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
        uiOutput("downloadReport"),
        uiOutput("ElefanVREUpload")
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
        box(plotOutput("plot_5")),
          box(
            htmlOutput("rnMax"),
            htmlOutput("par"),
            htmlOutput("title_tbl1_e"),
            tableOutput("tbl1_e"),
            htmlOutput("title_tbl2_e"),
            tableOutput("tbl2_e")
          )
        ),
        fluidRow (
          box(
            htmlOutput("titlePlot3_elefan"),
            plotOutput("plot_3")
          ),
          box(
            htmlOutput("titlePlot4_elefan"),
            plotOutput("plot_4")
          )
        )
      )
    )
  )