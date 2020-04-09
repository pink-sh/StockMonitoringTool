tabElefanSa <-
  tabItem("ElefanSaWidget",
      htmlOutput("elefanSaTitle"),
      htmlOutput("tropFishRLibVersion2", class="subTitle"),
      actionButton("elefanSADataConsiderations", "Data Considerations", class="topLevelInformationButton"),
      fluidRow(
        bsModal("modalExampleSA", "ELEFAN_SA Data Considerations", "elefanSADataConsiderations", size = "large", htmlOutput("elefanSADataConsiderationsText")),
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
              numericInput("ELEFAN_SA_agemax", "Maximum age of species:", 1, min = 0, max = 100, step=1),
              numericInput("ELEFAN_SA_PLUS_GROUP", "Plus group", 0, min = 0, max = 100000, step=1)
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
            uiOutput("downloadReport_sa"),
            uiOutput("ElefanSaVREUpload")
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
          box(plotOutput("plot_sa_5")),
          box(
            htmlOutput("rnMax_sa"),
            htmlOutput("par_sa"),
            htmlOutput("title_tbl1_sa"),
            tableOutput("tbl1_sa"),
            htmlOutput("title_tbl2_sa"),
            tableOutput("tbl2_sa")
          )
        ),
        fluidRow (
          box(
            htmlOutput("titlePlot3_elefan_sa"),
            plotOutput("plot_sa_3")
          ),
          box(
            htmlOutput("titlePlot4_elefan_sa"),
            plotOutput("plot_sa_4")
          )
        )
      )
    )
  )