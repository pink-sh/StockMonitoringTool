tabElefanSa <- function(id) {
  ns <- NS(id)
  tabItem("ElefanSaWidget",
      htmlOutput(ns("elefanSaTitle")),
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
              ),
            box(
              selectInput(ns("elefanSaDateFormat"), "Choose CSV date format", choices = c("Automatic guess" = "auto", "Year Month Day" = "ymd", "Year Day Month" = "ydm", "Day Month Year" = "dmy", "Month Day Year" = "mdy" ))
              #selectInput(ns("elefanSaDateFormat"), "Choose CSV date format", choices = c("Year Month Day" = "ymd", "Year Day Month" = "ydm", "Day Month Year" = "dmy", "Month Day Year" = "mdy" ))            
              )
          ),
          box(title = "Optional Parameters Initial Values",
            width = NULL,
            collapsible = T, 
            class = "collapsed-box",
            collapsed = T,
            box(
              checkboxInput(ns("ELEFAN_SA_seasonalised"), "Seasonalised : allows method to calculate the seasonal growth parameters, C and ts", FALSE),
             # numericInput(ns("ELEFAN_SA_binSize"), "Bin size : length interval over which the length frequency data are aggregated", 4, min = 1, max = 100, step=1),
              numericInput(ns("ELEFAN_SA_initPar_Linf"), p("Asymptotic length/length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm) of the von Bertalanffy growth function:"), 119, min = 1, max = 1000, step=1),
              numericInput(ns("ELEFAN_SA_initPar_K"), "Growth coefficient (K) of the von Bertalanffy growth function", 0.5, min = 0, max = 1, step=0.1),
              numericInput(ns("ELEFAN_SA_initPar_t_anchor"), "Time point anchoring the growth curves in the year-length coordinate system, corresponds to the peak spawning month. The fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year (t_anchor)", 0.5, min = 0, max = 1, step=0.01),
              numericInput(ns("ELEFAN_SA_initPar_C"), "Amplitude of growth oscillation (C):", 0.5, min = 0, max = 1, step=0.1),
              numericInput(ns("ELEFAN_SA_initPar_ts"), p("Summer point (", withMathJax("\\(t_s\\)"), "). Values between 0 and 1.   At the time of the year when the fraction, " , withMathJax("\\(t_s\\)"),", has elapsed, the growth rate is the highest."), 0.5, min = 0, max = 1, step=0.1)
              ),
            box(
              numericInput(ns("ELEFAN_SA_SA_time"), "Maximum running time in seconds (will impact the run time of the method):", 60, min = 0, max = 10000, step=1),
              numericInput(ns("ELEFAN_SA_SA_temp"), "Initial value for 'temperature' (will impact the run time of the method):", 100000, min = 1, max = 10000000, step=100),
              numericInput(ns("ELEFAN_SA_MA"), "Number indicating over how many length classes the moving average (MA) should be performed (must be an odd number):", 5, min = 1, max = 101, step=2),
              numericInput(ns("ELEFAN_SA_agemax"), "Maximum age of species (if not specified, estimated from Linf):", 1, min = 0, max = 100, step=1),
              numericInput(ns("ELEFAN_SA_PLUS_GROUP"), "Plus group: the largest length class with only a few individuals after which the method will pool together (important for cohort analysis later)", 0, min = 0, max = 100000, step=1),
              checkboxInput(ns("ELEFAN_SA_addl.sqrt"), "Additional squareroot transformation of positive values according to Brey et al. (1988), reduces the weighting of large individuals", FALSE)
            )
          ),
          box(title = "Lower Limits Of Parameters Ranges",
            width = NULL,
            collapsible = T, 
            class = "collapsed-box",
            collapsed = T,
            box(
              numericInput(ns("ELEFAN_SA_lowPar_Linf"), p("Asymptotic length/length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm) of the von Bertalanffy growth function:"), 119, min = 1, max = 1000, step=1),
              numericInput(ns("ELEFAN_SA_lowPar_K"), "Growth coefficient (K) of the von Bertalanffy growth function", 0.01, min = 0, max = 1, step=0.01),
              numericInput(ns("ELEFAN_SA_lowPar_t_anchor"), "Time point anchoring the growth curves in the year-length coordinate system, corresponds to the peak spawning month. The fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year (t_anchor)", 0, min = 0, max = 1, step=0.1)
            ),
            box(
              numericInput(ns("ELEFAN_SA_lowPar_C"), "Amplitude of growth oscillation (C): The higher the value of C the more pronounced are the seasonal oscillations. C = 0 implies that there is no seasonality in the growth rate;  if C = 1, the growth rate becomes zero at the winter point.", 0, min = 0, max = 1, step=0.1),
              numericInput(ns("ELEFAN_SA_lowPar_ts"), p("Summer point (", withMathJax("\\(t_s\\)"), "). Values between 0 and 1.   At the time of the year when the fraction, " , withMathJax("\\(t_s\\)"),", has elapsed, the growth rate is the highest."), 0, min = 0, max = 1, step=0.1)
            )
          ),
          box(title = "Upper Limits Of Parameters Ranges",
            width = NULL,
            collapsible = T, 
            class = "collapsed-box",
            collapsed = T,
          box(
            numericInput(ns("ELEFAN_SA_upPar_Linf"), p("Asymptotic length/length infinity (",withMathJax("\\(L_\\infty\\)"), "in cm) of the von Bertalanffy growth function:"), 129, min = 1, max = 1000, step=1),
            numericInput(ns("ELEFAN_SA_upPar_K"), "Growth coefficient (K) of the von Bertalanffy growth function", 1, min = 0, max = 1, step=0.01),
            numericInput(ns("ELEFAN_SA_upPar_t_anchor"), "Time point anchoring the growth curves in the year-length coordinate system, corresponds to the peak spawning month. The fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year (t_anchor)", 1, min = 0, max = 1, step=0.1)
          ),
          box(
            numericInput(ns("ELEFAN_SA_upPar_C"), "Amplitude of growth oscillation (C): The higher the value of C the more pronounced are the seasonal oscillations. C = 0 implies that there is no seasonality in the growth rate;  if C = 1, the growth rate becomes zero at the winter point.", 1, min = 0, max = 1, step=0.1),
            numericInput(ns("ELEFAN_SA_upPar_ts"), p("Summer point (", withMathJax("\\(t_s\\)"), "). Values between 0 and 1.   At the time of the year when the fraction, " , withMathJax("\\(t_s\\)"),", has elapsed, the growth rate is the highest."), 1, min = 0, max = 1, step=0.1)
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
            "Length frequency data visualised in terms of catches.",
            plotOutput(ns("plot_sa_1"))
          ),
          box(
            htmlOutput(ns("titlePlot2_elefan_sa")),
            "Restructured data with bin sizes and the number of bins over which the moving average is calculated as defined in the optional parameters.",
            plotOutput(ns("plot_sa_2"))
          )
        ),
        fluidRow (
          box("Graphical fit of growth curves plotted through the length frequency data.",
            plotOutput(ns("plot_sa_5"))),
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
            "Results of the Thompson and Bell model: Curves of yield and biomass per recruit. The black dot represents yield and biomass under current fishing pressure. The yellow and red dashed lines represent fishing mortality for maximum sustainable yield (Fmsy) and fishing mortality to fish the stock at 50% of the virgin biomass (F0.5).",
            plotOutput(ns("plot_sa_3"))
          ),
          box(
            htmlOutput(ns("titlePlot4_elefan_sa")),
            "Exploration of impact of different exploitation rates and Lc values on the relative yield per recruit.",
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
 # shinyjs::reset("ELEFAN_SA_binSize")
  shinyjs::reset("ELEFAN_SA_initPar_Linf")
  shinyjs::reset("ELEFAN_SA_initPar_K")
  shinyjs::reset("ELEFAN_SA_initPar_t_anchor")
  shinyjs::reset("ELEFAN_SA_initPar_C")
  shinyjs::reset("ELEFAN_SA_initPar_ts")
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
  shinyjs::disable("go_sa")
  clearResults("box_elefan_sa_results")
  shinyjs::reset("elefanSaDateFormat")
}