tabElefanGa <- function(id) {
    ns <- NS(id)
    tabItem("ElefanGaWidget",

            htmlOutput(ns("elefanGaTitle")),

            htmlOutput("tropFishRLibVersion1", class="subTitle"),

            actionButton(ns("elefanGADataConsiderations"), "Data Considerations", class="topLevelInformationButton"),

            fluidRow(

                ## Information tabs
                ## -------------------------------
                hr(),
                bsModal("modalExampleGA", "ELEFAN_GA Data Considerations",
                        ns("elefanGADataConsiderations"),
                        size = "large",
                        htmlOutput(ns("elefanGADataConsiderationsText"))),

                bsModal("info_binsize", "Bin size", ns("infoBS"),
                        size = "large",
                        "Bin size corresponds to the length interval over which the length frequency data are aggregated, for example 2cm."),

                bsModal("info_ma", "Moving average (MA)", ns("infoMA"),
                        size = "large",
                        p("Number indicating over how many length classes the moving average should be performed  (", withMathJax("\\(MA\\)"), ") (must be an odd number):")),


                bsModal("info_pg", "Plus group", ns("infoPG"),
                        size = "large",
                        "Plus group: the largest length class with only a few individuals after which the method will pool together (important for cohort analysis later)"),


                bsModal("info_at", "Additional squareroot transformation", ns("infoAT"),
                        size = "large",
                        "Additional squareroot transformation? of positive values according to Brey et al. (1988), reduces the weighting of large individuals"),


                bsModal("info_linf", withMathJax("\\(L_\\infty\\)"), ns("infolinf"),
                        size = "large",
                        p("Asymptotic length/length infinity of the von Bertalanffy growth function (",withMathJax("\\(L_\\infty\\)"), "in cm):")),



                ## Input - Data upload
                ## -------------------------------

                box(id = "box_datupload",
                    title = "Data upload",
                    width = NULL,
                    collapsible = T,
                    solidHeader = TRUE,
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
                        selectInput(ns("elefanGaDateFormat"),
                                    "Choose CSV date format",
                                    choices = c("Automatic guess" = "auto",
                                                "Year Month Day" = "ymd",
                                                "Year Day Month" = "ydm",
                                                "Day Month Year" = "dmy",
                                                "Month Day Year" = "mdy" ))
                        ## selectInput(ns("elefanGaDateFormat"),
                        ##             "Choose CSV date format",
                        ##             choices = c("Year Month Day" = "ymd",
                        ##                         "Year Day Month" = "ydm",
                        ##                         "Day Month Year" = "dmy",
                        ##                         "Month Day Year" = "mdy" ))
                    )
                    ),



                ## Input - Settings
                ## -------------------------------
                hr(),

                tabBox(
                    title = "Settings",
                    width = NULL,
                    height = "1200px",
                    side="left",
                    selected = "Length data",
                                        # The id lets us use input$tabset1 on the server to find the current tab
                    id = "settings",

                    tabPanel("Length data",

                             box(

                                 numericInput(ns("ELEFAN_GA_binSize"),
                                              p("Bin size",
                                                actionButton(ns("infoBS"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                              2, min = 0.5, max = 100, step=0.5),
                                 numericInput(ns("ELEFAN_GA_MA"),
                                              p("Moving Average (MA)",
                                                actionButton(ns("infoMA"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                              5, min = 3, max = 101, step=2)
                             ),
                             box(
                                 numericInput(ns("ELEFAN_GA_PLUS_GROUP"),
                                              p("Plus group",
                                                actionButton(ns("infoPG"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                              0, min = 0, max = 100000, step=1),  ## HERE: change to yes no, if yes pick from length classes
                                 checkboxInput(ns("ELEFAN_GA_addl.sqrt"),
                                               p("Additional squareroot transformation?",
                                                actionButton(ns("infoAT"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                 FALSE)
                             )
                             ),



                    tabPanel("Growth parameters",

                             box(title = "Parameter search space",
                                 width = 12,
                                 box(
                                     sliderInput(ns("ELEFAN_GA_Linf"),
                                                 p(withMathJax("\\(L_\\infty\\)"),
                                                actionButton(ns("infolinf"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                                value=c(119,129), min = 1, max = 1000, step=1),
                                     sliderInput(ns("ELEFAN_GA_K"),
                                                 p("The growth coefficient (", withMathJax("\\(K\\)"), ") of the von Bertalanffy growth function"),
                                                 value=c(0.01,1), min = 0, max = 10, step=0.01),
                                     sliderInput(ns("ELEFAN_GA_t_anchor"), p("Time point anchoring the growth curves in the year-length coordinate system, corresponds to the peak spawning month. The fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year (", withMathJax("\\(t_{anchor}\\)"), ")"), 0, min = 0, max = 1, step=0.01)
                                 ),
                                 box(
                                     checkboxInput(ns("ELEFAN_GA_seasonalised"), p("Seasonalised : allows method to calculate the seasonal growth parameters, ", withMathJax("\\(C\\)"), " and ", withMathJax("\\(t_{s}\\)")), FALSE)
                                 ),
                                 box(id="box_elefan_ga_seasonPar",
                                     sliderInput(ns("ELEFAN_GA_C"), p("Amplitude of growth oscillation (", withMathJax("\\(C\\)"), "): The higher the value of C the more pronounced are the seasonal oscillations. C = 0 implies that there is no seasonality in the growth rate;  if C = 1, the growth rate becomes zero at the winter point."), value=c(0,1), min = 0, max = 1, step=0.1),
                                     sliderInput(ns("ELEFAN_GA_ts"), p("Summer point (", withMathJax("\\(t_{s}\\)"), "). Values between 0 and 1. At the time of the year when the fraction, " , withMathJax("\\(t_{s}\\)"),", has elapsed, the growth rate is the highest."), value=c(0,1), min = 0, max = 1, step=0.1)
                                     )
                                 ),
                             box(title = "Settings for optimisation procedure",
                                 width = 12,
                                 box(
                                     numericInput(ns("ELEFAN_GA_popSize"), "Population size:", 50, min = 0, max = 10000, step=1),
                                     numericInput(ns("ELEFAN_GA_maxiter"), "Maximum number of iterations to run before the GA search is halted (note this affects the run time):", 10, min = 1, max = 1000, step=1),
                                     numericInput(ns("ELEFAN_GA_run"), p("Number of consecutive generations without any improvement in the best fitness value before the GA is stopped (", withMathJax("\\(maxiter\\)"), ") (note that this affects the run time):"), 100, min = 1, max = 1000, step=1)
                                 ),
                                 box(
                                     numericInput(ns("ELEFAN_GA_pmutation"), "Probability of mutation in a parent chromosome. Usually mutation occurs with a small probability:", 0.1, min = 0.1, max = 1, step=0.1),
                                     numericInput(ns("ELEFAN_GA_pcrossover"), "Probability of crossover between pairs of chromosomes. Typically this is a large value:", 0.8, min = 0.1, max = 1, step=0.1),
                                     numericInput(ns("ELEFAN_GA_elitism"), "Number of best fitness individuals to survive at each generation:", 5, min = 0, max = 100, step=1)
                                 ))
                             ),

                    tabPanel("Mortality rates"

                             ),

                    tabPanel("Stock status"

                             )

                ),




                ## Action buttons
                ## -------------------------------
                hr(),

                box(title = "Analysis",
                    width = NULL,
                    collapsible = F,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = F,

                    br(),br(),
                    tags$div(
                             disabled(actionButton(ns("go_ga"),
                                                   "Run Analysis",
                                                   class="topLevelInformationButton")
                                      ),
                             actionButton(ns("reset_ga"),
                                          "Reset",
                                          class="topLevelInformationButton"),
                             style = "margin-left: 40%;"
                         ),

                    br(),br()

                    ),





                ## Results
                ## -------------------------------
                hr(),
                box(id = "box_results",
                    title = "Results",
                    width = NULL,
                    collapsible = T,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = T,

                    hr(),
                    box(id = "box_data_results",
                        title = "Length frequency data",
                        width = NULL,
                        collapsible = F,
                        class = "collapsed-box",
                        collapsed = F,
                        tags$style(type="text/css",
                                   ".recalculating {opacity: 1.0;}"),
                        ## box( width= NULL, id = "box_data_results",
                        ##     title = "Length frequency data",
                        ##     tags$style(type="text/css",
                        ##                ".recalculating {opacity: 1.0;}"),
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
                        )
                        ),

                    hr(),
                    box( width= NULL, id = "box_elefan_ga_results",
                        collapsible = F,
                        class = "collapsed-box",
                        collapsed = F,
                        title = "Growth parameters",
                        tags$style(type="text/css",
                                   ".recalculating {opacity: 1.0;}"
                                   ),
                        fluidRow (
                            box("Graphical fit of growth curves plotted through the length frequency data.",
                                plotOutput(ns("plot_ga_5")))
                        ),
                        box(width=12,
                            htmlOutput(ns("rnMax_ga")),
                            htmlOutput(ns("par_ga"))
                            )
                        ),
                    hr(),
                    box( width= NULL, id = "box_mort_results",
                        collapsible = F,
                        class = "collapsed-box",
                        collapsed = F,
                        title = "Mortality rates",
                        tags$style(type="text/css",
                                   ".recalculating {opacity: 1.0;}")
                        ),
                    hr(),
                    box( width= NULL, id = "box_ypr_results",
                        collapsible = F,
                        class = "collapsed-box",
                        collapsed = F,
                        title = "Stock status",
                        tags$style(type="text/css",
                                   ".recalculating {opacity: 1.0;}"),
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
                        ),
                        box(width=12,
                            htmlOutput(ns("title_tbl1_ga")),
                            tableOutput(ns("tbl1_ga")),
                            htmlOutput(ns("title_tbl2_ga")),
                            tableOutput(ns("tbl2_ga"))
                            )
                        )
                    ),

                ## Report
                ## -------------------------------
                hr(),
                box(title = "Report",
                    id = "box_report",
                    width = NULL,
                    collapsible = F,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = F,
                    tags$style(type="text/css",
                               ".recalculating {opacity: 1.0;}"),
                    br(),br(),
                    tags$div(
                             uiOutput(ns("downloadReport_ga")),
                             uiOutput(ns("ElefanGaVREUpload")),
                             style = "margin-left: 45%;"
                         ),

                    br(),br()
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
    shinyjs::reset("ELEFAN_GA_Linf")
    shinyjs::reset("ELEFAN_GA_K")
    shinyjs::reset("ELEFAN_GA_t_anchor")
    shinyjs::reset("ELEFAN_GA_C")
    shinyjs::reset("ELEFAN_GA_ts")

    shinyjs::disable("go_ga")
    clearResults("box_elefan_ga_results")
    shinyjs::reset("elefanGaDateFormat")
}
