tabElefanGa <- function(id) {
    ns <- NS(id)
    tabItem("ElefanGaWidget",

            htmlOutput(ns("elefanGaTitle")),

            htmlOutput("tropFishRLibVersion1", class="subTitle"),

            fluidRow(
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    actionButton(ns("elefanGADataConsiderations"), "Data Considerations",
                                 class="topLevelInformationButton")
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("methodConsiderations"), "Method Considerations",
                                 class="topLevelInformationButton")
                    )
            ),


            fluidRow(

                ## Information tabs
                ## -------------------------------
                bsModal("modalExampleGA", "Data Considerations",
                        ns("elefanGADataConsiderations"),
                        size = "large",
                        htmlOutput(ns("elefanGADataConsiderationsText"))),

                bsModal("modalMethod", "Method Considerations",
                        ns("methodConsiderations"),
                        size = "large",
                        htmlOutput(ns("methodConsiderationsText"))),

                bsModal("info_yearsel", "Selected years", ns("infoYearSel"),
                        size = "large",
                        "Select all or a range of years covered by uploaded data set."),

                bsModal("info_agg", "Data aggregation", ns("infoAGG"),
                        size = "large",
                        "Choose the temporal resolution of aggregated length data. Options are month, quarter, and year. Quarter and year are more meaningful for slow-growing species and if uploaded data covers at least one year or several years, respectively."),

                bsModal("info_binsize", "Bin size", ns("infoBS"),
                        size = "large",
                        "Bin size corresponds to the length interval over which the length frequency data are aggregated, for example 2cm."),

                bsModal("info_ma", "Moving average (MA)", ns("infoMA"),
                        size = "large",
                        p("Number indicating over how many length classes the moving average should be performed  (", withMathJax("\\(MA\\)"), ") (must be an odd number).")),

                bsModal("info_pg", "Plus group", ns("infoPG"),
                        size = "large",
                        "Plus group: the largest length class with only a few individuals after which the method will pool together (important for cohort analysis later)."),

                bsModal("info_at", "Additional squareroot transformation", ns("infoAT"),
                        size = "large",
                        "Additional squareroot transformation? of positive values according to Brey et al. (1988), reduces the weighting of large individuals."),

                bsModal("info_linf", withMathJax("\\(L_\\infty\\)"), ns("infolinf"),
                        size = "large",
                        p("Asymptotic length/length infinity of the von Bertalanffy growth function (",withMathJax("\\(L_\\infty\\)"), "in cm).")),

                bsModal("info_k", withMathJax("\\(K\\)"), ns("infok"),
                        size = "large",
                        p("The growth coefficient (", withMathJax("\\(K\\)"), ") of the von Bertalanffy growth function.")),

                bsModal("info_tanchor", withMathJax("\\(t_{anchor}\\)"), ns("infotanchor"),
                        size = "large",
                        p("Time point anchoring the growth curves in the year-length coordinate system, corresponds to the peak spawning month. The fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year (", withMathJax("\\(t_{anchor}\\)"), ")")),

                bsModal("info_seasonal", "Seasonal model", ns("infoSeason"),
                        size = "large",
                        p("Should the seasonal model be used? The seasonal model (or seasonalised von Bertalanffy growth curve) allows to calculate the seasonal growth parameters, ",
                          withMathJax("\\(C\\)"), " and ",
                          withMathJax("\\(t_{s}\\)"))),

                bsModal("info_C", withMathJax("\\(C\\)"), ns("infoC"),
                        size = "large",
                        p("Amplitude of growth oscillation (", withMathJax("\\(C\\)"), "): The higher the value of C the more pronounced are the seasonal oscillations. C = 0 implies that there is no seasonality in the growth rate;  if C = 1, the growth rate becomes zero at the winter point.")),

                bsModal("info_ts", withMathJax("\\(t_\\s\\)"), ns("infots"),
                        size = "large",
                        p("Summer point (", withMathJax("\\(t_{s}\\)"), "). Values between 0 and 1. At the time of the year when the fraction, " , withMathJax("\\(t_{s}\\)"),", has elapsed, the growth rate is the highest.")),


                bsModal("info_popsize", "Population size", ns("infoPopSize"),
                        size = "large",
                        "Size of inital population for genetic algorithm. The larger the better, but at the expense of computation speed."),

                bsModal("info_maxiter", "Maximum number of iterations", ns("infoMaxIter"),
                        size = "large",
                        "Maximum number of iterations to run before the GA search is halted (note this affects the run time)."),

                bsModal("info_MaxRuns", "Maximum number of runs", ns("infoMaxRuns"),
                        size = "large",
                        p("Number of consecutive generations without any improvement in the best fitness value before the GA is stopped (", withMathJax("\\(maxiter\\)"), ") (note that this affects the run time).")),

                bsModal("info_pmut", "Probability of mutation", ns("infoPmut"),
                        size = "large",
                        "Probability of mutation in a parent chromosome. Usually mutation occurs with a small probability."),

                bsModal("info_pcross", "Probability of crossover", ns("infoPcross"),
                        size = "large",
                        "Probability of crossover between pairs of chromosomes. Typically this is a large value."),

                bsModal("info_elite", "Degree of elitism", ns("infoElite"),
                        size = "large",
                        "Number of best fitness individuals to survive at each generation."),

                ## bsModal("info_XX", "XX", ns("infoXX"),
                ##         size = "large",),



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
                    )
                    ),



                ## Input - Settings
                ## -------------------------------
                br(),

                box(id = "box_settings",
                    title = "Settings",
                    width = NULL,
                    collapsible = T,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = TRUE,

                tabBox(
                    title = "Settings",
                    width = NULL,
                    height = "600px",
                    side="left",
                    selected = "Length data",
                                        # The id lets us use input$tabset1 on the server to find the current tab
                    id = "settings",

                    tabPanel("Length data",

                             box(title = "Data aggregation",

                                 fluidRow(
                                     div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                         HTML("<b>Select years for analysis</b>")
                                         ),
                                     div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                         actionButton(ns("infoYearSel"),
                                                      tags$i(class = "fas fa-info",
                                                             style="font-size: 12px"),
                                                      ## size = "extra-small",
##                                                      style='padding:1px; font-size:70%',
                                                      class="topLevelInformationButton")
                                         )

                                 ),
                                 div(style = "margin-top:-3px",
                                     uiOutput(ns("ELEFAN_years_selected_out"))
                                     ),



                                 selectInput(ns("ELEFAN_agg"),
                                              p("Aggregate data by",
                                                actionButton(ns("infoAGG"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                             choices = c("Choose one"="",
                                                         c("month","quarter","year")),
                                             selected = "month",
                                             width ='30%'),

                                 fluidRow(
                                     div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                         HTML("<b>Bin Size</b>")
                                         ),
                                     div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                         actionButton(ns("infoBS"),
                                                      tags$i(class = "fas fa-info",
                                                             style="font-size: 12px"),
                                                      ## size = "extra-small",
##                                                      style='padding:1px; font-size:70%',
                                                      class="topLevelInformationButton")
                                         )

                                 ),
                                 div(style = "margin-top:-3px",
                                     uiOutput(ns("ELEFAN_binSize_out")),
                                     ),



                                 numericInput(ns("ELEFAN_GA_PLUS_GROUP"),
                                              p("Plus group",
                                                actionButton(ns("infoPG"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                              0, min = 0, max = 100000, step=1,
                                              width ='30%')
                                 ),

                             box(title = "Data restructuring",

                                 numericInput(ns("ELEFAN_GA_MA"),
                                              p("Moving Average (MA)",
                                                actionButton(ns("infoMA"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                              5, min = 3, max = 101, step=2,
                                              width ='30%'),

                                 br(),

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

                             box(title = "Search space for growth parameters",
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
                                                 p(withMathJax("\\(K\\)"),
                                                   actionButton(ns("infok"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),
                                                 value=c(0.01,1), min = 0, max = 10, step=0.01),
                                     sliderInput(ns("ELEFAN_GA_t_anchor"),
                                                 p(withMathJax("\\(t_{anchor}\\)"),
                                                   actionButton(ns("infotanchor"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),
                                                 0, min = 0, max = 1, step=0.01)
                                 ),
                                 box(
                                     checkboxInput(ns("ELEFAN_GA_seasonalised"),
                                                   p("Seasonal model?",
                                                     actionButton(ns("infoSeaonal"),
                                                                  tags$i(class = "fas fa-info",
                                                                         style="font-size: 12px"),
                                                                  class="topLevelInformationButton")),
                                                   FALSE)
                                 ),
                                 box(id="box_elefan_ga_seasonPar",
                                     sliderInput(ns("ELEFAN_GA_C"),
                                                 p(withMathJax("\\(C\\)"),
                                                   actionButton(ns("infoC"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),
                                                 value=c(0,1), min = 0, max = 1, step=0.1),
                                     sliderInput(ns("ELEFAN_GA_ts"),
                                                 p(withMathJax("\\(t_{s}\\)"),
                                                   actionButton(ns("infots"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),
                                                 value=c(0,1), min = 0, max = 1, step=0.1)
                                     )
                                 )
                             ),

                    tabPanel("ELEFAN optimisation",

                             box(title = "Settings for ELEFAN optimisation procedure",
                                 width = 12,
                                 box(
                                     numericInput(ns("ELEFAN_GA_popSize"),
                                                  p("Population size:",
                                                    actionButton(ns("infoPopSize"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  50, min = 0, max = 10000, step=1),
                                     numericInput(ns("ELEFAN_GA_maxiter"),
                                                  p("Maximum number of generations",
                                                    actionButton(ns("infoMaxIter"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  10, min = 1, max = 1000, step=1),
                                     numericInput(ns("ELEFAN_GA_run"),
                                                  p("Number of generations without improvment",
                                                    actionButton(ns("infoMaxRuns"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  100, min = 1, max = 1000, step=1)
                                 ),
                                 box(
                                     numericInput(ns("ELEFAN_GA_pmutation"),
                                                  p("Probability of mutation",
                                                    actionButton(ns("infoPmut"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  0.1, min = 0.1, max = 1, step=0.1),
                                     numericInput(ns("ELEFAN_GA_pcrossover"),
                                                  p("Probability of crossover",
                                                    actionButton(ns("infoPcross"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  0.8, min = 0.1, max = 1, step=0.1),
                                     numericInput(ns("ELEFAN_GA_elitism"),
                                                  p("Degree of elitism",
                                                    actionButton(ns("infoElite"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  5, min = 0, max = 100, step=1)
                                 ))
                             ),

                    tabPanel("Natural mortality formula",
                             box(
                                 selectInput(inputId = "natM",
                                             "Method:",
                                             choices = c("Then_growth",
                                                         "Pauly_Linf",
                                                         "Then_tmax"),
                                             selected = "Then_growth",
                                             width ='70%'),

                                 ## schooling correction in Pauly's formula?
                                 checkboxInput(inputId = "schooling",
                                               label = "Correction for schooling in fish?",
                                               value = FALSE)

                             ),
                             box(
                                 ## slider for temperature
                                 sliderInput(inputId = "temp",
                                             label = "Average ambient sea surface temperature:",
                                             min = 0,
                                             max = 40,
                                             step = 0.5,
                                             value = 20),


                                 ## tmax for Then_tmax
                                 numericInput(inputId = "tmax",
                                              label = "Maximum age",
                                              min = 0,
                                              max = 200,
                                              value = 10, width = '20%')
                             )
                             ),

                    tabPanel("Catch curve",
                             box(
                                 numericInput(ns("ELEFAN_GA_binSize2"),
                                              p("Bin size for catch curve",
                                                actionButton(ns("infoBS"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                              2, min = 0.5, max = 100, step=0.5)
                             )
                             ),

                    tabPanel("Yield per recruit model",
                             box(

                                 numericInput(ns("ELEFAN_GA_binSize3"),
                                              p("Bin size for catch curve",
                                                actionButton(ns("infoBS"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 12px"),
                                                             class="topLevelInformationButton")),
                                              2, min = 0.5, max = 100, step=0.5),

                                 wellPanel(
                                     fluidRow(
                                         ## a
                                         column(6,
                                                numericInput(inputId="LWa", label=" Constant  (a) ",
                                                             value = 0.001)),
                                         ## b
                                         column(6,
                                                numericInput(inputId="LWb", label="Exponent (b) ",
                                                             value = 3)))
                                 )

                             ),
                             box(
                                 fluidRow(
                                     ## L50
                                     column(6,
                                            uiOutput("l50")),
                                     ## wqs
                                     column(6,
                                            uiOutput("wqs"))),

                                 sliderInput(
                                     inputId = "fmChangeAbs",
                                     label = "Fishing mortality (absolute)",
                                     dragRange = TRUE,
                                     value = range(0, 10),
                                     min = 0, max = 20,
                                     step = 1

                                 ),
                                 uiOutput("lcChange"),

                                 wellPanel(
                                     fluidRow(
                                         ## length out of fm vector
                                         column(6,
                                                numericInput(
                                                    inputId = "fmLengthOut",
                                                    label = "Fishing mortality",
                                                    value = 50,
                                                    min = 0
                                                )),
                                         ## length out of lc vector
                                         column(6, numericInput(
                                                       inputId = "lcLengthOut",
                                                       label = "Selectivity (L50)",
                                                       value = 50,
                                                       min = 0
                                                   )))
                                 ),

                                 "Yield and biomass is returned relative to following stock size in numbers.",
                                 br(),
                                 br(),
                                 ## stock size
                                 numericInput(
                                     inputId = "stockSize",
                                     label = "Stock size",
                                     value = 1,
                                     min = 1,
                                     width = '30%'
                                 )
                             )
                             )
                    )
                ),




                ## Action buttons
                ## -------------------------------
                br(),

                box(title = "Analysis",
                    width = NULL,
                    collapsible = F,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = F,

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

                    br()

                    ),





                ## Results
                ## -------------------------------
                br(),
                box(id = "box_results",
                    title = "Results",
                    width = NULL,
                    collapsible = T,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = T,

                    br(),
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

                    br(),
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
                    br(),
                    box( width= NULL, id = "box_mort_results",
                        collapsible = F,
                        class = "collapsed-box",
                        collapsed = F,
                        title = "Mortality rates",
                        tags$style(type="text/css",
                                   ".recalculating {opacity: 1.0;}")
                        ),
                    br(),
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
                br(),
                box(title = "Report",
                    id = "box_report",
                    width = NULL,
                    collapsible = F,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = F,
                    tags$style(type="text/css",
                               ".recalculating {opacity: 1.0;}"),
                    tags$div(
                             uiOutput(ns("downloadReport_ga")),
                             uiOutput(ns("ElefanGaVREUpload")),
                             style = "margin-left: 45%;"
                         ),

                    br()
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
