tabElefanGa <- function(id) {
    ns <- NS(id)
    tabItem("ElefanGaWidget",

            htmlOutput(ns("elefanGaTitle")),

            htmlOutput("tropFishRLibVersion1", class="subTitle"),

            fluidRow(
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    "More information about "
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    actionButton(ns("elefanGADataConsiderations"), "Data",
                                 class="topLevelInformationButton")
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("methodConsiderations"), "Methods",
                                 class="topLevelInformationButton")
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("resultConsiderations"), "Results",
                                 class="topLevelInformationButton")
                    )
            ),


            fluidRow(

                ## Information tabs
                ## -------------------------------
                bsModal("modalExampleGA", "Data Considerations - TropFishR",
                        ns("elefanGADataConsiderations"),
                        size = "large",
                        htmlOutput(ns("elefanGADataConsiderationsText"))),

                bsModal("modalExampleGA2", "Data Considerations - TropFishR",
                        ns("dataConsiderations2"),
                        size = "large",
                        htmlOutput(ns("elefanGADataConsiderationsText2"))),

                bsModal("modalMethod", "Methodological Considerations - TropFishR",
                        ns("methodConsiderations"),
                        size = "large",
                        htmlOutput(ns("methodConsiderationsText"))),

                bsModal("modalMethod2", "Methodological Considerations - TropFishR",
                        ns("methodConsiderations2"),
                        size = "large",
                        htmlOutput(ns("methodConsiderationsText2"))),

                bsModal("modalResults", "Results Considerations - TropFishR",
                        ns("resultConsiderations"),
                        size = "large",
                        htmlOutput(ns("resultConsiderationsText"))),

                bsModal("modalResults2", "Results Considerations - TropFishR",
                        ns("resultConsiderations2"),
                        size = "large",
                        htmlOutput(ns("resultConsiderationsText2"))),

                bsModal("info_yearsel", "Selected years", ns("infoYearSel"),
                        size = "large",
                        "Select all or a range of years covered by uploaded data set."),

                bsModal("info_agg", "Data aggregation", ns("infoAGG"),
                        size = "large",
                        "Define whether the aggregation of the dataset should be kept ('none'), or if the dataset should be (re-)aggregate by 'month', 'quarter', and 'year'. If 'month' is chosen, the data is assigned to the middle of respective sampling times (i.e. 15. day of each month). The options 'quarter' and 'year' can be helpful if the dataset spans several years as it decreases computation time."),

                bsModal("info_binsize", "Bin size", ns("infoBS"),
                        size = "large",
                        "Bin size corresponds to the length interval over which the length frequency data are aggregated, for example 2cm."),

                bsModal("info_ma", "Moving average (MA)", ns("infoMA"),
                        size = "large",
                        p("Number indicating over how many length classes the moving average should be performed  (", withMathJax("\\(MA\\)"), ") (must be an odd number).")),

                bsModal("info_pg", "Plus group", ns("infoPG"),
                        size = "large",
                        "Allows to lump together all catches larger than given size (so called 'plus group'). Note: This can greatly affect the estimation of ",withMathJax("\\(L_\\infty\\)")," with ELEFAN. Default '0' implies that no plus group is used."),

                bsModal("info_at", "Additional squareroot transformation", ns("infoAT"),
                        size = "large",
                        "Additional squareroot transformation according to Brey et al. (1988) reduces the weighting of large individuals."),

                bsModal("info_linf", withMathJax("\\(L_\\infty\\)"), ns("infolinf"),
                        size = "large",
                        p(withMathJax("\\(L_\\infty\\)")," defines the asymptotic length of the von Bertalanffy growth (VBG) function. The default range is dependent on uploaded dataset and defined as +/- 20% around the guesstimate of ",withMathJax("\\(L_\\infty = L_\\max/0.95\\)"),".")),

                bsModal("info_k", withMathJax("\\(K\\)"), ns("infok"),
                        size = "large",
                        p("The growth coefficient (", withMathJax("\\(K\\)"), ") of the von Bertalanffy growth function.")),

                bsModal("info_tanchor", withMathJax("\\(t_{anchor}\\)"), ns("infotanchor"),
                        size = "large",
                        p("Time point anchoring the growth curves in the year-length coordinate system, corresponds to the peak spawning month. The fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year (", withMathJax("\\(t_{anchor}\\)"), ")")),

                bsModal("info_season", "Seasonal model", ns("infoSeason"),
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
                        p("Number of consecutive generations without any improvement in the best fitness value before the GA is stopped (note that this affects the run time).")),

                bsModal("info_pmut", "Probability of mutation", ns("infoPmut"),
                        size = "large",
                        "Probability of mutation in a parent chromosome. Usually mutation occurs with a small probability and by default is set to 0.2."),

                bsModal("info_pcross", "Probability of crossover", ns("infoPcross"),
                        size = "large",
                        "Probability of crossover between pairs of chromosomes. Typically this is a large value and by default is set to 0.8."),

                bsModal("info_elite", "Degree of elitism", ns("infoElite"),
                        size = "large",
                        "Number of best fitness individuals to survive at each generation. By default the top 5% individuals will survive at each iteration."),


                ## bsModal("info_yearselcc", "Selected years", ns("infoYearSelCC"),
                ##         size = "large",
                ##         "Select all or a range of years covered by uploaded data set for the estimation of total and fishing mortality using the catch curve. If multiple years are selected the total and fishing mortality rates correspond to the average rates of selected years."),

                bsModal("info_pred", "Prediction range", ns("infoPred"),
                        size = "large",
                        "F range and Length at 50% selectivity range"),

                bsModal("info_lengthweight", "Length-weight relationship", ns("infoLengthWeight"),
                        size = "large",
                        "Test"
                        ),

                bsModal("info_adjdata", "Adjust data (stock status)", ns("infoAdjData"),
                        size = "large",
                        "Test"),

                bsModal("info_mat", "Maturity", ns("infoMat"),
                        size = "large",
                        "Maturity"
                        ),

                bsModal("info_select", "Gear selectivity", ns("infoSelect"),
                        size = "large",
                        HTML("<p>The specifics of how fish are caught by fisheries and thus the probability of capture for fish of various length classes are dependent on the fishing gear, which is referred to as gear selectivity. Find more information about fishing gear for example <a href=http://www.fao.org/3/X7788E/X7788E00.htm> here </a>. <br> TropFishR allows to estimate the gear selectivity by means of the catch curve ('Estimate', default).  Alternatively, the gear selectivity can be specified by the selectivity at 50% and 75% selectivity (L50 and L75, respectively) or by the length at 50% selectivity (L50) and the width of selection ogive (L75-L25). <br> <br> Note that estimated and specified selectivity corresponds to a logistic curve (trawl-like selectivity).</p>")
                        ),

                bsModal("info_natm", "Natural mortality", ns("infoNatM"),
                        size = "large",
                        "Test"
                        ),

                bsModal("info_assessment", "Check & Assessment & Reset & Report", ns("infoAssessment"),
                        size = "large",
                        HTML("<p>It is recommended to run a quick check by pressing <b>'Run Check'</b> before running the main assessment. While the main assessment can take up to a few minutes dependent on the settings of the ELEFAN optimation routine and the sample size of the dataset, the check is performed in a matter of a few seconds and can already identify issues regarding the data or settings. The check does not produce and results (figures or tables), but a notification in the lower right corner of the screen will inform you whether the check was successful. <br> <br> <b>'Run Assessment'</b> performs the main assessment and should yield plenty figures and tables in the result section upon successful completion. A progress bar in the lower right corner will inform you about the computation progress of ELEFAN first and then YPR. <br> <br> <b>'Reset'</b> removes all results, the uploaded dataset, and resets all settings to default values. <br> <br> After successful completion of the main assessment, an additional button <b>'Download Report'</b> allows you to download a pdf document with all results.</p>"
                        )),



                ## Input - Data upload
                ## -------------------------------

                box(id = "box_datupload",
                    title = p("Data Upload",
                              actionButton(ns("dataConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 12px"),
                                           class="topLevelInformationButton")),
                    width = NULL,
                    collapsible = FALSE,
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
                    title = p("Assessment Settings",
                              actionButton(ns("methodConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 12px"),
                                           class="topLevelInformationButton")),
                    width = NULL,
                    collapsible = FALSE, ## careful: if made collapsible the renderUi does not update! see: https://github.com/rstudio/shinydashboard/issues/234
                    solidHeader = TRUE,
                    class = "collapsed-box",

                    tabBox(
                        title = "",
                        width = NULL,
                        height = "710px",
                        side="left",
                        selected = "1. Data",
                        id = "settings",


                        tabPanel("1. Data",

                                 box(width = 3,

                                     fluidRow(
                                         div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                             HTML("<b>Select years for analysis</b>")
                                             ),
                                         div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                             actionButton(ns("infoYearSel"),
                                                          tags$i(class = "fas fa-info",
                                                                 style="font-size: 12px"),
                                                          class="topLevelInformationButton")
                                             )

                                     ),
                                     div(style = "margin-top:-3px",
                                         uiOutput(ns("ELEFAN_years_selected_out"))
                                         ),

                                     br(),



                                     selectInput(ns("ELEFAN_agg"),
                                                 p("Aggregate data by",
                                                   actionButton(ns("infoAGG"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),
                                                 choices = c("Choose one"="",
                                                             c("none","month","quarter","year")),
                                                 selected = "none",
                                                 width ='100%'),
                                     br(),

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
                                     br(),

                                     ## numericInput(ns("ELEFAN_GA_PLUS_GROUP"),
                                     ##              p("Plus group",
                                     ##                actionButton(ns("infoPG"),
                                     ##                             tags$i(class = "fas fa-info",
                                     ##                                    style="font-size: 12px"),
                                     ##                             class="topLevelInformationButton")),
                                     ##              0, min = 0, max = 100000, step=1,
                                     ##              width ='50%'),

                                     numericInput(ns("ELEFAN_GA_MA"),
                                                  p("Moving Average (MA)",
                                                    actionButton(ns("infoMA"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  5, min = 3, max = 101, step=2,
                                                  width ='100%'),

                                     br(),

                                     checkboxInput(ns("ELEFAN_GA_addlsqrt"),
                                                   p("Additional squareroot transformation?",
                                                     actionButton(ns("infoAT"),
                                                                  tags$i(class = "fas fa-info",
                                                                         style="font-size: 12px"),
                                                                  class="topLevelInformationButton")),
                                                   FALSE)
                                     ),
                                 box(id = "box_exploPlots",
                                     width = 9,
                                     tags$div(
                                              plotOutput(ns("plot_explo1"), width = "90%",
                                                         height = "600px"),
                                              div(style = "margin-top:-10px; margin-left: 10px",
                                                  htmlOutput(ns("title_explo1"))
                                                  ),
                                              ## plotOutput(ns("plot_explo2"), width = "90%",
                                              ##            height = "280px"),
                                              ## div(style = "margin-top:-10px; margin-left: 10px",
                                              ##     htmlOutput(ns("title_explo2"))
                                              ##     ),
                                              style = "margin-left: 10%;"
                                          )
                                     )
                                 ),



                        tabPanel("2. ELEFAN",
                                 box(title = "Search space for growth parameters",
                                     width = 9,
                                     box(width=6,
                                         fluidRow(
                                             div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                 HTML(paste0("<b> Asymptotic length (",withMathJax("\\(L_\\infty\\)"),") </b>"))
                                                 ),
                                             div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                                 actionButton(ns("infolinf"),
                                                              tags$i(class = "fas fa-info",
                                                                     style="font-size: 12px"),
                                                              class="topLevelInformationButton")
                                                 )

                                         ),
                                         div(style = "margin-top:-3px",
                                             uiOutput(ns("ELEFAN_GA_Linf_out")),
                                             ),
                                         br(),
                                         sliderInput(ns("ELEFAN_GA_K"),
                                                     p(HTML(paste0("Growth rate (",withMathJax("\\(K\\)"),")")),
                                                       actionButton(ns("infok"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 12px"),
                                                                    class="topLevelInformationButton")),
                                                     value=c(0.05,1), min = 0, max = 10, step=0.01),
                                         br(),
                                         sliderInput(ns("ELEFAN_GA_t_anchor"),
                                                     p(HTML(paste0("Time anchor (",withMathJax("\\(t_{a}\\)"),")")),
                                                       actionButton(ns("infotanchor"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 12px"),
                                                                    class="topLevelInformationButton")),
                                                     value=c(0,1), min = 0, max = 1, step=0.01),
                                         ),
                                     box(width=6,
                                         checkboxInput(ns("ELEFAN_GA_seasonalised"),
                                                       p("Seasonal model?",
                                                         actionButton(ns("infoSeason"),
                                                                      tags$i(class = "fas fa-info",
                                                                             style="font-size: 12px"),
                                                                      class="topLevelInformationButton")),
                                                       FALSE)
                                         ),
                                     br(),
                                     box(id="box_elefan_ga_seasonPar",
                                         width = 6,
                                         sliderInput(ns("ELEFAN_GA_C"),
                                                     p(HTML(paste0("Amplitude (",withMathJax("\\(C\\)"),")")),
                                                       actionButton(ns("infoC"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 12px"),
                                                                    class="topLevelInformationButton")),
                                                     value=c(0,1), min = 0, max = 1, step=0.01),
                                         br(),
                                         sliderInput(ns("ELEFAN_GA_ts"),
                                                     p(HTML(paste0("Summer point (",withMathJax("\\(t_{s}\\)"),")")),
                                                       actionButton(ns("infots"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 12px"),
                                                                    class="topLevelInformationButton")),
                                                     value=c(0,1), min = 0, max = 1, step=0.01)
                                         )
                                     ),

                                 box(title = "ELEFAN's genetic algorithm:",
                                     width = 3,
                                     numericInput(ns("ELEFAN_GA_popSize"),
                                                  p("Population size:",
                                                    actionButton(ns("infoPopSize"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  100, min = 50, max = 1e3, step=1,
                                                  width = "90%"),
                                     numericInput(ns("ELEFAN_GA_maxiter"),
                                                  p("Maximum number of generations",
                                                    actionButton(ns("infoMaxIter"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  50, min = 20, max = 1e3, step=1,
                                                  width = "90%"),
                                     numericInput(ns("ELEFAN_GA_run"),
                                                  p("Number of generations without improvment",
                                                    actionButton(ns("infoMaxRuns"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  20, min = 10, max = 1e3, step=1,
                                                  width = "90%"),

                                     numericInput(ns("ELEFAN_GA_pmutation"),
                                                  p("Probability of mutation",
                                                    actionButton(ns("infoPmut"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  0.2, min = 0.1, max = 1, step=0.1,
                                                  width = "90%"),

                                     numericInput(ns("ELEFAN_GA_pcrossover"),
                                                  p("Probability of crossover",
                                                    actionButton(ns("infoPcross"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  0.8, min = 0.1, max = 1, step=0.1,
                                                  width = "90%"),

                                     numericInput(ns("ELEFAN_GA_elitism"),
                                                  p("Degree of elitism",
                                                    actionButton(ns("infoElite"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 12px"),
                                                                 class="topLevelInformationButton")),
                                                  5, min = 1, max = 1e2, step=1,
                                                  width = "90%")
                                     )
                                 ),

                        tabPanel("3. Stock status",

                                 fluidRow(

                                     box(
                                         title = p(HTML(paste0("Length-weight relationship (",
                                                               withMathJax("\\(W = a \ L \ e^{b}\\)"),")")),
                                                   actionButton(ns("infoLengthWeight"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),

                                         br(),
                                         width = 4,
                                         height = "200px",
                                         fluidRow(
                                             column(6,
                                                    numericInput(ns("LWa"),
                                                                 label=" Constant  (a) ",
                                                                 min = 0.0001,
                                                                 max = 10,
                                                                 value = 0.01,
                                                                 step = 0.01,
                                                                 width = "60%")),
                                             column(6,
                                                    numericInput(ns("LWb"),
                                                                 label="Exponent (b) ",
                                                                 min = 0.0001,
                                                                 max = 10,
                                                                 value = 3,
                                                                 step = 0.1,
                                                                 width = "60%"))
                                         )
                                     ),

                                     box(title = p("Natural mortality",
                                                   actionButton(ns("infoNatM"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),
                                         width = 4,
                                         height = "200px",
                                         br(),
                                         fluidRow(
                                             column(6,
                                                    selectInput(ns("natM"),
                                                                "Method:",
                                                                choices = c("Then's growth formula",
                                                                            "Pauly's growth & temp. formula",
                                                                            "Then's max. age formula"),
                                                                selected = "Then's growth formula",
                                                                width ='100%')
                                                    ),
                                             column(6,
                                                    div(id ="ui_natM_pauly",
                                                        fluidRow(
                                                            column(7,
                                                                   numericInput(ns("temp"),
                                                                                label = "Average ambient sea surface temperature",
                                                                                min = 0,
                                                                                step = 0.5,
                                                                                value = 20)
                                                                   ),
                                                            column(5,
                                                                   div(style="margin-top:15px;",
                                                                       checkboxInput(ns("schooling"),
                                                                                     label = "Correction for schooling?",
                                                                                     value = FALSE)
                                                                       )
                                                                   )
                                                        )
                                                        ),
                                                    div(id ="ui_natM_then_tmax",
                                                        ## tmax for Then_tmax
                                                        numericInput(ns("tmax"),
                                                                     label = "Maximum age",
                                                                     min = 0,
                                                                     max = 200,
                                                                     value = 20,
                                                                     step = 1,
                                                                     width = '30%'),
                                                        )
                                                    )
                                         )
                                         ),

                                     box(title = p("Adjust length data",
                                                   actionButton(ns("infoAdjData"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),
                                         width = 4,
                                         height = "200px",
                                         br(),
                                         fluidRow(
                                             column(6,
                                                    fluidRow(
                                                        div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                            HTML("<b>Bin Size (stock status)</b>")
                                                            ),
                                                        ),
                                                    div(style = "margin-top:-3px",
                                                        uiOutput(ns("ELEFAN_binSize2_out")),
                                                        ),

                                                    ),
                                             column(6,
                                                    fluidRow(
                                                        div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                            HTML("<b>Select years (stock status)</b>")
                                                            ),
                                                        ),
                                                    div(style = "margin-top:-3px",
                                                        uiOutput(ns("ELEFAN_years_selected_cc_out"))
                                                        )
                                                    )
                                         ),
                                         br(), br()
                                         )

                                 ),

                                 fluidRow(
                                     box(title = p("Maturity",
                                                   actionButton(ns("infoMat"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),
                                         width = 4,
                                         height = "200px",
                                         br(),
                                         fluidRow(
                                             column(6,
                                                    numericInput(ns("Lm50"),
                                                                 label="Lm50",
                                                                 value = 0,
                                                                 step = 1,
                                                                 width = "60%")),
                                             column(6,
                                                    numericInput(ns("Lm75"),
                                                                 label="Lm75",
                                                                 value = 0,
                                                                 step = 1,
                                                                 width = "60%"))
                                         )
                                         ),

                                     box(title = p("Selectivity",
                                                   actionButton(ns("infoSelect"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 12px"),
                                                                class="topLevelInformationButton")),
                                         width=8,
                                         height = "200px",
                                         br(),
                                         fluidRow(
                                             column(4,
                                                    selectInput(ns("select"),
                                                                "Selectivity",
                                                                choices = c("Estimate",
                                                                            "Define L50 & L75",
                                                                            "Define L50 & (L75-L25)"),
                                                                selected = "Estimate",
                                                                width = "80%")
                                                    ),
                                             column(4,
                                                    div(
                                                        id ="ui_l50",
                                                        numericInput(ns("l50_user"),
                                                                     p(withMathJax("\\(L_{50}\\)")," (user defined)",
                                                                       actionButton(ns("infoL50"),
                                                                                    tags$i(class = "fas fa-info",
                                                                                           style="font-size: 12px"),
                                                                                    class="topLevelInformationButton")),
                                                                     value = 0, min = 0, step = 1,
                                                                     width = "80%")
                                                    )
                                                    ),
                                             column(4,
                                                    div(
                                                        id ="ui_l75",
                                                        numericInput(ns("l75_user"),
                                                                     p(withMathJax("\\(L_{75}\\)")," (user defined)",
                                                                       actionButton(ns("infoL75"),
                                                                                    tags$i(class = "fas fa-info",
                                                                                           style="font-size: 12px"),
                                                                                    class="topLevelInformationButton")),
                                                                     value = 0, min = 0, step = 1,
                                                                     width = "80%")
                                                    ),
                                                    div(
                                                        id ="ui_wqs",
                                                        numericInput(ns("wqs_user"),
                                                                     p("Width (",withMathJax("\\(L_{75}-L_{25}\\)"),"; user defined)",
                                                                       actionButton(ns("infoWQS"),
                                                                                    tags$i(class = "fas fa-info",
                                                                                           style="font-size: 12px"),
                                                                                    class="topLevelInformationButton")),
                                                                     value = 0, min = 0, step = 1,
                                                                     width = "80%")
                                                    )
                                                    )
                                         ),
                                         br(), br()
                                         )
                                 ),

                                 box(title = p("Prediction range",
                                               actionButton(ns("infoPred"),
                                                            tags$i(class = "fas fa-info",
                                                                   style="font-size: 12px"),
                                                            class="topLevelInformationButton")),
                                     width=12,
                                     height = "200px",
                                     br(),
                                     fluidRow(
                                         column(1),
                                         column(1,
                                                div(style = "margin-top:30px;",
                                                    "Fishing mortality")
                                                ),
                                         column(1,
                                                numericInput(ns("fRangeSteps"),
                                                             label = "Steps",
                                                             value = 100,
                                                             min = 0,
                                                             step = 1,
                                                             width = "100%"
                                                             )
                                                ),
                                         column(1,
                                                numericInput(ns("fRangeMin"),
                                                             label = "Min",
                                                             value = 0,
                                                             min = 0,
                                                             step = 0.1,
                                                             width = "100%"
                                                             )
                                                ),
                                         column(1,
                                                numericInput(ns("fRangeMax"),
                                                             label = "Max",
                                                             value = 3,
                                                             min = 0,
                                                             step = 0.1,
                                                             width = "100%"
                                                             )
                                                ),
                                         column(1),
                                         column(1),
                                         column(1,
                                                div(style = "margin-top:20px;",
                                                    "Length at 50% selectivity (L50)")
                                                ),
                                         column(1,
                                                numericInput(ns("lcRangeSteps"),
                                                             label = "Steps",
                                                             value = 100,
                                                             min = 0,
                                                             step = 1,
                                                             width = "100%"
                                                             )
                                                ),
                                         column(1,
                                                div(
                                                    id ="ui_lcMin",
                                                    numericInput(ns("lcRangeMin"),
                                                                 label = "Min",
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 1,
                                                                 width = "100%"
                                                                 )
                                                )
                                                ),
                                         column(1,
                                                div(
                                                    id ="ui_lcMax",
                                                    numericInput(ns("lcRangeMax"),
                                                                 label = "Max",
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 1,
                                                                 width = "100%"
                                                                 )
                                                )
                                                ),
                                         column(1)
                                     )
                                     )
                                 )

                    )
                    ),



                ## Action buttons
                ## -------------------------------
                br(),

                box(title = p("Run Assessment & Download Report",
                              actionButton(ns("infoAssessment"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 12px"),
                                           class="topLevelInformationButton")),
                    width = NULL,
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = FALSe,

                    br(),

                    fluidRow(
                        div(style = "display: inline-block; vertical-align:center; margin-left: 50px;",
                            disabled(actionButton(ns("check_ga"),
                                                  "Run Check",
                                                  class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(actionButton(ns("go_ga"),
                                                  "Run Assessment",
                                                  class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            actionButton(ns("reset_ga"),
                                         "Reset",
                                         class="topLevelInformationButton")
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            uiOutput(ns("downloadReport_ga"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            uiOutput(ns("ElefanGaVREUpload"))
                            ),
                        ),
                    br(),br()
                    ),

                br(),


                ## Results
                ## -------------------------------
                br(),
                box(id = "box_results",
                    title = p("Assessment Results",
                              actionButton(ns("resultConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 12px"),
                                           class="topLevelInformationButton")),
                    width = NULL,
                    height = "2200px",
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = FALSE,

                    fluidRow (
                        column(
                            7,
                            tags$div(
                                     plotOutput(ns("plot_growthCurves"), width = "90%",
                                                height = "600px"),
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_growthCurves"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        ),
                        column(
                            5,
                            br(),
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 3px",
                                         htmlOutput(ns("title_table_growth"))
                                         ),
                                     tableOutput(ns("table_growth")),
                                     style = "margin-left: 20%;"
                                 ),
                            br(),
                            tags$div(
                                     plotOutput(ns("plot_elefanFit"), width = "80%",
                                                height = "400px"),
                                     div(style = "margin-top:0px; margin-left: 3px",
                                         htmlOutput(ns("title_elefanFit"))
                                         ),
                                     style = "margin-left: 0%;"
                                 )
                        )

                    ),

                    br(),br(),

                    fluidRow (
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_catchCurve"), width = "80%",
                                                height = "400px"),
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_catchCurve"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        ),
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_select"), width = "80%",
                                                height = "400px"),
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_select"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        )

                    ),

                    br(), br(),

                    fluidRow(
                        column(
                            4,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 7px",
                                         htmlOutput(ns("title_table_mort"))
                                         ),
                                     tableOutput(ns("table_mort")),
                                     style = "margin-left: 5%;"
                                 )
                        ),
                        column(
                            4,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 2px",
                                         htmlOutput(ns("title_table_refs"))
                                         ),
                                     tableOutput(ns("table_refs")),
                                     style = "margin-left: 0%;"
                                 )
                        ),
                        column(
                            4,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 2px",
                                         htmlOutput(ns("title_table_stockstatus"))
                                         ),
                                     tableOutput(ns("table_stockstatus")),
                                     style = "margin-left: 0%;"
                                 )
                        )
                    ),

                    br(),br(),

                    fluidRow(
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_ypr"), width = "90%",
                                                height = "700px"),
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_ypr"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        ),
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_ypr_iso"), width = "90%",
                                                height = "700px"),
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_ypr_iso"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        )
                    )


                    ##         "Results of the Thompson and Bell model: Curves of yield and biomass per recruit. The black dot represents yield and biomass under current fishing pressure. The yellow and red dashed lines represent fishing mortality for maximum sustainable yield (Fmsy) and fishing mortality to fish the stock at 50% of the virgin biomass (F0.5).",

                    ##         "Exploration of impact of different exploitation rates and Lc values on the relative yield per recruit.",


                    )
            )
            )


}
