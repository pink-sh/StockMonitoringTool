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
                    actionButton(ns("elefanGAWorkflowConsiderations"), "Workflow",
                                 class="topLevelInformationButton")
                ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
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

                ## Notifications in middle of screen (if decided to keep -> move to custom.css?)
                tags$head(tags$style(HTML(".shiny-notification { position:fixed; top: calc(50%); left: calc(50%); width: 250px; height: 80px;}"))),

                ## Information tabs
                ## -------------------------------
                bsModal("modalWorkflow", "Workflow Considerations - TropFishR",
                        ns("elefanGAWorkflowConsiderations"),
                        size = "large",
                        htmlOutput(ns("elefanGAWorkflowConsiderationsText"))),

                bsModal("modalExampleGA", "Data Loading and Formatting Considerations - TropFishR",
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
                        HTML("<p>Select all or a range of years in the uploaded data set to be included in the analysis. <br><br> In theory, the longer the period covered by the data set, the better. However, data sets covering a long period with monthly aggregated data, might lead to a long run time and make the assumption that the growth parameters did not change over this period. In this case, you could consider to choose only the most recent years.</p>")),

                bsModal("info_agg", "Data aggregation", ns("infoAGG"),
                        size = "large",
                        HTML("<p>Define whether the aggregation of the dataset should be kept ('none', default), or if the dataset should be (re-)aggregate by 'month', 'quarter', and 'year'. <br><br> Note that if 'month' is chosen, the data is assigned to the middle of respective sampling times (i.e. 15. day of each month). <br><br> In theory, the longer the period covered by the data set, the better. However, data sets covering a long period with monthly aggregated data, might lead to a long run time and make the assumption that the growth parameters did not change over this period. Choosing only the most recent years or changing the aggregation 'quarter' and 'year' can be helpful to decrease computation time. <br><br> Note that a coarser aggregation reduces the information content of the data set. </p>")),

                bsModal("info_binsize", "Bin size", ns("infoBS"),
                        size = "large",
                        HTML("<p>Bin size corresponds to the length interval over which the length frequency data are aggregated, for example 2cm. <br><br> The combination of bin size and moving average (MA) critically affects the separation of peaks (i.e. potential cohorts) in the dataset and thus the estimation of growth parameters by ELEFAN. The bin size should be defined before defining the MA value. Ideally, the bin size is as small as possible, but large enough so that adjacent bins with high and low counts correspond to potential cohorts rather than noise.</p>")),

                bsModal("info_ma", "Moving average (MA)", ns("infoMA"),
                        size = "large",
                        HTML(paste0("<p>Number indicating over how many adjacent length classes the moving average should be performed  (", withMathJax("\\(MA\\)"), ") (must be a positive odd number, e.g. 5 or 7). <br><br>The combination of bin size and MA critically affects the separation of peaks (i.e. potential cohorts) in the dataset and thus the estimation of growth parameters by ELEFAN. Ideally, the MA value is defined after defining the optimal bin size and leads to visually distinct peaks in particular among small length classes. One option for the MA value is setting it equal to the number of length classes (bins) potentially corresponding to the youngest cohort.</p>"))),

                bsModal("info_pg", "Plus group", ns("infoPG"),
                        size = "large",
                        "Allows to lump together all catches larger than given size (so called 'plus group'). Note: This can greatly affect the estimation of ",withMathJax("\\(L_\\infty\\)")," with ELEFAN. Default '0' implies that no plus group is used."),

                bsModal("info_at", "Additional squareroot transformation", ns("infoAT"),
                        size = "large",
                        "Additional squareroot transformation according to Brey et al. (1988) reduces the weighting of large individuals."),

                bsModal("info_searchspace", "Search space for growth parameters", ns("info_searchSpace"),
                        size = "large",
                        HTML(paste0("<p>ELEFAN uses the genetic algorithm (GA) to find the set of growth parameters which fits best to the uploaded data set. In this tab, you can define the search space for each growth parameter. Note that the algorithm only searches within the defined parameter range. Thus, it is recommended to define the range rather wider than narrower. <br><br> By default, a reasonable range is defined for all parameters based on uploaded input data. </p>"))),

                bsModal("info_linf", withMathJax("\\(L_\\infty\\)"), ns("infolinf"),
                        size = "large",
                        p(withMathJax("\\(L_\\infty\\)")," defines the asymptotic length of the von Bertalanffy growth (VBG) function. The default range is dependent on uploaded dataset and defined as +/- 20% around the guesstimate of ",withMathJax("\\(L_\\infty = L_\\max/0.95\\)"),". Note that the maximum possible range is limited to +/- 50% of this estimate.")),

                bsModal("info_k", withMathJax("\\(K\\)"), ns("infok"),
                        size = "large",
                        HTML(paste0("<p>The growth coefficient (", withMathJax("\\(K\\)"), ") of the von Bertalanffy growth function determines the slope of the logistic growth curve: A low ",withMathJax("\\(K\\)")," defines a slow-growing species and a high K a fast growing species. <br><br>If no prior knowledge about this life-history parameter is available, it is recommended to define a wide search space from 0.01 to 2-4. If prior information is available, a narrrower range might be considered.</p>"))),

                bsModal("info_tanchor", withMathJax("\\(t_{a}\\)"), ns("infotanchor"),
                        size = "large",
                        p("Time point anchoring the growth curves in the year-length coordinate system, corresponds to the peak spawning month. The fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year (", withMathJax("\\(t_{a}\\)"), "). Values between 0 and 1.")),

                bsModal("info_season", "Seasonal model", ns("infoSeason"),
                        size = "large",
                        p("Should the seasonal model be used? The seasonal model (or seasonalised von Bertalanffy growth curve) allows to calculate the seasonal growth parameters, ",
                          withMathJax("\\(C\\)"), " and ",
                          withMathJax("\\(t_{s}\\)"),". Please see the Seasonal Von Bertalanffy page in the Supporting Tools menu for more information.")),

                bsModal("info_C", withMathJax("\\(C\\)"), ns("infoC"),
                        size = "large",
                        p("Amplitude of growth oscillation (", withMathJax("\\(C\\)"), "): The higher the value of C the more pronounced are the seasonal oscillations. C = 0 implies that there is no seasonality in the growth rate;  if C = 1, the growth rate becomes zero at the winter point. Values between 0 and 1.")),

                bsModal("info_ts", withMathJax("\\(t_\\s\\)"), ns("infots"),
                        size = "large",
                        p("Summer point (", withMathJax("\\(t_{s}\\)"), "). Values between 0 and 1. The time of the year when growth rate is highest, represented by the fraction of the calendar year, e.g. 0.25 corresponds to April 1st.")),

                bsModal("info_ga", "ELEFAN's genetic algorithm", ns("info_GA"),
                        size = "large",
                        HTML(paste0("<p>Genetic algorithms (GAs) are stochastic search algorithms inspired by the basic principles of biological evolution and natural selection.  GAs simulate the evolution of living organisms, where the fittest individuals dominate over the weaker ones, by mimicking the biological mechanisms of evolution, such as selection, crossover and mutation.<br><br> Changing default parameter can have a substantial effect on the optimization process and, thus, on estimated growth parameters. Therefore, please apply caution when changing these parameters. In fact, it should only be considered to increase the values which comes at the expense of computation time however. For example, it is recommended to the maximum number of generations and number of generations without improvement in the case that the fitness plot (Fig. 3) indicated that the best fitness value is not stable yet over 50 generations.</p>"))),

                bsModal("info_popsize", "Population size", ns("infoPopSize"),
                        size = "large",
                        "Size of inital population for genetic algorithm. Common values are 50 or 100. Higher values might require a larger number of generations to find a stable optimum."),

                bsModal("info_maxiter", "Maximum number of iterations", ns("infoMaxIter"),
                        size = "large",
                        "Maximum number of iterations to run before the GA search is halted. Note that this parameter might affect the run time significantly."),

                bsModal("info_MaxRuns", "Maximum number of runs", ns("infoMaxRuns"),
                        size = "large",
                        p("Number of consecutive generations without any improvement in the best fitness value before the GA is stopped. Note that this parameter might affect the run time significantly.")),

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
                        HTML("<p>The prediction range determines the fishing mortality rates and length at 50% selectivity (L50) values which are used in the yield per recruit model. The model estimates yield per recruit (YPR) and biomass per recruit (BPR) for each combination of fishing mortality and L50 value. Thus, the prediction ranges (F and L50) affect the axes of Figures 6 and 7. <br> The range for fishing mortality can be defined by the number of 'Steps' between the minimum ('Min') and maximum ('Max') mortality rate. <br> If the selectivity is estimated (default), only the number of 'Steps' can be changed for he L50 range. If the selectivity parameters are provided (e.g. L50 and L75), the minimum ('Min') and maximum ('Max') of the L50 range can be changed.</p>")),

                bsModal("info_lengthweight", "Length-weight relationship", ns("infoLengthWeight"),
                        size = "large",
                        HTML(paste0("<p>The estimation of the yield and biomass per recruit requires information about the average weight per length class, which can be estimated with the length-weight relationship. A common assumption is the allometric relationship ",withMathJax("\\(W = a L^{b}\\)"),", with the constant a in ",
                                    withMathJax("\\( g/cm^{b}\\)"), " and the unitless exponent ",
                                    withMathJax("\\( b\\)"),". <br>Ideally, the parameters are estimated based on length and weight measurements of the stock under study. Alternatively, information about the length-weight relationship of the species under study can be found on <a href='http://www.fishbase.org/search.php'> FishBase</a> or <a href='https://www.sealifebase.ca'> SeaLifeBase for invertebrates</a>. By default the parameters are set to ",
                                    withMathJax("\\(a = 0.01 g/cm^{3}\\)")," and ",
                                    withMathJax("\\(b = 3\\)"),".",
                                    "</p>"))
                        ),

                bsModal("info_adjdata", "Adjust data (stock status)", ns("infoAdjData"),
                        size = "large",
                        HTML(paste0("<p> Select the year(s) for the estimation of the stock status. The mortality rates estimated by the catch curve correspond to all years selected. If several years are selected, the samples for selected years are combined and the estimated rates correspond to the average values over all years selected. </p>"))),

                bsModal("info_mat", "Maturity (optional)", ns("infoMat"),
                        size = "large",
                        HTML("<p>If available, maturity information about your species in terms of the length at 50% and 75% maturity can be provided and allows to estimate current Spawning Potential Ratio (SPR) and SPR-related reference points. The parameterisation with Lm50 and Lm75 assumes a logistic maturity ogive. <br><br> Ideally, maturity information is collected directly from the stock under study e.g. by determining the maturation states of the gonades. <br>Alternatively, you might find maturity information about your species on <a href='http://www.fishbase.org/search.php'> FishBase</a> or <a href='https://www.sealifebase.ca'> SeaLifeBase for invertebrates</a>.</p>")
                        ),

                bsModal("info_select", "Gear selectivity", ns("infoSelect"),
                        size = "large",
                        HTML("<p>The specifics of how fish are caught by fisheries and thus the probability of capture for fish of various length classes are dependent on the fishing gear, which is referred to as gear selectivity. Find more information about fishing gear for example <a href=http://www.fao.org/3/X7788E/X7788E00.htm> here </a>. <br><br>TropFishR allows to estimate the gear selectivity by means of the catch curve ('Estimate', default).  Alternatively, the gear selectivity can be specified by the selectivity at 50% and 75% selectivity (L50 and L75, respectively) or by the length at 50% selectivity (L50) and the width of selection ogive (L75-L25). <br> <br> Note that estimated and specified selectivity corresponds to a logistic curve (trawl-like selectivity).</p>")
                        ),

                bsModal("info_natm", "Natural mortality", ns("infoNatM"),
                        size = "large",
                        HTML("<p>The natural mortality rate (M) is required to estimate the fishing mortality (F) from the total mortality (Z) estimated by the catch curve (F = Z - M). The natural mortality is estimated by an empirical formula based on estimated growth parameters. The options are: <br> - Then's growth formula (Then et al. 2015), <br> - Pauly's growth and temperature formula (Pauly et al. 1980), and <br> - Then's maximum age formula (Then et al. 2015); <br> While the first option does not require any additional information, the second requires the average annual sea surface temperature (SST) in grad Celsius and allows to correct for schooling fish (multiplication with 0.8) and third option requires an estimate of the maximum age of the fish.<br>Please see the Natural Mortality estimator page in the Supporting Tools menu for more information. </p>")
                        ),

                bsModal("info_assessment", "Check & Assessment & Reset & Report", ns("infoAssessment"),
                        size = "large",
                        HTML("<p>It is recommended to run a quick check by pressing <b>'Run Check'</b> before running the main assessment. While the main assessment can take up to a few minutes dependent on the settings of the ELEFAN optimation routine and the sample size of the dataset, the check is performed in a matter of a few seconds and can already identify issues regarding the data or settings. The check does not produce results (figures or tables), but a notification in the lower right corner of the screen will inform you whether the check was successful. <br> <br> <b>'Run Assessment'</b> performs the main assessment and should yield plenty of figures and tables in the result section upon successful completion. A progress bar in the lower right corner will inform you about the computation progress of ELEFAN first and then YPR. <br> <br> <b>'Reset'</b> removes all results, the uploaded dataset, and resets all settings to default values. <br> <br> After successful completion of the main assessment, an additional button <b>'Download Report'</b> allows you to download a pdf document with all results.</p>"
                             )),



                ## Input - Data upload
                ## -------------------------------

                box(id = "box_datupload",
                    title = p("Data Upload",
                              actionButton(ns("dataConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
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
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE, ## careful: if made collapsible the renderUi does not update! see: https://github.com/rstudio/shinydashboard/issues/234
                    solidHeader = TRUE,
                    class = "collapsed-box",

                    tabBox(
                        title = "",
                        width = NULL,
                        height = "730px",
                        side="left",
                        selected = "1. Data",
                        id = "settings",


                        tabPanel("1. Data",

                                 box(title = "Data settings",
                                     width = 3,

                                     fluidRow(
                                         div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                             HTML("<b>Select years for analysis</b>")
                                             ),
                                         div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                             actionButton(ns("infoYearSel"),
                                                          tags$i(class = "fas fa-info",
                                                                 style="font-size: 8px"),
                                                          class="infoBubbleButton")
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
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
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
                                                                 style="font-size: 8px"),
                                                          ## size = "extra-small",
                                                          ##                                                      style='padding:1px; font-size:70%',
                                                          class="infoBubbleButton")
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
                                                                        style="font-size: 8px"),
                                                                 class="infoBubbleButton")),
                                                  5, min = 3, max = 101, step=2,
                                                  width ='100%'),

                                     br(),

                                     checkboxInput(ns("ELEFAN_GA_addlsqrt"),
                                                   p("Additional squareroot transformation?",
                                                     actionButton(ns("infoAT"),
                                                                  tags$i(class = "fas fa-info",
                                                                         style="font-size: 8px"),
                                                                  class="infoBubbleButton")),
                                                   FALSE)
                                     ),
                                 box(title = "Data exploration",
                                     id = "box_exploPlots",
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
                                 box(title = p("Search space for growth parameters",
                              actionButton(ns("info_searchSpace"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                                     width = 9,
                                     box(width=6,
                                         fluidRow(
                                             div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                 HTML(paste0("<b> Asymptotic length (",withMathJax("\\(L_\\infty\\)"),") </b>"))
                                                 ),
                                             div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                                 actionButton(ns("infolinf"),
                                                              tags$i(class = "fas fa-info",
                                                                     style="font-size: 8px"),
                                                              class="infoBubbleButton")
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
                                                                           style="font-size: 8px"),
                                                                    class="infoBubbleButton")),
                                                     value=c(0.05,1), min = 0, max = 10, step=0.01),
                                         br(),
                                         sliderInput(ns("ELEFAN_GA_t_anchor"),
                                                     p(HTML(paste0("Time anchor (",withMathJax("\\(t_{a}\\)"),")")),
                                                       actionButton(ns("infotanchor"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 8px"),
                                                                    class="infoBubbleButton")),
                                                     value=c(0,1), min = 0, max = 1, step=0.01),
                                         ),
                                     box(width=6,
                                         checkboxInput(ns("ELEFAN_GA_seasonalised"),
                                                       p("Seasonal model?",
                                                         actionButton(ns("infoSeason"),
                                                                      tags$i(class = "fas fa-info",
                                                                             style="font-size: 8px"),
                                                                      class="infoBubbleButton")),
                                                       FALSE)
                                         ),
                                     br(),
                                     box(id="box_elefan_ga_seasonPar",
                                         width = 6,
                                         sliderInput(ns("ELEFAN_GA_C"),
                                                     p(HTML(paste0("Amplitude (",withMathJax("\\(C\\)"),")")),
                                                       actionButton(ns("infoC"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 8px"),
                                                                    class="infoBubbleButton")),
                                                     value=c(0,1), min = 0, max = 1, step=0.01),
                                         br(),
                                         sliderInput(ns("ELEFAN_GA_ts"),
                                                     p(HTML(paste0("Summer point (",withMathJax("\\(t_{s}\\)"),")")),
                                                       actionButton(ns("infots"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 8px"),
                                                                    class="infoBubbleButton")),
                                                     value=c(0,1), min = 0, max = 1, step=0.01)
                                         )
                                     ),

                              box(title = p("ELEFAN's genetic algorithm",
                              actionButton(ns("info_GA"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                                     width = 3,
                                     numericInput(ns("ELEFAN_GA_popSize"),
                                                  p("Population size:",
                                                    actionButton(ns("infoPopSize"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 8px"),
                                                                 class="infoBubbleButton")),
                                                  100, min = 50, max = 1e3, step=1,
                                                  width = "90%"),
                                     numericInput(ns("ELEFAN_GA_maxiter"),
                                                  p("Maximum number of generations",
                                                    actionButton(ns("infoMaxIter"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 8px"),
                                                                 class="infoBubbleButton")),
                                                  50, min = 20, max = 1e3, step=1,
                                                  width = "90%"),
                                     numericInput(ns("ELEFAN_GA_run"),
                                                  p("Number of generations without improvement",
                                                    actionButton(ns("infoMaxRuns"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 8px"),
                                                                 class="infoBubbleButton")),
                                                  20, min = 10, max = 1e3, step=1,
                                                  width = "90%"),

                                     numericInput(ns("ELEFAN_GA_pmutation"),
                                                  p("Probability of mutation",
                                                    actionButton(ns("infoPmut"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 8px"),
                                                                 class="infoBubbleButton")),
                                                  0.2, min = 0.1, max = 1, step=0.1,
                                                  width = "90%"),

                                     numericInput(ns("ELEFAN_GA_pcrossover"),
                                                  p("Probability of crossover",
                                                    actionButton(ns("infoPcross"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 8px"),
                                                                 class="infoBubbleButton")),
                                                  0.8, min = 0.1, max = 1, step=0.1,
                                                  width = "90%"),

                                     numericInput(ns("ELEFAN_GA_elitism"),
                                                  p("Degree of elitism",
                                                    actionButton(ns("infoElite"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 8px"),
                                                                 class="infoBubbleButton")),
                                                  5, min = 1, max = 1e2, step=1,
                                                  width = "90%")
                                     )
                                 ),

                        tabPanel("3. Stock status",

                                 fluidRow(

                                     box(
                                         title = p(HTML(paste0("Length-weight relationship (",
                                                               withMathJax("\\(W = a \ L^{b}\\)"),")")),
                                                   actionButton(ns("infoLengthWeight"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),

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
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                         width = 5,
                                         height = "200px",
                                         br(),
                                         fluidRow(
                                             column(5,
                                                    selectInput(ns("natM"),
                                                                "Method:",
                                                                choices = c("Then's growth formula",
                                                                            "Pauly's growth & temp. formula",
                                                                            "Then's max. age formula"),
                                                                selected = "Then's growth formula",
                                                                width ='90%')
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
                                                                     width = '40%'),
                                                        )
                                                    )
                                         )
                                         ),

                                     box(title = p("Adjust length data",
                                                   actionButton(ns("infoAdjData"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                         width = 3,
                                         height = "200px",
                                         br(),
                                         fluidRow(
                                             column(12,
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
                                     box(title = p("Maturity (optional)",
                                                   actionButton(ns("infoMat"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                         width = 4,
                                         height = "200px",
                                         br(),
                                         fluidRow(
                                             column(6,
                                                    numericInput(ns("Lm50"),
                                                                 label=p(withMathJax("\\(L_{m50}\\)")),
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 1,
                                                                 width = "60%")),
                                             column(6,
                                                    numericInput(ns("Lm75"),
                                                                 label=p(withMathJax("\\(L_{m75}\\)")),
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 1,
                                                                 width = "60%"))
                                         )
                                         ),

                                     box(title = p("Selectivity",
                                                   actionButton(ns("infoSelect"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                         width=8,
                                         height = "200px",
                                         br(),
                                         fluidRow(
                                             column(4,
                                                    selectInput(ns("select"),
                                                                p("Selectivity"),
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
                                                                     p(withMathJax("\\(L_{50}\\)")," (user defined)"),
                                                                     value = 0, min = 0, step = 1,
                                                                     width = "80%")
                                                    )
                                                    ),
                                             column(4,
                                                    div(
                                                        id ="ui_l75",
                                                        numericInput(ns("l75_user"),
                                                                     p(withMathJax("\\(L_{75}\\)")," (user defined)"),
                                                                     value = 0, min = 0, step = 1,
                                                                     width = "80%")
                                                    ),
                                                    div(
                                                        id ="ui_wqs",
                                                        numericInput(ns("wqs_user"),
                                                                     p("Width (",withMathJax("\\(L_{75}-L_{25}\\)"),"; user defined)"),
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
                                                                   style="font-size: 8px"),
                                                            class="infoBubbleButton")),
                                     width=12,
                                     height = "200px",
                                     br(),
                                     fluidRow(
##                                         column(1),
                                         column(2,
                                                div(style = "margin-top:32px; margin-left:100px;",
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
##                                         column(1),
                                         column(2,
                                                div(style = "margin-top:32px; margin-left:20px;",
                                                    p("Length at 50% selectivity (",withMathJax("\\(L_{50}\\)"),")"))
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
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
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
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    height = "2600px",
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
