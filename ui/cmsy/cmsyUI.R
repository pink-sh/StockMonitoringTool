tabCmsyIntro <- tabItem("cmsyIntro",htmlOutput("cmsyIntroOut"))
tabCmsySampleDataset <- tabItem("cmsySampleDataset",htmlOutput("cmsySampleDataset"))

tabCmsy <- function(id) {
  ns <- NS(id)
  tabItem("cmsyWidget",
          htmlOutput(ns("cmsyMethodTitle")),
          # actionButton("cmsyDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
          fluidRow(
            div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                "More information about "
            ),
            div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                actionButton(ns("cmsyDataConsiderations"), "Data",
                             class="topLevelInformationButton")
            )#,
            # div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
            #     actionButton(ns("cmsyMethodConsiderations"), "Methods",
            #                  class="topLevelInformationButton")
            # ),
            # div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
            #     actionButton(ns("cmsyResultConsiderations"), "Results",
            #                  class="topLevelInformationButton")
            # )
          ),
          
          fluidRow(
            ## Information tabs
            ## -------------------------------
            bsModal("modalExampleCmsy", "Data Loading and Formatting Considerations - CMSY",
                    ns("cmsyDataConsiderations"),
                    size = "large",
                    htmlOutput(ns("cmsyDataConsiderationsText"))),
            
            bsModal("modalExampleCmsy2", "Data Considerations - CMSY",
                    ns("cmsyDataConsiderations2"),
                    size = "large",
                    htmlOutput(ns("cmsyDataConsiderationsText2"))),
            
            bsModal("modalMethod", "Methodological Considerations - CMSY",
                    ns("cmsyMethodConsiderations"),
                    size = "large",
                    htmlOutput(ns("cmsyMethodConsiderationsText"))),
            
            bsModal("modalMethod2", "Methodological Considerations - CMSY",
                    ns("cmsyMethodConsiderations2"),
                    size = "large",
                    htmlOutput(ns("cmsyMethodConsiderationsText2"))),
            
            bsModal("modalResults", "Results Considerations - CMSY",
                    ns("cmsyResultConsiderations"),
                    size = "large",
                    htmlOutput(ns("cmsyResultConsiderationsText"))),
            
            bsModal("modalResults2", "Results Considerations - CMSY",
                    ns("cmsyResultConsiderations2"),
                    size = "large",
                    htmlOutput(ns("cmsyResultConsiderationsText2"))),
            
            
            bsModal("info_yearsel", "Selected years", ns("infoYearSel"),
                    size = "large",
                    HTML("<p>Select all (default) or a range of years in the uploaded data set to be included in the analysis. <br><br> The start year should correspond to the first year from when on the data are thought to be
reliable. <br><br> <strong>Time series should be at least 15 years long.</strong> In theory, the longer the period covered by the data set, the better.</p>")),
            
            bsModal("info_r", "Resilience, or intrinsic growth rate, r", ns("infor"),
                    size = "large",
                    HTML("<p>Resilience,  prior estimate of resilience, corresponding to intrinsic growth rate ranges:<br><br>
<table><tr>    <th><strong> Resilience&nbsp;</strong></th>    <th><strong> Prior r range</strong></th> </tr><tr>    <td>High</td>    <td>0.6-1.5</td> </tr><tr>    <td>Medium</td>    <td>0.2-0.8</td> </tr><tr>    <td>Low</td>    <td>0.05-0.5</td> </tr><tr>  <td>Very low</td>
    <td>0.015-0.1</td> </tr></table> <br><br>Check <a href=www.FishBase.ca>FishBase.org </a> and <a href=www.SeaLifeBase.se>SeaLifeBase.org </a> for prior estimates, or by using the <a href=https://github.com/James-Thorson-NOAA/FishLife>FishLife R package </a> get_r.R function </p>")),
            
            bsModal("info_depletion", "Depletion (B/k)", ns("infodepletion"),
                    size = "large",
                    HTML("<p>The prior biomass range relative to the unexploited biomass (B/k) at the beginning, end and at an intermediate point in the catch time series.<br><br>
What is the most likely stock status for the beginning of the time series: lightly fished, fully exploited, or overfished?, i.e. the depletion rate. <br><br>
In general, the range of the B/k prior should not be less than 0.4, unless the stock is known to be very strongly depleted,
in which case ranges of 0.01-0.3 or 0.01 – 0.2 are appropriate. If the stock was nearly unexploited 0.75-1.0 is appropriate
for the relative start biomass. Setting a range of 0.01 to 1 is also possible, and would indicate no information at all about
stock status, which is, however, unlikely. If a stock is fished it must be smaller than 1. If it is delivering decent catches, it
must be larger than 0.01. <br><br>

<table><tr>    <th><strong> Prior relative biomass (B/k) ranges for CMSY+&nbsp;</strong></th>    <th><strong> </strong></th> </tr><tr>    <td>Nearly unexploited</td>    <td>0.75 – 1.0</td> </tr><tr>    <td>Low depletion </td>    <td>0.4 – 0.8 </td> </tr><tr>    <td>Medium depletion</td>    <td>0.2 – 0.6</td> </tr><tr>  <td>Strong depletion</td>
    <td>0.01 – 0.4</td> </tr><tr>    <td>Very strong depletion </td>    <td>0.01 – 0.2 </td> </tr></table> <br><br>
**The user should take care when setting the prior estimates for depletion at the beginning and end of the time series. 
                         Depletion levels are assumptions about the initial and current state of the stock, and they have a 
                         strong influence on the results of CMSY, so careful evaluation of these parameters is recommended. 
                         These parameters are determined in CMSY using the relationship between current catch and maximum catch.** </p>")),
            
            bsModal("info_stb", "Starting depletion range", ns("infostb"),
                    size = "large",
                    HTML("<p>The prior biomass range relative to the unexploited biomass (B/k) at the beginning of the catch time series.<br><br> </p>")),
           
            bsModal("info_intb", "Intermediate depletion", ns("infointb"),
                    size = "large",
                    HTML("<p>Does the catch time series have a year where biomass is particularly high or low? <br><br>If there is a year with particularly high or low biomass in the time series, check this box to specify which year and relative biomass ranges. This could occur when, e.g.,
                      exploitation changed from light to full, or where an extraordinarily large year class entered the fishery.
                      Otherwise, leave unchecked.</p>")),
            
            bsModal("info_intyr", "Intermediate year", ns("infointyr"),
                    size = "large",
                    HTML("<p>A year in the time series for an intermediate biomass level. This setting is automatically calculated if not set, though it must be specified by user if the intermediate biomass range below is specified.<br><br>
                         Consider setting this value to an intermediate year where biomass is considered to have been particularly high or low,
e.g. exploitation change from light to full, or where an extraordinarily large year class entered the fishery. When choosing the B/k prior for 
the intermediate year, it often improves the CMSY+ analysis if the intermediate B/k prior is placed at the end of period of sustained very high 
catches that are suspected to have led to low biomass. Similarly, a longer period of low catches from the start to some intermediate may indicate 
a period of large biomass if followed by a substantial increase in catches thereafter. In this case, it is advisable to set the intermediate B/k 
prior to the last year with high biomass.</p>")),
            
            bsModal("info_intb_value", "Intermediate depletion biomass", ns("infointbval"),
                    size = "large",
                    HTML("<p>The estimated relative biomass range (B/k) at an intermediate year of the catch time series <br><br>
                         What is the most likely stock status for an intermediate year of the time series: lightly fished, 
fully exploited, or overfished?, i.e. the depletion rate
                         
                         When choosing the B/k prior for the intermediate year, it often improves the CMSY+ analysis if the intermediate 
B/k prior is placed at the end of period of sustained very high catches that are suspected to have led to low biomass by specifying a respective
relative range, e.g. as 0.01 – 0.3. Similarly, a longer period of low catches from the start to some intermediate may indicate a period of 
large biomass if followed by a substantial increase in catches thereafter. In this case, it is advisable to set the intermediate B/k prior to 
the last year with high biomass and indicate a respective range, e.g. as 0.4 – 0.8. <br><br>

<table><tr>    <th><strong> Prior relative biomass (B/k) ranges for CMSY+&nbsp;</strong></th>    <th><strong> </strong></th> </tr><tr>    <td>Nearly unexploited</td>    <td>0.75 – 1.0</td> </tr><tr>    <td>Low depletion </td>    <td>0.4 – 0.8 </td> </tr><tr>    <td>Medium depletion</td>    <td>0.2 – 0.6</td> </tr><tr>  <td>Strong depletion</td>
    <td>0.01 – 0.4</td> </tr><tr>    <td>Very strong depletion </td>    <td>0.01 – 0.2 </td> </tr></table> <br><br></p>")),
            
            bsModal("info_endb", "Ending depletion range", ns("infoendb"),
                    size = "large",
                    HTML("<p>The prior biomass range relative to the unexploited biomass (B/k) at the end of the catch time series.<br><br> </p>")),
           
             bsModal("info_q", "Catchability year range", ns("infoq"),
                    size = "large",
                    HTML("<p>The start and end year for determining the catchabilitiy coefficient. Set to a recent period of at least 5 years where catch and abundance were relatively stable or had similar trends. <br><br> </p>")),
            
            bsModal("info_comp", "Previous assessment comparison", ns("infocomp"),
                    size = "large",
                    HTML("<p> Check this box if you have fisheries reference points from previous assessments used for comparison to the results from this analysis. These data are optional and are not used in the analysis. <br><br> </p>")),
            
            # # bsModal("modalExampleCMSY", "CMSY Data Considerations", "cmsyDataConsiderations", size = "large", htmlOutput(ns("cmsyDataConsiderationsText"))),
            # box(title = "Data Upload",
            #     width = NULL,
            #     collapsible = F, 
            #     class = "collapsed-box",
            #     box(
            #       fileInput(ns("fileCmsy"), "Choose Input CSV File",
            #                 accept = c(
            #                   "text/csv",
            #                   "text/comma-separated-values,text/plain",
            #                   ".csv", id="fileCmsy")
            #       )
            #     ),
            #     box(
            #       tags$div(id="stockSelectorContainer")
            #     )
            # ),
            
            ################## cmsy
            box(id = "box_datupload",
                title = p("Data Upload",
                          actionButton(ns("cmsyDataConsiderations2"),
                                       tags$i(class = "fas fa-info",
                                              style="font-size: 12px"),
                                       class="topLevelInformationButton")),
                width = NULL,
                collapsible = FALSE,
                solidHeader = TRUE,
                class = "collapsed-box",
                
                box(
                  fileInput(ns("fileCmsy"), "Choose Input CSV File",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                  )
                ),
                box(
                  tags$div(id="stockSelectorContainer")
                )
            ),
            #################
            
            
            
            ## Input - Settings
            ## -------------------------------
            # box(title = "Assessment Settings",
            #     width = NULL,
            #     collapsible = T, 
            #     class = "collapsed-box",
            #     collapsed = T,
            
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
                
                fluidRow(
                box(title = "Data selection",
                    width=6,
                
                    fluidRow(
                      div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                          HTML("<b>Select year range of analysis</b>")
                      ),
                      div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                          actionButton(ns("infoYearSel"),
                                       tags$i(class = "fas fa-info",
                                              style="font-size: 12px"),
                                       class="topLevelInformationButton")
                      ),
                    div(style = "margin-top:-3px",
                        uiOutput(ns("CMSY_years_selected_out"))
                    )
                    ),
                    br(),    
                    
                    fluidRow(
                      div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                          HTML("<b>Select years over which to calculate catchability</b>")
                      ),
                      div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                          actionButton(ns("infoq"),
                                       tags$i(class = "fas fa-info",
                                              style="font-size: 12px"),
                                       class="topLevelInformationButton")
                      ),
                      div(style = "margin-top:-3px",
                          uiOutput(ns("CMSY_years_q_out"))
                      )
                    )
                    ),
                    
                # box(title = p("Select year range of analysis",
                #     actionButton(ns("infoYearSel"),
                #                  tags$i(class = "fas fa-info",
                #                         style="font-size: 12px"),
                #                  class="topLevelInformationButton")),
                #     width = 6,
                #     
                #     fluidRow(
                #       div(style = "margin-top:-3px",
                #           uiOutput(ns("CMSY_years_selected_out"))
                #       )
                #     )
                # ),
                    
                # box(width=6,  
                #     title=p('Select years over which to calculate catchability',
                #             actionButton(ns("infoq"),
                #                          tags$i(class = "fas fa-info",
                #                                 style="font-size: 12px"),
                #                          class="topLevelInformationButton")),
                #     fluidRow(
                #        div(style = "margin-top:-3px",
                #           uiOutput(ns("CMSY_years_q_out"))
                #       )
                #     )
                # )
                # ),
                
                box(title = "Catch time series",
                    id = "box_cmsy_exploPlots",
                    width = 6,
                    tags$div(
                      plotOutput(ns("plot_cmsy_explo1"), width = "90%",
                                 height = "250px"),
                      div(style = "margin-top:-10px; margin-left: 10px",
                          htmlOutput(ns("title_cmsy_explo1"))
                      ),
                      ## plotOutput(ns("plot_explo2"), width = "90%",
                      ##            height = "280px"),
                      ## div(style = "margin-top:-10px; margin-left: 10px",
                      ##     htmlOutput(ns("title_explo2"))
                      ##     ),
                      style = "margin-left: 10%;"
                    )
                )),
                
                fluidRow(          
                box(title = p("Resilience",
                               actionButton(ns("infor"),
                                            tags$i(class = "fas fa-info",
                                                   style="font-size: 12px"),
                                            class="topLevelInformationButton")),
                    width = 6,

                    sliderInput(ns("resiliance"),
                                label = div(style='width:400px;',style = "margin-top:-3px",
                                            div(style='float:left;', 'Very Low'),
                                            div(style='float:right;', 'High'),style='font-size: 10px'),
                                min = 0.015, max = 1.5, value = c(0.2,0.8), step=0.001, width = '400px')
                                
                    )),
                    # br(),
                box(
                    title = p("Depletion",
                              actionButton(ns("infodepletion"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 12px"),
                                           class="topLevelInformationButton")),
                    width=12,
                    box(width=4,
                    fluidRow(
                      div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                          HTML("<b>Starting depletion range (B/k) </b>")
                      ),
                      div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                          actionButton(ns("infostb"),
                                       tags$i(class = "fas fa-info",
                                              style="font-size: 12px"),
                                       class="topLevelInformationButton")
                      ),
                      sliderInput(ns("stb"),label="",min = 0.01, max = 1,step=0.001,value = c(0.01,0.4))
                    )),
                    
                    box(width=4,
                      checkboxInput(ns("cmsy_checkbox_intb"),
                                      p("Does the catch time series have an intermediate year where biomass is particularly high or low?",
                                        actionButton(ns("infointb"),
                                                     tags$i(class = "fas fa-info",
                                                            style="font-size: 12px"),
                                                     class="topLevelInformationButton")),
                                      FALSE),
                    # ),
                    br(),
                    box(id="box_cmsy_intb",  ## THIS BOX TO ALIGN WITH IN VERTICAL ALIGNMENT WITH cmsy_checkbox_intb WHEN OPENED.
                        width = '100%',
                        fluidRow(
                        numericInput(ns("int.yr"),
                                    p(HTML(paste0("Intermediate year")),
                                      actionButton(ns("infointyr"),
                                                   tags$i(class = "fas fa-info",
                                                          style="font-size: 12px"),
                                                   class="topLevelInformationButton")),
                                    value=c(NA), min = 1990, max=2030, step=1), ## MAKE THE MIN/MAX RELATE TO THE MIN/MAX OF THE TIME SERIES
                        br(),
                        sliderInput(ns("intb"),
                                    p(HTML(paste0("Relative biomass at an intermediate year",
                                      actionButton(ns("infointbval"),
                                                   tags$i(class = "fas fa-info",
                                                          style="font-size: 12px"),
                                                   class="topLevelInformationButton")))),
                                    value=c(0.01,0.4), min = 0.01, max = 1, step=0.001)
                                    # sliderInput(ns("stb"),label="",min = 0.01, max = 1,step=0.001,value = c(0.01,0.4))
                    )
                    )
                    ),
                
                    
                    
                    box(width=4,  ## THIS BOX TO BE IN HORIZONTAL ALIGNMENT - one row for DEPLETION box
                    fluidRow(
                      div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                          HTML("<b>Ending depletion range (B/k) </b>")
                      ),
                      div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                          actionButton(ns("infoendb"),
                                       tags$i(class = "fas fa-info",
                                              style="font-size: 12px"),
                                       class="topLevelInformationButton")
                      ),
                      sliderInput(ns("endb"),label="",min = 0.01, max = 1,step=0.001,value = c(0.01,0.4))
                    ))
                    
                    ),
                    
                # ,
                box(width=12,
                    checkboxInput(ns("cmsy_checkbox_comparison"),
                                  p("Do you have data from a previous assessment to compare? (optional)",
                                    actionButton(ns("infocomp"),
                                                 tags$i(class = "fas fa-info",
                                                        style="font-size: 12px"),
                                                 class="topLevelInformationButton")),
                                  FALSE),
                    # ),
                    br(),
                    box(id='biomass_ref_points',
                      width=6,
                        title='Biomass reference points and MSY',
                    # numericInput(ns("minOfYear"), p("Earliest year of the catch series (", withMathJax("\\(minOfYear\\)"), ")"), 1998, min = 1900, max = 2030, step=1),
                    # numericInput(ns("maxOfYear"), p("Latest year of the catch series (", withMathJax("\\(maxOfYear\\)"), ")"), 2015, min = 1900, max = 2030, step=1),
                    # selectInput(ns("resiliance"), p("Resilience, or intrinsic growth rate (", withMathJax("\\(r\\)"), ") as qualitative information (Use information from FishBase or SeaLifeBase)"), choices=c("Very low", "Low", "Medium", "High"), selected="Medium"),
                    # textInput(ns("r.low"), "Lower limit of resilience (Both the high and low range of this parameter must be set by the user, otherwise, the range is calculated automatically from Resilience)", "NA"),
                    # #numericInput(ns("r.low"), "Lowest resilience (automatically calculated if not set)", "NA", min = 10^-5, max = NA, step=NA),
                    # textInput(ns("r.hi"), "Upper limit of resilience (Both the high and low range of this parameter must be set by the user, otherwise, the range is calculated automatically from Resilience)", "NA"),
                    # p("**The user should take care when setting the prior estimates for depletion at the beginning and end of the time series. Depletion levels are assumptions about the initial and current state of the stock, and they have a strong influence on the results of CMSY, so careful evaluation of these parameters is recommended. These parameters are determined in CMSY using the relationship between current catch and maximum catch."),
                    #numericInput(ns("stb.low"), "**Starting depletion range: Lowest possible relative biomass at the beginning of the catch time series (automatically calculated if not set)", 0, min = 0, max = 10, step=0.1),
                    #numericInput(ns("stb.hi"), "**Starting depletion range: Highest possible relative biomass at the beginning of the catch time series (automatically calculated if not set)", 0, min = 0, max = 10, step=0.1),
                    # sliderInput(ns("stb"), "**Starting depletion range: Lower and upper limits of relative biomass at the beginning of the catch time series (automatically calculated if not set)",min = 0, max = 10,step=0.1,value = c(0,0)),
                    # textInput(ns("int.yr"), p("Intermediate year (", withMathJax("\\(int.yr\\)"), " automatically calculated if not set. Must be specified by user if intermediate biomass range is specified below)"), "NA"),
                    # textInput(ns("intb.low"), "Lower limit of relative biomass at the intermediate year of the catch time series (intermediate year, low range and high range must all be set by user; otherwise leave all three fields blank)", "NA"),
                    # textInput(ns("intb.hi"), "Upper limit of relative biomass at the intermediate year of the catch time series (intermediate year, low range and high range must all be set by user; otherwise leave all three fields blank)", "NA"),
                    #numericInput(ns("endb.low"), "**Ending depletion range: Lowest possible relative biomass at the end of the catch time series (automatically calculated if not set)", 0.01, min = 0, max = 10, step=0.01),
                    #numericInput(ns("endb.hi"), "**Ending depletion range: Highest possible relative biomass at the end of the catch time series (automatically calculated if not set)", 0.4, min = 0, max = 10, step=0.1),
                    # sliderInput(ns("endb"), "**Ending depletion range: Lower and upper limits of relative biomass at the end of the catch time series (automatically calculated if not set)",min = 0, max = 10,step=0.1,value = c(0.01,0.4)),
                    
                    
                    # textInput(ns("q.start"), p("Start year over which to calculate catchability (", withMathJax("\\(q\\)"), ") value at the beginning of a stable catch-biomass period (", withMathJax("\\(q.start\\)"), " automatically calculated if not set)"), "NA"),
                    # textInput(ns("q.end"), p("End year over which to calculate catchability (", withMathJax("\\(q\\)"), ") at the end of a stable catch-biomass period (", withMathJax("\\(q.end\\)"), " automatically calculated if not set)"), "NA")
                    textInput(ns("blim"), p("Biomass biological limit (", withMathJax("\\(B_{lim}\\)"), ")"), "NA"),
                    textInput(ns("bpa"), p("Biomass precautionary value (",withMathJax("\\(B_{pa}\\)") , ")"), "NA"),
                    textInput(ns("bmsy"), p("Biomass maximum sustainable yield (", withMathJax("\\(B_{MSY}\\)"), ")"), "NA"),
                    textInput(ns("b40"), p("Biomass at 40% over the unfished level (", withMathJax("\\(B_{40\\%}\\)"), ")"), "NA"),
                    textInput(ns("msy"), p("Maximum Sustainable Yield (", withMathJax("\\(MSY\\)"), ")"), "NA"),
                    textInput(ns("msyBTrigger"), p("Spawning Stock Biomass at MSY (", withMathJax("\\(SSB_{MSY}\\)"), ")"), "NA")
                    ),
                box(id='fish_mort_ref_pts',
                  width=6,
                    title='Fishing mortality values',
                    
                  # numericInput(ns("startYear"), "Start year to process the catch series from", 1998, min = 1900, max = 2030, step=1),
                  # numericInput(ns("endYear"), "End year to process the catch series up to", 2015, min = 1900, max = 2030, step=1),
                  
                  textInput(ns("fmsy"), p("Fishing mortality at Maximum Sustainable Yield (",withMathJax("\\(F_{MSY}\\)") , "). If" 
                                          ,withMathJax("\\(F_{MSY}\\)") ,"is known, the resilience prior range (lowest and highest resilience estimates) 
              could be defined to include estimate of", withMathJax("\\(F_{MSY}\\)") , 
                                          "assuming that r", withMathJax("\\(\\approx\\)"),withMathJax("\\(F_{MSY}\\)")), "NA"),
                  textInput(ns("flim"), p("Fishing mortality biological limit (", withMathJax("\\(F_{lim}\\)"), ")"), "NA"),
                  textInput(ns("fpa"), p("Fishing mortality precautionary value (", withMathJax("\\(F_{pa}\\)"), ")"), "NA"),
                  textInput(ns("fofl"), p("Fishing mortality at overfishing level (", withMathJax("\\(F_{ofl}\\)"),")"), "NA"),
                  textInput(ns("last_f"), "Last known exploitation rate", "NA"),
                 
                   textInput(ns("m"), p("Natural mortality (", withMathJax("\\(M\\)"), ")"), "NA")
                  # p("**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M here."),
                  ##KK: it's not clear to me what the user input would be here if not "None". Suggest deleting (also for Comments.
                  #textInput("btype", "btype indicates if the catch file contains biomass, CPUE or no information associated with the catch time series", "None"),
                  #textInput("comments", "Comments on data and computation", "landings"),
                  #checkboxInput(ns("force.cmsy"), "Check this if CMSY results are to be preferred over the Bayesian State Model results (only when biomass or CPUE is available)", FALSE)
                )
            )
            ),
            tags$div(disabled(actionButton(ns("go_cmsy"), "Run CMSY Method", class="topLevelInformationButton")),
                     actionButton(ns("reset_cmsy"), "Reset", class="topLevelInformationButton"), style="margin-left: 15px;")
            ,
            htmlOutput("cmsyWarning"),
            hr(),
            box( width= 100, id = "box_cmsy_results",
                 title = "Results of CMSY Method",
                 tags$style(type="text/css",
                            ".recalculating {opacity: 1.0;}"
                 ),
                 fluidRow(
                   box(
                     uiOutput(ns("downloadCmsyReportButton")),
                     uiOutput(ns("CmsyVREUpload"))
                   )
                 ),
                 fluidRow(
                   box(
                     "The upper left panel shows catches relative to the estimate of MSY, with indication of 95% confidence limits in grey. The upper right panel shows the development of relative total biomass (B/Bmsy), with the grey area indicating uncertainty. The lower left graph shows relative exploitation (F/Fmsy), with Fmsy corrected for reduced recruitment below 0.5 Bmsy. The lower-right panel shows the trajectory of relative stock size (B/Bmsy) over relative exploitation (F/Fmsy).",
                     htmlOutput(ns("renderCmsyLog")),
                     htmlOutput(ns("renderCmsyInfo"))
                   ),
                   box(id = "box_cmsy_results_charts",
                       htmlOutput(ns("titleCmsyManagementChart")),
                       "Panel A shows in black the time series of catches and in blue the three-years moving average with indication of highest and lowest catch, as used in the estimation of prior biomass by the default rules. Panel B shows the explored r-k log space and in dark grey the r-k pairs which were found by the CMSY model to be compatible with the catches and the prior information. Panel C shows the most probable r-k pair and its approximate 95% confidence limits in blue. Panel D shows in blue the biomass trajectory estimated by CMSY. Dotted lines indicate the 2.5th and 97.5th percentiles. Vertical blue lines indicate the prior biomass ranges. Panel E shows in blue the harvest rate from CMSY. Panel F shows the Schaefer equilibrium curve of catch/MSY relative to B/k, here indented at B/k < 0.25 to account for reduced recruitment at low stock sizes. The blue dots are scaled by CMSY estimates.",
                       imageOutput(ns("renderCmsyManagementChart")),
                       htmlOutput(ns("titleCmsyAnalisysChart")),
                       imageOutput(ns("renderCmsyAnalysisChart"))
                   )
                 )
            )
          )
  )
}

resetCmsyInputValues <- function() {
  shinyjs::reset("fileCmsy")
  shinyjs::reset("rangeYear")
  shinyjs::reset("resiliance")
  shinyjs::reset("stb")
  shinyjs::reset("int.yr")
  shinyjs::reset("intb")
  shinyjs::reset("endb")
  shinyjs::reset("CMSY_years_q")
  shinyjs::reset("CMSY_years_selected")
  shinyjs::reset("blim")
  shinyjs::reset("bpa")
  shinyjs::reset("bmsy")
  shinyjs::reset("b40")
  shinyjs::reset("fmsy")
  shinyjs::reset("flim")
  shinyjs::reset("fpa")
  shinyjs::reset("fofl")
  shinyjs::reset("last_f")
  shinyjs::reset("msy")
  shinyjs::reset("msyBTrigger")
  shinyjs::reset("m")
  #careful removeUI conflict with event
  removeUI(selector="#stockSelectorContainerInner")
  shinyjs::disable("go_cmsy")
  clearResults("box_cmsy_results")
}