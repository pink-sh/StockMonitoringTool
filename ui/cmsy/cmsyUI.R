tabCmsyIntro <- tabItem("cmsyIntro",htmlOutput("cmsyIntroOut"))
tabCmsySampleDataset <- tabItem("cmsySampleDataset",htmlOutput("cmsySampleDataset"))

tabCmsy <- function(id) {
  ns <- NS(id)
  tabItem("cmsyWidget",
          htmlOutput(ns("cmsyMethodTitle")),
          actionButton("cmsyDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
          fluidRow(

            ## Information tabs
            ## -------------------------------
            bsModal("modalWorkflowCmsy", "Workflow Considerations - CMSY",
                    ns("cmsyWorkflowConsiderations"),
                    size = "large",
                    htmlOutput(ns("cmsyWorkflowConsiderationsText"))),
            
            
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
            
            
            bsModal("modalMethod2", "Assessment Settings - CMSY",
                    ns("cmsyMethodConsiderations2"),
                    size = "large",
                    htmlOutput(ns("cmsyMethodConsiderationsText2"))),
            
            
            bsModal("modalResults", "Results Considerations - CMSY",
                    ns("cmsyResultConsiderations"),
                    size = "large",
                    htmlOutput(ns("cmsyResultConsiderationsText"))),
            
            
            bsModal("modalResults2", "CMSY Assessment Results",
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
    <td>0.015-0.1</td> </tr></table> <br><br>Check <a href=www.FishBase.ca>FishBase.org </a> and <a href=www.SeaLifeBase.se>SeaLifeBase.org </a> for prior estimates, or by using the <a href=https://github.com/James-Thorson-NOAA/FishLife>FishLife R package </a> get_r.R function <br><br>
                        Alternatively, consider that natural mortality (M) of adults can inform the prior r, i.e. r ~ 2*M. Estimates of M can be derived from the Supporting Tools: Natural Mortality Estimator, or can be search for on <a href=www.FishBase.ca>FishBase.org </a>, <a href=www.SeaLifeBase.se>SeaLifeBase.org </a>, or by using the <a href=https://github.com/James-Thorson-NOAA/FishLife>FishLife R package </a>.
                         In addition, if Fmsy is known, it could be used to reinforce or change the prior range set for r by considering that r ~ 2*Fmsy.</p>")),
            
            
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
                    HTML("<p>The prior biomass range relative to the unexploited biomass (B/k) at the beginning of the catch time series.<br><br>
                         **The user should take care when setting the prior estimates for depletion at the beginning and end of the time series. 
                         Depletion levels are assumptions about the initial and current state of the stock, and they have a strong influence on 
                         the results of CMSY, so careful evaluation of these parameters is recommended. These parameters are determined in CMSY 
                         using the relationship between current catch and maximum catch.**</p>")),
            
            
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
                          fully exploited, or overfished?, i.e. the depletion rate <br><br>
                         
                         When choosing the B/k prior for the intermediate year, it often improves the CMSY+ analysis if the intermediate 
                          B/k prior is placed at the end of period of sustained very high catches that are suspected to have led to low biomass by specifying a respective
                          relative range, e.g. as 0.01 – 0.3. Similarly, a longer period of low catches from the start to some intermediate may indicate a period of 
                          large biomass if followed by a substantial increase in catches thereafter. In this case, it is advisable to set the intermediate B/k prior to 
                          the last year with high biomass and indicate a respective range, e.g. as 0.4 – 0.8. <br><br>

<table><tr>    <th><strong> Prior relative biomass (B/k) ranges for CMSY+&nbsp;</strong></th>    <th><strong> </strong></th> </tr><tr>    <td>Nearly unexploited</td>    <td>0.75 – 1.0</td> </tr><tr>    <td>Low depletion </td>    <td>0.4 – 0.8 </td> </tr><tr>    <td>Medium depletion</td>    <td>0.2 – 0.6</td> </tr><tr>  <td>Strong depletion</td>
    <td>0.01 – 0.4</td> </tr><tr>    <td>Very strong depletion </td>    <td>0.01 – 0.2 </td> </tr></table> <br><br></p>")),
            
            
            bsModal("info_endb", "Ending depletion range", ns("infoendb"),
                    size = "large",
                    HTML("<p>The prior biomass range relative to the unexploited biomass (B/k) at the end of the catch time series.<br><br> 
                         **The user should take care when setting the prior estimates for depletion at the beginning and end of the time series. 
                        Depletion levels are assumptions about the initial and current state of the stock, and they have a strong influence on the results of CMSY, 
                        so careful evaluation of these parameters is recommended. These parameters are determined in CMSY using the relationship between current catch 
                        and maximum catch.**
                         </p>")),
            
            
            bsModal("info_q", "Catchability year range", ns("infoq"),
                    size = "large",
                    HTML("<p>The start and end year for determining the catchabilitiy coefficient. Set to a recent period of at least 5 years where catch and abundance were relatively stable or had similar trends. <br><br> </p>")),
            
            
            bsModal("info_comp", "Previous assessment comparison", ns("infocomp"),
                    size = "large",
                    HTML("<p> Check this box if you have fisheries reference points from previous assessments to use for comparison to the results from this analysis. These data are optional and are not used in the analysis. <br><br> </p>")),
            
            
            bsModal("info_m", "Natural mortality", ns("infom"),
                    size = "large",
                    HTML("<p>Natural mortality can be estimated in the Supporting Tools: 'Natural Mortality Estimators', or check <a href=www.FishBase.ca>FishBase.org </a> and <a href=www.SeaLifeBase.se>SeaLifeBase.org </a>  for estimates of your species' natural mortality.<br><br>
                          Natural mortality can be useful in setting the resilience prior range (lowest and highest resilience estimates) considering that r ~ 2*M.
                         
                         </p>")),
            
            
            bsModal("info_fmsy", "Fishing mortality", ns("infofmsy"),
                    size = "large",
                    HTML("<p>If Fmsy is known, the resilience prior range (lowest and highest resilience estimates) 
                         could be defined to include the estimate of Fmsy considering that r ~ 2*Fmsy</p>")),
            
            
            
            
            ################## cmsy
            box(id = "box_datupload",
                title = p("Data Upload",
                          actionButton(ns("cmsyDataConsiderations2"),
                                       tags$i(class = "fas fa-info",
                                              style="font-size: 8px"),
                                       class="infoBubbleButton")),

                width = NULL,
                collapsible = T, 
                class = "collapsed-box",
                box(
                  fileInput(ns("fileCmsy"), "Choose Stock CSV File",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv", id="fileCmsy")
                  )
                ),
                box(
                  tags$div(id="stockSelectorContainer")
                )
            ),

            #################
            
            
            
            ## Input - Settings
            ## -------------------------------
            
            box(id = "box_settings",
                title = p("Assessment Settings",
                          actionButton(ns("cmsyMethodConsiderations2"),
                                       tags$i(class = "fas fa-info",
                                              style="font-size: 8px"),
                                       class="infoBubbleButton")),

                width = NULL,
                collapsible = T, 
                class = "collapsed-box",

                
                
                box(title = "Data selection",
                    width=6,
                    
                    fluidRow(
                      div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                          HTML("<b>Select year range of analysis</b>")
                      ),
                      div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                          actionButton(ns("infoYearSel"),
                                       tags$i(class = "fas fa-info",
                                              style="font-size: 8px"),
                                       class="infoBubbleButton")
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
                                              style="font-size: 8px"),
                                       class="infoBubbleButton")
                      ),
                      div(style = "margin-top:-3px",
                          uiOutput(ns("CMSY_years_q_out"))
                      )
                    )
                ),
                
                
                box(title = "Catch time series",
                    id = "box_cmsy_exploPlots",
                    width = 6,
                    tags$div(
                      plotOutput(ns("plot_cmsy_explo1"), width = "90%",
                                 height = "250px"),
                      div(style = "margin-top:-10px; margin-left: 10px",
                          htmlOutput(ns("title_cmsy_explo1"))
                      ),
                      style = "margin-left: 10%;"
                    )
                )),
            
            
            box(title = p("Resilience",
                          actionButton(ns("infor"),
                                       tags$i(class = "fas fa-info",
                                              style="font-size: 8px"),
                                       class="infoBubbleButton")),
                width = 6,
                
                fluidPage(
                  sliderInput(ns("resiliance"),
                              label = div(style='width:400px;',style = "margin-top:-3px",
                                          div(style='float:left;', 'Very Low'),
                                          div(style='float:right;', 'High'),style='font-size: 10px'),
                              min = 0.015, max = 1.5, value = c(0.2,0.8), step=0.001, width = '400px')
                  
                )
            ),
            
            
            
            box(
              title = p("Depletion",
                        actionButton(ns("infodepletion"),
                                     tags$i(class = "fas fa-info",
                                            style="font-size: 8px"),
                                     class="infoBubbleButton")),
              width=12,
              
              
              box(width=4,
                  fluidRow(
                    div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                        HTML("<b>Starting depletion range (B/k) </b>")
                    ),
                    div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                        actionButton(ns("infostb"),
                                     tags$i(class = "fas fa-info",
                                            style="font-size: 8px"),
                                     class="infoBubbleButton")
                    ),
                    sliderInput(ns("stb"),label="",min = 0.01, max = 1,step=0.001,value = c(0.01,0.4))
                  )),
              
              
              box(width=4,
                  checkboxInput(ns("cmsy_checkbox_intb"),
                                p("Does the catch time series have an intermediate year where biomass is particularly high or low?",
                                  actionButton(ns("infointb"),
                                               tags$i(class = "fas fa-info",
                                                      style="font-size: 8px"),
                                               class="infoBubbleButton")),
                                FALSE),
                  
                  
                  br(),
                  
                  
                  box(id="box_cmsy_intb",  
                      width = '100%',
                      fluidRow(
                        numericInput(ns("int.yr"),
                                     p(HTML(paste0("Intermediate year")),
                                       actionButton(ns("infointyr"),
                                                    tags$i(class = "fas fa-info",
                                                           style="font-size: 8px"),
                                                    class="infoBubbleButton")),
                                     value=c(NA), min = 1990, max=2030, step=1), ## MAKE THE MIN/MAX RELATE TO THE MIN/MAX OF THE TIME SERIES. SET DEFAULT TO MAX OF RANGE MINUS 5
                        br(),
                        sliderInput(ns("intb"),
                                    p(HTML(paste0("Intermediate depletion range (B/k)",
                                                  actionButton(ns("infointbval"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")))),
                                    value=c(0.01,0.4), min = 0.01, max = 1, step=0.001)
                      )
                  )
              ),
              
              
              
              box(width=4,  
                  fluidRow(
                    div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                        HTML("<b>Ending depletion range (B/k) </b>")
                    ),
                    div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                        actionButton(ns("infoendb"),
                                     tags$i(class = "fas fa-info",
                                            style="font-size: 8px"),
                                     class="infoBubbleButton")
                    ),
                    sliderInput(ns("endb"),label="",min = 0.01, max = 1,step=0.001,value = c(0.01,0.4))
                  ))
              
            ),
            
            
            
            box(width=12,
                checkboxInput(ns("cmsy_checkbox_comparison"),
                              p("Do you have data from a previous assessment to compare? (optional)",
                                actionButton(ns("infocomp"),
                                             tags$i(class = "fas fa-info",
                                                    style="font-size: 8px"),
                                             class="infoBubbleButton")),
                              FALSE),
                
                br(),
                
                
                box(id='biomass_ref_points',
                    width=6,
                    title='Biomass reference points and MSY',
                    
                    textInput(ns("msy"), p("Maximum Sustainable Yield (", withMathJax("\\(MSY\\)"), ")"), "NA"),
                    textInput(ns("blim"), p("Biomass biological limit (", withMathJax("\\(B_{lim}\\)"), ")"), "NA"),
                    textInput(ns("bpa"), p("Biomass precautionary value (",withMathJax("\\(B_{pa}\\)") , ")"), "NA"),
                    textInput(ns("bmsy"), p("Biomass maximum sustainable yield (", withMathJax("\\(B_{MSY}\\)"), ")"), "NA"),
                    textInput(ns("b40"), p("Biomass at 40% over the unfished level (", withMathJax("\\(B_{40\\%}\\)"), ")"), "NA"),
                    textInput(ns("msyBTrigger"), p("Spawning Stock Biomass at MSY (", withMathJax("\\(SSB_{MSY}\\)"), ")"), "NA")
                ),
                
                
                box(id='fish_mort_ref_pts',
                    width=6,
                    title='Fishing mortality values',
                    textInput(ns("fmsy"),
                              p(HTML(paste0('Fishing mortality at Maximum Sustainable Yield (',withMathJax('\\(F_{MSY}\\)'),')',
                                            actionButton(ns("infofmsy"),
                                                         tags$i(class = "fas fa-info",
                                                                style="font-size: 8px"),
                                                         class="infoBubbleButton")))),"NA"),
                    textInput(ns("flim"), p("Fishing mortality biological limit (", withMathJax("\\(F_{lim}\\)"), ")"), "NA"),
                    textInput(ns("fpa"), p("Fishing mortality precautionary value (", withMathJax("\\(F_{pa}\\)"), ")"), "NA"),
                    textInput(ns("fofl"), p("Fishing mortality at overfishing level (", withMathJax("\\(F_{ofl}\\)"),")"), "NA"),
                    textInput(ns("last_f"), "Last known exploitation rate", "NA"),
                    textInput(ns("m"),
                              p(HTML(paste0("Natural mortality",
                                            actionButton(ns("infom"),
                                                         tags$i(class = "fas fa-info",
                                                                style="font-size: 8px"),
                                                         class="infoBubbleButton")))),"NA")
                    
                    #textInput("btype", "btype indicates if the catch file contains biomass, CPUE or no information associated with the catch time series", "None"),
                    #textInput("comments", "Comments on data and computation", "landings"),
                    #checkboxInput(ns("force.cmsy"), "Check this if CMSY results are to be preferred over the Bayesian State Model results (only when biomass or CPUE is available)", FALSE)
                )
                
            )
          ),
          
          ## Action buttons
          ## -------------------------------
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
              
              fluidRow(
                div(style = "display: inline-block; vertical-align:center; margin-left: 50px;",
                    disabled(actionButton(ns("go_cmsy"),
                                          "Run CMSY Method", 
                                          class="topLevelInformationButton"))
                ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                    actionButton(ns("reset_cmsy"), 
                                 "Reset", 
                                 class="topLevelInformationButton"), 
                ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                    uiOutput(ns("downloadCmsyReportButton"))
                ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                    uiOutput(ns("CmsyVREUpload"))
                ),
              ),
              br(),br()
          ),
          
          br(),
          
          htmlOutput("cmsyWarning"),
          hr(),
          box( width= 100, id = "box_cmsy_results",
               title = p("Results of CMSY Method",
                         actionButton(ns("cmsyResultConsiderations2"),
                                      tags$i(class = "fas fa-info",
                                             style="font-size: 8px"),
                                      class="infoBubbleButton")),
               tags$style(type="text/css",
                          ".recalculating {opacity: 1.0;}"
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

}

resetCmsyInputValues <- function() {
  shinyjs::reset("fileCmsy")
  shinyjs::reset("minOfYear")
  shinyjs::reset("maxOfYear")
  shinyjs::reset("resiliance")
  shinyjs::reset("r.low")
  shinyjs::reset("r.hi")
#shinyjs::reset("stb.low")
#shinyjs::reset("stb.hi")
  shinyjs::reset("stb")
  shinyjs::reset("int.yr")
  shinyjs::reset("intb.low")
  shinyjs::reset("intb.hi")
 #shinyjs::reset("endb.low")
 #shinyjs::reset("endb.hi")
  shinyjs::reset("endb")
  shinyjs::reset("q.start")
  shinyjs::reset("q.end")
  shinyjs::reset("startYear")
  shinyjs::reset("endYear")
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
  shinyjs::reset("force.cmsy")
  #careful removeUI conflict with event
  removeUI(selector="#stockSelectorContainerInner")
  shinyjs::disable("go_cmsy")
  clearResults("box_cmsy_results")
}