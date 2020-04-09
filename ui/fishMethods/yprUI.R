tabYpr <-
  tabItem("YPRWidget",
    htmlOutput("yprTitle"),
    htmlOutput("fishMethodsVersion2"),
    actionButton("YPRDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
    fluidRow(
      bsModal("modalExampleYPR", "YPR Data Considerations", "YPRDataConsiderations", size = "large", htmlOutput("YPRDataConsiderationsText")),
      box(title = "Main Parameters",
        width = NULL, 
        collapsible = T, 
        class = "collapsed-box",
        box(
          fileInput("fileYpr", "Choose Input CSV File",
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
            numericInput("YPR_M", "Single natural mortality (M) rate if M is assumed constant over all ages", 0.2, min = 0, max = 10, step=0.1),
            checkboxInput("YPR_Plus", "Plus -  logical value indicating whether the last age is a plus-group", TRUE),
            numericInput("YPR_oldest", "if plus is checked, a numeric value indicating the oldest age in the plus group", 100, min = 0, max = 1000, step=1)
          ),
          box(
            numericInput("YPR_maxF", "Maximum value of F range over which YPR will be calculated. YPR is calculated for F = 0 to maxF", 2, min = 0, max = 100, step=1),
            numericInput("YPR_incrF", "F increment for SBPR calculation", 0.01, min = 0, max = 10, step=0.01)
          )
        ),
        actionButton("go_YPR", "Run YPR"),
        hr(),
        box( width= 100,  id = "box_ypr_results",
          title = "Results of Fishmethods - YPR Computation",
          tags$style(type="text/css",
            ".recalculating {opacity: 1.0;}"
          ),
          fluidRow(
            box(
              uiOutput("downloadYprReport"),
              uiOutput("YPRVREUpload")
            )
          ),
          fluidRow(
            box(
              plotOutput("yprOutPlot")
            ), 
            box(
              htmlOutput("yprDifference"),
              hr(),
              htmlOutput("<b>Ans Matrix</b>"),
              tableOutput("yprOutTable")
              #htmlOutput("yprFishingMortality")
            )
          )
        )
      )
    )