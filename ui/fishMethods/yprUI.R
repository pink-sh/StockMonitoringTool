tabYpr <- function(id) {
  ns <- NS(id)
  tabItem("YPRWidget",
    htmlOutput(ns("yprTitle")),
    htmlOutput("fishMethodsVersion2"),
    actionButton(ns("YPRDataConsiderations"), "Data Considerations", class="topLevelInformationButton"),
    fluidRow(
      bsModal("modalExampleYPR", "YPR Data Considerations", ns("YPRDataConsiderations"), size = "large", htmlOutput(ns("YPRDataConsiderationsText"))),
      box(title = "Main Parameters",
        width = NULL, 
        collapsible = T, 
        class = "collapsed-box",
        box(
          fileInput(ns("fileYpr"), "Choose Input CSV File",
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
            numericInput(ns("YPR_M"), "Single natural mortality (M) rate if M is assumed constant over all ages", 0.2, min = 0, max = 10, step=0.1),
            checkboxInput(ns("YPR_Plus"), "Plus -  logical value indicating whether the last age is a plus-group", TRUE),
            numericInput(ns("YPR_oldest"), "if plus is checked, a numeric value indicating the oldest age in the plus group", 100, min = 0, max = 1000, step=1)
          ),
          box(
            numericInput(ns("YPR_maxF"), "Maximum value of F range over which YPR will be calculated. YPR is calculated for F = 0 to maxF", 2, min = 0, max = 100, step=1),
            numericInput(ns("YPR_incrF"), "F increment for SBPR calculation", 0.01, min = 0, max = 10, step=0.01)
          )
        ),
        tags$div( disabled(actionButton(ns("go_YPR"), "Run YPR", class="topLevelInformationButton")),
                actionButton(ns("reset_ypr"), "Reset", class="topLevelInformationButton"), style="margin-left: 15px;"),
        hr(),
        box( width= 100,  id = "box_ypr_results",
          title = "Results of Fishmethods - YPR Computation",
          tags$style(type="text/css",
            ".recalculating {opacity: 1.0;}"
          ),
          fluidRow(
            box(
              uiOutput(ns("downloadYprReport")),
              uiOutput(ns("YPRVREUpload"))
            )
          ),
          fluidRow(
            box(
              plotOutput(ns("yprOutPlot"))
            ), 
            box(
              htmlOutput(ns("yprDifference")),
              hr(),
              htmlOutput("<b>Ans Matrix</b>"),
              tableOutput(ns("yprOutTable"))
            )
          )
        )
      )
    )
}

resetYPRInputValues <- function() {
  shinyjs::reset("fileYpr")
  shinyjs::reset("YPR_Plus")
  shinyjs::reset("YPR_oldest")
  shinyjs::reset("YPR_incrF")
  shinyjs::disable("go_YPR")
  clearResults("box_ypr_results")
}