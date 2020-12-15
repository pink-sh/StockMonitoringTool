tabSbpr <- function(id) {
  ns <- NS(id)
  tabItem("SBPRWidget",
    htmlOutput(ns("sbprTitle")),
    htmlOutput("fishMethodsVersion1"),
    actionButton(ns("SBPRDataConsiderations"), "Data Considerations", class="topLevelInformationButton"),
    fluidRow(
      bsModal("modalExampleSBPR", "SBPR Data Considerations", ns("SBPRDataConsiderations"), size = "large", htmlOutput(ns("SBPRDataConsiderationsText"))),
      box(title = "Main Parameters",
        width = NULL, 
        collapsible = T, 
        class = "collapsed-box",
        box(
          fileInput(ns("fileSbpr"), "Choose Input CSV File",
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
            numericInput(ns("SBPR_M"), p("Single natural mortality (", withMathJax("\\(M\\)"), ") rate if M is assumed constant over all ages:"), 0.2, min = 0, max = 10, step=0.1),
            numericInput(ns("SBPR_pM"), "Proportion of natural mortality that occurs before spawning:", 0.1667, min = 0, max = 10, step=0.0001),
            numericInput(ns("SBPR_maxF"), "Maximum value of F range over which SBPR will be calculated:", 2, min = 0, max = 100, step=1)
          ),
          box(
            numericInput(ns("SBPR_pF"), "Proportion of fishing mortality that occurs before spawning:", 0.2, min = 0, max = 10, step=0.1),
            numericInput(ns("SBPR_MSP"), "Percentage of maximum spawning potential (percent MSP reference point) for which F and SBPR should be calculated:", 30, min = 0, max = 1000, step=1),
            numericInput(ns("SBPR_incrF"), "F increment for SBPR calculation:", 0.001, min = 0, max = 10, step=0.001)
          )
        ),
        tags$div( disabled(actionButton(ns("go_sbpr"), "Run SBPR", class="topLevelInformationButton")),
                actionButton(ns("reset_sbpr"), "Reset", class="topLevelInformationButton"), style="margin-left: 15px;"),
        hr(),
        box( width= 100,  id = "box_sbpr_results",
          title = "Results of Fishmethods - SBPR Computation",
          tags$style(type="text/css",
            ".recalculating {opacity: 1.0;}"
          ),
          fluidRow(
            box(
              uiOutput(ns("downloadSbprReport")),
              uiOutput(ns("SBPRVREUpload"))
            )
          ),
          fluidRow(
            box(
              plotOutput(ns("sbprOutPlot1")),
              htmlOutput(ns("sbprMSPTableTitle")),
              tableOutput(ns("sbprOutTable"))
            ), 
            box(
              plotOutput(ns("sbprOutPlot2"))
            )
          )
        )
      )
    )
}

resetSBPRInputValues <- function() {
  shinyjs::reset("fileSbpr")
  shinyjs::reset("SBPR_M")
  shinyjs::reset("SBPR_pM")
  shinyjs::reset("SBPR_maxF")
  shinyjs::reset("SBPR_pF")
  shinyjs::reset("SBPR_MSP")
  shinyjs::reset("SBPR_incrF")
  shinyjs::disable("go_sbpr")
  clearResults("box_sbpr_results")
}