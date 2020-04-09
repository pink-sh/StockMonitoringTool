tabSbpr <-
tabItem("SBPRWidget",
  htmlOutput("sbprTitle"),
  htmlOutput("fishMethodsVersion1"),
  actionButton("SBPRDataConsiderations", "Data Considerations", class="topLevelInformationButton"),
  fluidRow(
    bsModal("modalExampleSBPR", "SBPR Data Considerations", "SBPRDataConsiderations", size = "large", htmlOutput("SBPRDataConsiderationsText")),
    box(title = "Main Parameters",
      width = NULL, 
      collapsible = T, 
      class = "collapsed-box",
      box(
        fileInput("fileSbpr", "Choose Input CSV File",
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
          numericInput("SBPR_M", "Single natural mortality (M) rate if M is assumed constant over all ages:", 0.2, min = 0, max = 10, step=0.1),
          numericInput("SBPR_pM", "Proportion of natural mortality that occurs before spawning:", 0.1667, min = 0, max = 10, step=0.0001),
          numericInput("SBPR_maxF", "Maximum value of F range over which SBPR will be calculated:", 2, min = 0, max = 100, step=1)
        ),
        box(
          numericInput("SBPR_pF", "Proportion of fishing mortality that occurs before spawning:", 0.2, min = 0, max = 10, step=0.1),
          numericInput("SBPR_MSP", "Percentage of maximum spawning potential (percent MSP reference point) for which F and SBPR should be calculated:", 30, min = 0, max = 1000, step=1),
          numericInput("SBPR_incrF", "F increment for SBPR calculation:", 0.001, min = 0, max = 10, step=0.001)
        )
      ),
      actionButton("go_sbpr", "Run SBPR"),
      hr(),
      box( width= 100,  id = "box_sbpr_results",
        title = "Results of Fishmethods - SBPR Computation",
        tags$style(type="text/css",
          ".recalculating {opacity: 1.0;}"
        ),
        fluidRow(
          box(
            uiOutput("downloadSbprReport"),
            uiOutput("SBPRVREUpload")
          )
        ),
        fluidRow(
          box(
            plotOutput("sbprOutPlot1"),
            htmlOutput("sbprMSPTableTitle"),
            tableOutput("sbprOutTable")
          ), 
          box(
            plotOutput("sbprOutPlot2")
          )
        )
      )
    )
  )