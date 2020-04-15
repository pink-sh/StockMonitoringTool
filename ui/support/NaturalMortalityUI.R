tabNaturalMortality <- function(id) {
  ns <- NS(id)
  tabItem("NaturalMortality",
    htmlOutput("naturalMortalityTitle"),
    actionButton("naturalMortalityInfo", "More Information", class="topLevelInformationButton"),
    fluidRow(id = "box_naturalMortality_x",
      bsModal("modalExample2", "Estimating Natural Mortality (M) from FishBase life history parameters", "naturalMortalityInfo", size = "large", htmlOutput("naturalMortalityInfoText")),
      box( width= 50,  id = "box_naturalMortality",
        fluidRow(
          box( id="box_naturalMortality_in",
            textInput(ns("Species"), label="Type genus and species name found in FishBase:", value = "Gadus morhua"),
            actionButton(label="Submit", inputId="nm_sub_fb"),
            numericInput(ns("Amax"), "Maximum age (years):", value=NA,min=1, max=300, step=0.1),
            numericInput(ns("Linf"),"Linf (in cm):", value=NA,min=1, max=1000, step=0.01),
            numericInput(ns("k"), "VBGF Growth coeff. k:", value=NA,min = 0.001, max = 1,step=0.01),
            numericInput(ns("t0"), "VBGF age at size 0 (t_0)", value=NA,min = -15, max = 15,step=0.01),
            numericInput(ns("Amat"),"Age at maturity (years)", value=NA,min = 0.01, max = 100,step=0.01),
            numericInput(ns("Temp"),"Water temperature (in C):" , value=NA,min = 0.001, max = 60,step=0.01),
            numericInput(ns("Bl"),"Body length (cm):",value=NA,min = 0.01, max = 10000,step=0.01),
            numericInput(ns("User_M"),"User M input (Type value and hit 'Enter'):",value=NA,min = 0, max = 10,step=0.001),
            h4(em(tags$b("FishBase values for species of interest:"))),
            column(1,tableOutput("Ftable")),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h4("Composite M: method weighting"),
            h5(p(em("Allows for weighting of the contribution of each method in the composite M distribution"))),
            h5("Values range from 0 to 1. A value of 0 removes the contribution; a value of 1 is full weighting."),
            h5("Default values are based on redundancies of methods using similar information."),
            h5("For instance,the four max. age methods are given a weight of 0.25, so all weighted together equal 1"),
            wellPanel(
              fluidRow(
                column(4,numericInput(ns("Then_Amax_1"),"Then_Amax 1",value=0.25,min = 0, max = 1,step=0.001)),
                column(4,numericInput(ns("Then_Amax_2"),"Then_Amax 2",value=0.25,min = 0, max = 1,step=0.001)),
                column(4,numericInput(ns("Then_Amax_3"),"Then_Amax 3",value=0.25,min = 0, max = 1,step=0.001))
              ),
              fluidRow(
                column(4,numericInput(ns("Hamel_Amax"),"Hamel_Amax",value=0.25,min = 0, max = 1,step=0.001)),
                column(4,numericInput(ns("AnC"),"AnC",value=0,min = 0, max = 1,step=0.001)),
                column(4,numericInput(ns("Then_VBGF"),"Then_VBGF",value=0.34,min = 0, max = 1,step=0.001))
              ),
              fluidRow(
                column(4,numericInput(ns("Jensen_VBGF_1"),"Jensen_VBGF 1",value=0.33,min = 0, max = 1,step=0.001)),
                column(4,numericInput(ns("Jensen_VBGF_2"),"Jensen_VBGF 2",value=0.33,min = 0, max = 1,step=0.001)),
                column(4,numericInput(ns("Pauly_lt"),"Pauly_lt",value=0.5,min = 0, max = 1,step=0.001))
              ),
              fluidRow(
                column(4,numericInput(ns("Gislason"),"Gislason",value=1,min = 0, max = 1,step=0.001)),
                column(4,numericInput(ns("Chen_Wat"),"Chen-Wat",value=0.5,min = 0, max = 1,step=0.001)),
                column(4,numericInput(ns("Roff"),"Roff",value=0.5,min = 0, max = 1,step=0.001))
              ),
              fluidRow(
                column(4,numericInput(ns("Jensen_Amat"),"Jensen_Amat",value=0.5,min = 0, max = 1,step=0.001)),
                column(4,numericInput(ns("Ri_Ef_Amat"),"Ri_Ef_Amat",value=0.5,min = 0, max = 1,step=0.001))
              ),
              fluidRow(
                column(4,numericInput(ns("UserM"),"User M",value=1,min = 0, max = 1,step=0.001)))
              )
            ),
            box(
              h4("Natural mortality (M) estimates by method"),
              plotOutput(ns("Mplot")),
              h4("Natural mortality (M) values"),
              fluidRow(
                column(6,tableOutput(ns("Mtable"))),
                column(6,tableOutput(ns("Mtable2"))),
                div(class="divDividerMain",
                  downloadButton(ns('downloadMs'), 'Download M values'),
                  downloadButton(ns('downloadCW_M_a'), 'Download Chen-Wat. age-specific M values'),
                  h4("Composite natural mortality"),
                  h5(p(em("Blue vertical line indicates median value")))),
                  div(class="divDividerMain2",
                    plotOutput(ns("Mcomposite")),
                    downloadButton(ns('downloadMcompositedensityplot'), 'Download composite M density plot'),
                    downloadButton(ns('downloadMcompositedist'), 'Download composite M for resampling'))
                  )
                )
              )
            )
        )
    )
}