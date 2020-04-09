tabBasicSchaefer <-
  tabItem("BasicSchaefer",
    htmlOutput("basicShaeferTitle"),
    actionButton("basicShaeferMoreInfo", "More Information", class="topLevelInformationButton"),
    fluidRow(id = "box_shaefer_x",
      bsModal("modalExample3", "Surplus production model", "basicShaeferMoreInfo", size = "large", htmlOutput("basicShaeferInfoText")),
      box( width= 50,  id = "box_shaefer",
        fluidRow(
          box( id="box_shaefer_in",
            sliderInput("r", "Intrinsic rate of growth (r):", 
              min=0.01, max=1, value=0.5),    
            sliderInput("K", "Carrying capacity (K):", 
              min=500, max=3500, value=1000),
            withMathJax(),
            uiOutput('shaefer_ex1'),
            helpText('Once the parameters have been estimated, fishery performance indicators useful to fisheries management can be calculated.'),
            uiOutput('shaefer_ex2'),
            uiOutput('shaefer_ex3'),
            uiOutput('shaefer_ex4')
          ),
          box(
            plotOutput("Biomassplot"),
            plotOutput("Growthplot")
          )
        )
      )
    )
  )