tabSeasonalVonBertalannfy <-
  tabItem("SeasonalVonBertalannfy",
    htmlOutput("SeasonalVonBertalannfyTitle"),
    actionButton("SeasonalVonBertalannfyInfo", "More Information", class="topLevelInformationButton"),
    fluidRow(id = "box_vonbertalannfy_x",
      bsModal("modalExample4", "Seasonal Von Bertalanffy Growth Function (VBGF)", "SeasonalVonBertalannfyInfo", size = "large", htmlOutput("seasonalVonBertalannfyInfoText")),
      box( width= 50,  id = "box_seasonal_vonbertalannfy",
        fluidRow(
          box( id="box_seasonal_vonbertalannfy_in",
            sliderInput("samax", "Age classes:", 
              min=1, max=50, value=5),    
            sliderInput("sLinf", withMathJax("$$L_\\infty:$$"), 
              min=1, max=500, value=21),
            sliderInput("sk", "k:", 
              min = 0.01, max = 1, value = 0.8,step=0.01),
            sliderInput("st0", withMathJax("$$t_0:$$"),
              min = -2, max = 2, value = 0,step=0.01) ,
            sliderInput("sts", withMathJax("$$t_s:$$"),
              min = 0, max = 1, value = 0.5,step=0.01),
            sliderInput("sC", "C (amplitude):",
              min = 0, max = 1, value = 1,step=0.01) 
            ),
            box(
              plotOutput("SVBGFplot"),
              h3(strong(withMathJax(helpText('$$L_t = L_\\infty(1-e^{(-k(t-t_0)-\\frac{Ck}{2\\pi}sin2\\pi(t-t_s))})$$'))))
            )
          )
        )
      ) 
    )