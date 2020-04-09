tabBasicVonBertalannfy <-
  tabItem("BasicVonBertalannfy",
    htmlOutput("basicVonBertalannfyTitle"),
    actionButton("vonBertalannfyInfo", "More Information", class="topLevelInformationButton"),
    fluidRow(id = "box_vonbertalannfy_x",
      bsModal("modalExample", "Generalized Von Bertalanffy Growth Function (VBGF)", "vonBertalannfyInfo", size = "large", htmlOutput("vonBertalannfyInfoText")),
      box( width= 50,  id = "box_vonbertalannfy",
        fluidRow(
          box( id="box_vonbertalannfy_in",
            sliderInput("amax", "Age classes:", 
              min=1, max=100, value=50),    
            sliderInput("Linf", withMathJax("$$L_\\infty:$$"), 
              min=1, max=1000, value=100),
            sliderInput("k", "k:", 
              min = 0.01, max = 1, value = 0.1,step=0.01),
            sliderInput("t0", withMathJax("$$t_0:$$"),
              min = -5, max = 5, value = 0,step=0.1)
            ),
            box(
              plotOutput("VBGFplot"),
              h3(withMathJax(helpText('$$L_t = L_\\infty(1-e^{(-k(t-t_0))})$$')))
            )
          )
        )
      )
    )