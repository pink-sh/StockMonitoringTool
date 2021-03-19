tabBasicVonBertalannfy <- function(id) {
  ns <- NS(id)
  tabItem("BasicVonBertalannfy",
    htmlOutput("basicVonBertalannfyTitle"),
    actionButton("vonBertalannfyInfo", "More Information", class="topLevelInformationButton"),
    fluidRow(id = "box_vonbertalannfy_x",
      bsModal("modalExample", "Generalized Von Bertalanffy Growth Function (VBGF)", "vonBertalannfyInfo", size = "large", htmlOutput("vonBertalannfyInfoText")),
      box( width= 50,  id = "box_vonbertalannfy",
        fluidRow(
          box( id="box_vonbertalannfy_in",
            sliderInput(ns("amax"), "Age classes:",
              min=0, max=100, value=50, step=0.25),
            sliderInput(ns("Linf"), withMathJax("$$L_\\infty:$$"),
              min=1, max=1000, value=100),
            sliderInput(ns("k"), "k:",
              min = 0.01, max = 10, value = 0.1,step=0.01),
            sliderInput(ns("t0"), withMathJax("$$t_0:$$"),
              min = -5, max = 0, value = 0, step=0.01)
            ),
            box(
              plotOutput(ns("VBGFplot")),
              h3(withMathJax(helpText('$$L_t = L_\\infty(1-e^{(-k(t-t_0))})$$')))
            )
          )
        )
      )
    )
}
