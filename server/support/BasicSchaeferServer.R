output$Biomassplot <- renderPlot({
  sbio<-c(0:input$K)
  B.out<-0
  for (i in 1:length(sbio)){
    B.out[i]<-Schaefer(input$K, input$r, sbio[i])
  }
  # plot stock-growth curve:
  plot(sbio, B.out,xlab=expression('Stock biomass B'[t]),
       ylab="Stock growth",xlim=c(0,max(sbio)),type="l",lwd=3)
  Bmsy=max(B.out)
  arrows(input$K, Bmsy/3, input$K, 2, lwd = 1, lty=1, col="blue")
  text(input$K, Bmsy/2.6, "K", col="blue", cex=1)
  abline(v=(max(sbio)/2), lty=1, col="blue", lwd=1)
  text((input$K/1.9), (max(B.out)/2), "K/2", cex=1, col="blue")
  abline(h=max(B.out), lty=1, col="red", lwd=2)
  text((max(sbio)*0.1), (max(B.out)*0.9), "Maximum production", cex=1, col="red")
})
output$Growthplot <- renderPlot({
  hold_stocks<-rep(0, 50)
  hold_stocks[1] = .05
  hold_growth<-rep(0, 50)
  for (i in 1:50){
    hold_growth[i] =input$r * hold_stocks[i] * (1 - hold_stocks[i]/input$K)
    hold_stocks[i+1] = hold_stocks[i] + hold_growth[i]
  }
  
  # plot Schaefer growth:
  plot(1:51, hold_stocks, xlab="Time", ylab=expression('Stock biomass B'[t]),type="l",lwd=2, col="red")
  abline(h=input$K, col="blue", lwd=1, lty=2)
  text(10, (input$K-0.1*input$K), "K: Asymptotic carrying capacity", 
       col = "blue", cex=1)
  text(40, (input$K-0.1*input$K), "Unharvested curve", col = "red", cex=1)
})