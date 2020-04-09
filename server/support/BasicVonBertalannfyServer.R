output$VBGFplot <- renderPlot({
  ages<-c(1:input$amax)
  lengths.out<-GVBGF(input$Linf,input$k,input$t0, ages)
  # plot VBGF
  plot(ages, lengths.out, col = rgb(0/255,86/255,149/255),xlab="Age",ylab="Length",xlim=c(0,input$amax),ylim=c(0,input$Linf*1.1),type="l",lwd=3)
})