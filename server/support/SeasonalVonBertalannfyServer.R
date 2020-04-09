output$SVBGFplot <- renderPlot({
  tw<-input$sts+0.5
  ages<-seq(0,input$samax, 0.1)
  tw_line<-seq(tw, input$samax, 1)
  ts_line<-seq(input$sts, input$amax-input$sts, 1)
  solengths.out<-soGVBGF(input$sLinf,input$sk,input$st0, ages, input$sC, input$sts)
  lengths.out<-GVBGF(input$sLinf,input$sk,input$st0, ages)
  # plot VBGF
  plot(ages, lengths.out, col="black",xlab="Age",ylab="Length",xlim=c(0,input$samax),ylim=c(0,input$sLinf*1.1),type="l",lwd=2)
  lines(ages, solengths.out, col="red", type="l",lwd=2)
  abline(v=tw_line, lty=3, lwd=2, col="blue")
  abline(v=ts_line, lty=3, lwd=2, col="green")
  legend("topright", c("Generalized VBGF", "Seasonal VBGF", expression('t'[s]), expression('t'[w])), col=c("black", "red", "green", "blue"),
         lty = c(1, 1, 3, 3), lwd=3)
})