naturalMortalityModule <- function(input, output, session) {
    
  
  SPEC <- reactive({
    Amax1<-popchar(input$Species, field="tmax")
    Amax<-mean(Amax1$tmax, na.rm=T) 
    Amat1<-maturity(input$Species, field=c("tm", "Lm"))
    Amat<-mean(Amat1$tm, na.rm=T)
    Bl<-mean(Amat1$Lm, na.rm=T)
    PopGwth<-popgrowth(input$Species, field=c("Loo", "TLinfinity", "K", "to", "Temperature"))
    PopGwth$TLinfinity<-ifelse(is.na(PopGwth$TLinfinity), PopGwth$Loo, PopGwth$TLinfinity)
    PopGwth$phi1<-2*log10(PopGwth$TLinfinity)+log10(PopGwth$K)
    Linf<-mean(PopGwth$TLinfinity, na.rm=T)
    logTLinf<-2*log10(Linf)
    avephi1<-mean(PopGwth$phi1, na.rm=T)
    k<-10^(avephi1-logTLinf)
    t0<-mean(PopGwth$to, na.rm=T)
    Temp<-mean(PopGwth$Temp, na.rm=T)
    SPEC<-cbind.data.frame(Amax,Amat,Bl,Linf,k,t0,Temp)
    names(SPEC)<-c("Amax", "Amat", "Bl", "Linf", "k", "t0", "Temp")
    SPEC
  })
  M_vals_all<- reactive({
    SPEC<-SPEC()
    Amax<-SPEC$Amax
    Amat<-SPEC$Amat
    Bl<-SPEC$Bl
    Linf<-SPEC$Linf
    k<-SPEC$k
    t0<-SPEC$t0
    Temp<-SPEC$Temp
    # Show the first "n" observations
    output$Ftable <- renderTable({
      input$nm_sub_fb
      F_table<-data.frame(cbind(Amax, Amat, Bl, Linf, k, t0, Temp))
      colnames(F_table)<-c("Max Age","Age Mat.", "Mean Length", "VB Linf", "VB k", "VB t0", "Temp.")
      F_table
    })
    Pauly80lt_M<-Pauly80wt_M<-AnC75_M<-Roff_M<-GnD_GSI_M<-PnW_M<-Lorenzen96_M<-Gislason_M<-NA
    if(!anyNA(c(input$Amax))){Then_M_Amax<-Then_M(input$Amax)}
    else{
      Then_M_Amax<-Then_M(Amax)
    }
    if(!(anyNA(c(input$k,input$Amax)))) {
      AnC75_M<-M.empirical(Kl=input$k,tmax=input$Amax,method=4)[1]
      Then_M_VBGF<-Then_VBGF(input$Linf*10,input$k)
      Jensen_M_VBGF<-Jensen_M_k(input$k)
    }
    else {
      if(!(anyNA(c(k,Amax)))){AnC75_M<-M.empirical(Kl=k,tmax=Amax,method=4)[1]
      Then_M_VBGF<-Then_VBGF(Linf*10,k)
      Jensen_M_VBGF<-Jensen_M_k(k)}
    }
    if(!(anyNA(c(input$Linf,input$k,input$Bl)))) {
      Gislason_M<-M.empirical(Linf=input$Linf,Kl=input$k,Bl=input$Bl,method=9)[1]
      CnW_M_VBGF<-Chen_N_Wat_M(input$Amax,input$Amat,input$k,input$t0)
      CnW_M_a_VBGF<-Chen_N_Wat_M(input$Amax,input$Amat,input$k,input$t0,out.type = 0)
      maxage<-input$Amax
    } else {
      if(!(anyNA(c(Linf,k,Bl)))){Gislason_M<-M.empirical(Linf=Linf,Kl=k,Bl=Bl,method=9)[1]
      CnW_M_VBGF<-Chen_N_Wat_M(Amax,Amat,k,t0)
      CnW_M_a_VBGF<-Chen_N_Wat_M(Amax,Amat,k,t0,out.type = 0)
      maxage<-Amax}
    }
    
    if(!is.na(maxage)){
      CnW_M_a_VBGF_table<-cbind(c(1:maxage),CnW_M_a_VBGF)
      colnames(CnW_M_a_VBGF_table)<-c("Age","M")
    }
    
    if(!(anyNA(c(input$k,input$Amat)))) {
      Roff_M<-M.empirical(Kl=input$k,tm=input$Amat,method=5)[1]
      Jensen_M_Amat<-Jensen_M_amat(input$Amat)
      Rikhter_Efanov_Amat<-Rikhter_Efanov_Amat_M(input$Amat)
    } else {
      if(!(anyNA(c(k,Amat)))) {
        Roff_M<-M.empirical(Kl=k,tm=Amat,method=5)[1]
        Jensen_M_Amat<-Jensen_M_amat(Amat)
        Rikhter_Efanov_Amat<-Rikhter_Efanov_Amat_M(Amat)
      }
    }
    if(!(anyNA(c(input$Linf,input$k,input$Temp)))) {
      Pauly80lt_M<-M.empirical(Linf=input$Linf,Kl=input$k,T=input$Temp,method=1)[1]
    } else {
      if(!(anyNA(c(Linf,k,Temp)))){Pauly80lt_M<-M.empirical(Linf=Linf,Kl=k,T=Temp,method=1)[1]}
    }
    User_M<-input$User_M
    M_vals_all<-c(Then_M_Amax,AnC75_M,Then_M_VBGF,Jensen_M_VBGF,Pauly80lt_M,Gislason_M,CnW_M_VBGF,Roff_M,Jensen_M_Amat,Rikhter_Efanov_Amat,User_M) #Pauly80wt_M,PnW_M,Lorenzen96_M,GnD_GSI_M,
    output$downloadCW_M_a <- downloadHandler(
      filename = function() {paste0("CW_M_a_values", '.csv') },
      content = function(file) {write.csv(CnW_M_a_VBGF_table, file=file)}
    )  
    M_vals_all
  })
  output$Mplot <- renderPlot({
    input$nm_sub_fb
    M_vals_all<-M_vals_all()
    M_methods<-c("Then_Amax 1","Then_Amax 2","Then_Amax 3","Hamel_Amax","AnC","Then_VBGF","Jensen_VBGF 1","Jensen_VBGF 2","Pauly_lt","Gislason","Chen-Wat","Roff","Jensen_Amat","Ri_Ef_Amat","User input") #"Pauly_wt","PnW","Lorenzen","GSI",
    # plot M
    if(all(is.na(M_vals_all))){ymax<-0.5}
    if(!(all(is.na(M_vals_all)))){ymax<-ceiling((max(M_vals_all,na.rm=TRUE)*1.1*10))/10}
    par(mar=c(8,4,3,6),xpd =TRUE)
    plot(M_vals_all, col = "black",bg=c("blue","blue","blue","blue","green","green","green","green","yellow","yellow","orange","red","red","red","brown"),xlab=" ",ylab="Natural mortality",ylim=c(0,ymax),pch=22,cex=1.5,axes=F) #"black","black","black","purple",
    box()
    axis(1,at=1:length(M_vals_all),labels=M_methods,las=3)
    axis(2)
    legend(x="topright",legend=c("Amax","VBGF","VBGF:Temp","VBGF;Amat","Amat","User input"),pch=22,col="black",pt.bg=c("blue","green","yellow","orange","red","brown"),bty="n",horiz=FALSE,cex=1,inset=c(-0.08,0)) #"Weight","GSI",  #"black","purple",
    M_table<-data.frame(cbind(M_methods,M_vals_all))
    colnames(M_table)<-c("Method","M")
    # if(all(is.na(M_vals()))){return(NULL)}
    output$downloadMs <- downloadHandler(
      filename = function() {paste0("M_values", '.csv') },
      content = function(file) {write.csv(M_table, file=file)}
    )
  })
  # Show the first "n" observations
  output$Mtable <- renderTable({
    input$nm_sub_fb
    SPEC<-SPEC()
    Amax<-SPEC$Amax
    Amat<-SPEC$Amat
    Bl<-SPEC$Bl
    Linf<-SPEC$Linf
    k<-SPEC$k
    t0<-SPEC$t0
    Temp<-SPEC$Temp
    Pauly80lt_M<-Pauly80wt_M<-AnC75_M<-Roff_M<-Gislason_M<-NA #<-Pauly80wt_M<-PnW_M<-Lorenzen96_M<-GnD_GSI_M
    Then_M_Amax<-Then_M(Amax)
    if(!(anyNA(c(k,Amax)))){AnC75_M<-M.empirical(Kl=k,tmax=Amax,method=4)[1]}
    Then_M_VBGF<-Then_VBGF(Linf*10,k)
    Jensen_M_VBGF<-Jensen_M_k(k) 
    if(!(anyNA(c(Linf,k,Bl)))){Gislason_M<-M.empirical(Linf=Linf,Kl=k,Bl=Bl,method=9)[1]}
    CnW_M_VBGF<-Chen_N_Wat_M(Amax,Amat,k,t0)
    if(!(anyNA(c(k,Amat)))){Roff_M<-M.empirical(Kl=k,tm=Amat,method=5)[1]}
    Jensen_M_Amat<-Jensen_M_amat(Amat)
    Rikhter_Efanov_Amat<-Rikhter_Efanov_Amat_M(Amat)
    if(!(anyNA(c(Linf,k,Temp)))){Pauly80lt_M<-M.empirical(Linf=Linf,Kl=k,T=Temp,method=1)[1]}
    M_vals_all<-c(Then_M_Amax,AnC75_M,Then_M_VBGF,Jensen_M_VBGF)
    M_methods<-c("Then_Amax 1","Then_Amax 2","Then_Amax 3","Hamel_Amax","AnC","Then_VBGF","Jensen_VBGF 1","Jensen_VBGF 2")
    M_table<-data.frame(cbind(M_methods,signif(M_vals_all,3)))
    colnames(M_table)<-c("Methods","M")
    M_table
  })
  # Show the first "n" observations
  output$Mtable2 <- renderTable({
    input$nm_sub_fb
    SPEC<-SPEC()
    Amax<-SPEC$Amax
    Amat<-SPEC$Amat
    Bl<-SPEC$Bl
    Linf<-SPEC$Linf
    k<-SPEC$k
    t0<-SPEC$t0
    Temp<-SPEC$Temp
    Pauly80lt_M<-AnC75_M<-Roff_M<-Gislason_M<-NA #<-Pauly80wt_M<-PnW_M<-Lorenzen96_M<-GnD_GSI_M
    Then_M_Amax<-Then_M(Amax)
    if(!(anyNA(c(k,Amax)))){AnC75_M<-M.empirical(Kl=k,tmax=Amax,method=4)[1]}
    Then_M_VBGF<-Then_VBGF(Linf*10,k)
    Jensen_M_VBGF<-Jensen_M_k(k) 
    if(!(anyNA(c(Linf,k,Bl)))){Gislason_M<-M.empirical(Linf=Linf,Kl=k,Bl=Bl,method=9)[1]}
    CnW_M_VBGF<-Chen_N_Wat_M(Amax,Amat,k,t0)
    if(!(anyNA(c(k,Amat)))){Roff_M<-M.empirical(Kl=k,tm=Amat,method=5)[1]}
    Jensen_M_Amat<-Jensen_M_amat(Amat)
    Rikhter_Efanov_Amat<-Rikhter_Efanov_Amat_M(Amat)
    if(!(anyNA(c(Linf,k,Temp)))){Pauly80lt_M<-M.empirical(Linf=Linf,Kl=k,T=Temp,method=1)[1]}
    User_M<-input$User_M
    M_vals_all<-c(Pauly80lt_M,Gislason_M,CnW_M_VBGF,Roff_M,Jensen_M_Amat,Rikhter_Efanov_Amat,User_M) #Pauly80wt_M,PnW_M,Lorenzen96_M,GnD_GSI_M,
    M_methods<-c("Pauly_lt","Gislason","Chen-Wat","Roff","Jensen_Amat","Ri_Ef_Amat","User input") #"Pauly_wt","PnW","Lorenzen","GSI",
    M_table<-data.frame(M_vals_all)
    M_table<-data.frame(cbind(M_methods,signif(M_vals_all,3)))
    colnames(M_table)<-c("Methods","M")
    M_table
  })
  #Plot Composite M
  output$Mcomposite<- renderPlot({    
    input$nm_sub_fb
    if(all(is.na(M_vals_all()))){return(NULL)}
    else{
      M.wts<-c(input$Then_Amax_1,input$Then_Amax_2,input$Then_Amax_3,input$Hamel_Amax,input$AnC,input$Then_VBGF,input$Jensen_VBGF_1,input$Jensen_VBGF_2,input$Pauly_lt,input$Gislason,input$Chen_Wat,input$Roff,input$Jensen_Amat,input$Ri_Ef_Amat,input$UserM) #input$Pauly_wt,input$PnW,input$Lorenzen,input$Gonosoma,
      #remove NAs
      if(any(is.na(M_vals_all()))){
        NA.ind<-attributes(na.omit(M_vals_all()))$na.action
        M.sub<-M_vals_all()[-NA.ind]
        M.wts.sub<-M.wts[-NA.ind]
      }
      else{
        M.sub<-M_vals_all()
        M.wts.sub<-M.wts
      }
      #remove 0 weight
      M.sub.n0<-M.sub[M.wts.sub>0]
      M.wts.sub.n0<-M.wts.sub[M.wts.sub>0]
      M.wts.sub.stand<-M.wts.sub.n0/sum(M.wts.sub.n0)
      M.densum<-density(M.sub.n0,weights=M.wts.sub.stand,from=0,cut=0)
      #Approximate the denisty function
      f<- approxfun(M.densum$x, M.densum$y, yleft=0, yright=0)
      #Standardize densities
      pdf_counts<-round(1000000*(M.densum$y/sum(M.densum$y)))
      #Expand densities to samples
      pdf.samples<-unlist(mapply(rep,M.densum$x,pdf_counts))
      #Calculate the cdf
      cdf.out<-ecdf(pdf.samples)
      #Plot the density function
      M.densum.plot<- data.frame(x = M.densum$x, y = M.densum$y)
      Mcomposite.densityplot<- ggplot(data=M.densum.plot,aes(x,y,fill="blue"))+
        geom_line(col="black")+
        labs(x="Natural Mortality",y="Density")+ 
        geom_area(fill="gray")+ 
        geom_vline(xintercept = quantile(cdf.out,0.5),color="darkblue",size=1.2)
      print(Mcomposite.densityplot)
      output$downloadMcompositedensityplot <- downloadHandler(
        filename = function() { paste0('Mcomposite_densityplot',Sys.time(), '.png')},
        content = function(file) {
          png(file, type='cairo',width=800,height=720)
          print(Mcomposite.densityplot)
          dev.off()},contentType = 'image/png') 
      output$downloadMcompositedist <- downloadHandler(
        filename = function() {  paste0("Mcomposite_samples",Sys.time(),".DMP") },
        content = function(file) {save(pdf.samples,file=file)}) 
    }
  })
}