Then_M<-function(Amax)
{
  M_vals<-rep(NA,4)
  M_vals[1]<-4.889*Amax^-0.916
  M_vals[2]<-5.109/Amax
  M_vals[3]<-exp(1.717-1.01*log(Amax))
  M_vals[4]<-5.4/Amax
  return(M_vals)
}

Then_VBGF<-function(Linf,k)
{
  M_val_vbgf<-4.11*k^0.73*Linf^-0.33
  return(M_val_vbgf)
}

Jensen_M_amat<-function(Amat)
{
  M_val_Jensen<-1.65/Amat
  return(M_val_Jensen)
}

Jensen_M_k<-function(k)
{
  M_val_Jensen_k<-k*c(1.5,1.6)
  return(M_val_Jensen_k)
}

#Rikhter & Efanov

Rikhter_Efanov_Amat_M<-function(Amat)
{
  M_val_RiEf<-(1.52/Amat^0.72)-0.16
  return(M_val_RiEf)
}


Chen_N_Wat_M<-function(Amax,Amat,k,t0,out.type=1)
{
  if(anyNA(c(Amax,k,t0))){M.out<-NA}
  else
  {
    M_ages<-rep(NA,length(c(1:Amax)))
    tM<--1/k*(log(abs(1-exp(k*t0)))+t0)
    a0<-1-exp(-k*(tM-t0))
    a1<-k*exp(-k*(tM-t0))
    a2<--0.5*k^2*exp(-k*(tM-t0))
    for(a in 1:Amax)
    {
      if(a<=tM){M_ages[a]<-k/(1-exp(-k*(a-t0)))}
      if(a>tM){M_ages[a]<-k/(a0+a1*(a-tM)+a2*(a-tM)^2)}
    }
    if(out.type==1){M.out<-mean(M_ages)}
    else M.out<-M_ages
  }
  return(M.out)
}