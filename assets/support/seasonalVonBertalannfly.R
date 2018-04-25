soGVBGF<-function(Linf,k,t0, ages, C, ts)
{
  Lengths_exp<-Linf*(1-exp(-k*(ages-t0)-((C*k)/(2*pi))*sin(2*pi*(ages-ts))))
  return(Lengths_exp)
}