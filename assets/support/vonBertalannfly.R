GVBGF<-function(Linf,k,t0, ages)
{
  Lengths_exp<-Linf*(1-exp(-k*(ages-t0)))
  return(Lengths_exp)
}