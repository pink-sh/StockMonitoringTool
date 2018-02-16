GVBGF<-function(Linf,k,t0,b,d,ages)
{
  Lengths_exp<-Linf*(1-exp(-k*b*(1-d)*(ages-t0)))^(1/(b*(1-d)))
  return(Lengths_exp)
}