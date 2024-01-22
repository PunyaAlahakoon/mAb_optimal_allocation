#get the the Ab vs time curve:
#inputs: Antibody level (ab) and time

ab_vs_time_curve2<-function(t,ab0,half_t){
  #transpose ab in fold conv to mg/L:


  par=c(ab0, log(2)/half_t)

  #assumining exponential decay for Ab(t) vs t, Ab(t)=ab0 exp(-lambda t)
  #lambda= 2*half_t

  ab=par[1]*exp(-par[2]*t)
  #ab=log(par[1])-(par[2]*t)
  ab

}
