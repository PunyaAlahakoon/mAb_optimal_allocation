
get_theraputic_hos_curve<-function(dose,max_eff){
  #dose=seq(0,1000,by=0.2)
 # par=c(-1.376157,1.190623,0.675)
  par=c(-1.376157,1.190623,max_eff)
  
  efficacy=theraputic_dose_response_curve(dose,par)
  efficacy

}
