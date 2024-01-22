#load the data

#half-maximal dose (EC50, log-transformed), slope (log-transformed), and maximal efficacy
#protection from progression from infection to hospitalisation

get_prophy_hos_curve<-function(dose,max_eff){
#dose=seq(0,1000,by=0.2)
#par=c(-1.376157,1.190623,0.87)
par=c(-1.376157,1.190623,max_eff)
#  par=c(-1.376157,1.190623,0.25)
efficacy=theraputic_dose_response_curve(dose,par)
efficacy

}
