
#protection from progression to pre-exposure to infection

get_prophy_inf_curve<-function(dose,max_eff){
  #dose=seq(0,1000,by=0.2)
  #maximum (log-transformed), slope (log-transformed), IC50 (log-transformed):
  #par=c(-0.05729787,0.25335348, 3.99488427)
#  m=0.25
  #m=0.9443
  m=max_eff
  k=1.29
  c50=0.09

  par=c(log(m),log(k),log(c50))

  efficacy=prophy_dose_response_curve(dose,par)
  efficacy

}
