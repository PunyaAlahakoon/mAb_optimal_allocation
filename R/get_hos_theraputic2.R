


get_hos_theraputic2<-function(N,drug_number,r_b_i,r_b_h){
  i=drug_number
  ancestral=get_theraputic_risk(1,0,0.67) #risks at 0th time. initial dose time
  #rr1 =ancestral[[1]] #risk of infection
  r1=r_b_i
  #head(e_exp_inf_ancestral)
  rr2=ancestral[[i]]  #risk from infection to hospitalisation
  r2=rr2*r_b_h
  
  
  s_t=N   #initial pops who are all treated as prophylaxis
  
  i_t=r1*s_t  #infections baseline
  hos_t=r2*i_t # hospitalisations theraputic
  
  #baseline risk
  
  base_hos=r_b_h*i_t
  #avoided hospitalisations:
  a_h_t=base_hos-hos_t  #hospitalisations at baseline - hospitalisations
  #a_h_t=hos_t
  # a_h_t
  #output the vector of avoided hospitalisations, number of other infections:
  output=c(a_h_t,i_t) #no p, and baseline infections
  output
}
