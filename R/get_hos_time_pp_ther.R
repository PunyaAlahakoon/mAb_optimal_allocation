
#this version: same as get_hos_time_7 version, but add proprtion of people getting prohylactic tratment.
#pp=proprtin of prophylaxix from N pop
#others ===1 or 2, 1==treat others as theraputic if 2=== no treatment at all. baseline hos risk

get_hos_time_pp_ther<-function(N,tspan,drug_number,r_b_i,r_b_h,pp,others){
  i=drug_number
  ancestral=get_risks(1,tspan,prohy_max_hos_eff=0.87,prohy_max_inf_eff=0.9443)
  rr1 =ancestral[[1]] #risk of infection
  r1=rr1[,i] #risk of infection
  #head(e_exp_inf_ancestral)
  rr2=ancestral[[2]]  #risk from infection to hospitalisation
  r2=rr2[,i] #risk from infection to hospitalisation
  
  #t=1
  nt=length(tspan)
  # s_t=N   #initial pops who are all treated as prophylaxis
  
  
  #theraputic treatment:
  #get the therapeutic risk
  ther=get_theraputic_risk(1,0,0.67)
  rrt=ther[[i]]  #risk from infection to hospitalisation
  
  
  
  #risk of hospitalisation for others
  if (others==1){
    #get the therapeutic risk
    ot_r_h=rrt*r_b_h
  }else {
    #get the naseline hospitalisation risk
    ot_r_h=r_b_h
  }
  
  
  
  s_p=c() #prohylaxis pop
  #s_io=c() #other pop
  s_p[1]=N*pp
  s_io=N*(1-pp)
  
  proh_inf=c()
  
  
  hh_p=c()
  r_t=1-exp(1/nt*log(1-r_b_i))
  #also equivalent to 1-((1-r_b_i)^(1/nt))
  
  
  for (t in 1:nt) {
    
    #prophylaxis infections
    i_p=r_t*r1[t]*s_p[t] #infections
    # i_p=r_t*s_p[t] #infections
    
    proh_inf[t]=i_p #store infections
    
    #other infections
    # i_o=r_t*s_io[t]
    
    #hospitalisations:
    r_h_p=min(r2[t],rrt)
    
    #hospitalisations:
    h_p=i_p*r_h_p*r_b_h  #hosp prophylaxis
    
    
    hh_p[t]=h_p
    s_p[t+1]=s_p[t]-(i_p)
    
  }
  
  #other infections:
  oth_inf=r_b_i*s_io
  h_o=oth_inf*ot_r_h
  #other hospitalisations:
  
  hb=(N*r_b_i)*r_b_h #baseline
  #a_h_t=hb-sum(hh_p)
  
  #all hospitalisations:
  all_hos=sum(hh_p)+h_o
  
  avoided_hos=hb-all_hos
  
  #output the vector of avoided hospitalisations, number of prohylaxis infections, number of other infections:
  output=c(avoided_hos,sum(proh_inf),oth_inf)
  #a_h_t=sum(hh_p)
  output
}
