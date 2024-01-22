
#this version: same as get_hos_time_7 version, but add proprtion of people getting prohylactic tratment.
#also a different proprtion of peeople also get theraputic

#pp=proprtin of prophylaxix from availble doses pop
#av_doses= total avialble doses 

#others ===1 or 2, 1==treat others as theraputic if 2=== no treatment at all. baseline hos risk

get_hos_time_pp_ther_prohy2<-function(N,tspan,drug_number,r_b_i,r_b_h,av_doses,pp,prohy_max_hos_eff,prohy_max_inf_eff,ther_max_eff){
  i=drug_number
  ancestral=get_risks(1,tspan,prohy_max_hos_eff,prohy_max_inf_eff)
  rr1 =ancestral[[1]] #risk of infection
  r1=rr1[,i] #risk of infection
  #head(e_exp_inf_ancestral)
  rr2=ancestral[[2]]  #risk from infection to hospitalisation
  r2=rr2[,i]*r_b_h #risk from infection to hospitalisation
  
  #t=1
  nt=length(tspan)
  # s_t=N   #initial pops who are all treated as prophylaxis
  
  
  
  
  
  #risk of hospitalisation for others
  
  #get the therapeutic risk
  ther=get_theraputic_risk(1,0,ther_max_eff) #variant_number,times
  rrt=ther[[i]]  #risk from infection to hospitalisation i is the antibody type
  ot_r_h=rrt*r_b_h
  
  
  #avaible prohylaxis doses:
  n_prohy_doses=av_doses*pp
  
  #availble theraputic doses:
  n_the_doses=av_doses-n_prohy_doses
  
  s_p=c() #prohylaxis pop
  #s_io=c() 
  s_p[1]=n_prohy_doses
  s_io=N-n_prohy_doses #other pop
  

  
  proh_inf=c()
  
  
  hh_p=c()
  #r_t=-log(1-r_b_i)/nt
  r_t=1-exp(1/nt*log(1-r_b_i))
  
  for (t in 1:nt) {
    
    #prophylaxis infections
    i_p=r_t*r1[t]*s_p[t] #infections
    proh_inf[t]=i_p #store infections
    
    #other infections
    # i_o=r_t*s_io[t]
    
    
    #hospitalisations:
    h_p=i_p*r2[t]   #hosp prophylaxis
    
    
    hh_p[t]=h_p
    s_p[t+1]=s_p[t]-(i_p)
    
  }
  
  #other theraputic infections:
  oth_inf=r_b_i*s_io #all the other infcetions that can get either theraputic or no treatment 
  

  
  #out of them, who can get the theraputicet theraputic
  if(oth_inf<=n_the_doses){ #if the availble theraputic doses are less than or equal to infections, all get the treatment
    h_t=oth_inf*ot_r_h
    #theraputic doses:
    ther_dos=oth_inf #treat everyone who is infected
    h_o=0  #everyone else do not get any theraputic treatment
  }else{ # people get treated as much as there are doses, other infections don't get treated 
    h_t=n_the_doses*ot_r_h #people getting therautic teatment 
    h_o=(oth_inf-n_the_doses)*r_b_h  #people not getting any treatment 
    ther_dos=n_the_doses #treat as much as you can depending on the doses you have 
  }
  
  
 # h_t=oth_inf*pt*ot_r_h
  
  #everyone else do not get any theraputic treatment
  #h_o=oth_inf*(1-pt)*r_b_h #they are similar to baseline risk
  
  #other hospitalisations:
  
  hb=(N*r_b_i)*r_b_h #baseline
  #a_h_t=hb-sum(hh_p)
  
  #all hospitalisations:
  all_hos=sum(hh_p)+h_t+h_o
  
  avoided_hos=hb-all_hos
  

  
  #output the vector of avoided hospitalisations, number of prohylaxis infections, number of other infections:
  output=c(avoided_hos,ther_dos,sum(proh_inf),oth_inf)
  #a_h_t=sum(hh_p)
  output
}
