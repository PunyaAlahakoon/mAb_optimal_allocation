
#this version: gives number of infections at each time

get_hos_time_infections<-function(N,tspan,drug_number,r_b_i,r_b_h,pp,others){
  i=drug_number
  ancestral=get_risks(1,tspan,prohy_max_hos_eff=0.87,prohy_max_inf_eff=0.9443)
  rr1 =ancestral[[1]] #risk of infection
  r1=rr1[,i] #risk of infection
  #head(e_exp_inf_ancestral)
  rr2=ancestral[[2]]  #risk from infection to hospitalisation
  r2=rr2[,i]*r_b_h #risk from infection to hospitalisation

  #t=1
  nt=length(tspan)
  # s_t=N   #initial pops who are all treated as prophylaxis





  #risk of hospitalisation for others
  if (others==1){
    #get the therapeutic risk
    ther=get_theraputic_risk(1,0,max_eff=0.67) #variant_number,times
    rrt=ther[[i]]  #risk from infection to hospitalisation i is the antibody type
    ot_r_h=rrt*r_b_h
  }else {
    #get the naseline hospitalisation risk
    ot_r_h=r_b_h
  }



  s_p=c() #prohylaxis pop

  s_p[1]=N*pp
  s_io=N*(1-pp)

  proh_inf=c() #prohylaxis infections 


  hh_p=c()
  r_t=1-exp(1/nt*log(1-r_b_i)) #daily rate of infection 


  for (t in 1:nt) {

    #prophylaxis infections
    i_p=r_t*r1[t]*s_p[t] #infections
    proh_inf[t]=i_p #store infections



    #hospitalisations:
    h_p=i_p*r2[t]   #hosp prophylaxis


    hh_p[t]=h_p
    s_p[t+1]=s_p[t]-(i_p)

  }

  #other infections:
  oth_inf=r_b_i*s_io #the proportion who will not get prohylaxis doses 
  h_o=oth_inf*ot_r_h

  all_hos=sum(hh_p)+h_o
  #hb=(N*r_b_i)*r_b_h #baseline
 # a_h_t=hb-sum(hh_p)

  #output the vector of hospitalisations, number of prohylaxis infections, number of other infections:
  output=matrix(NA, nrow = nt,ncol=5)
  output[,1]=all_hos #total hospitalisations
  output[,2]=proh_inf  #prohylaxis infections
  output[,3]=oth_inf #other infections
  output[,4]=r1 #risk of infection for prohylaxis
  output[,5]=rr2[,i] #risk from infection to hospitalisation
  #a_h_t=sum(hh_p)
  output
}
