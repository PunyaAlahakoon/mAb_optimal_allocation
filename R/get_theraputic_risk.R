
# variant number anc,ba1,ba1_1,ba2,ba45 = 1, 2, 3, 4, 5

get_theraputic_risk<-function(variant_number,times,max_eff){
  n=length(times) #the length of the points
  j=variant_number

  e_exp_inf=matrix(data=NA, nrow = n,ncol=3)
  e_hos=matrix(NA, nrow = n,ncol=3)

  ab0=c(36.3,108.1,23.9) #Adintrevimab, Casirivimab/imdevimab, Tixagevimab/cilgavimab
  half_t=c(134.7,28.8,94.9)

  #all ic50s

  #Adintrevimab, Casirisvimab/imdevimab, Tixagevimab/cilgavimab order
  ic50=matrix(NA,nrow = 5,ncol=3) #variant=raw, column= antibody type
  ic50[1,]=c(0.00655, 0.00326,0.004) #ancestral
  ic50[2,]=c(1.360, 10,0.262) #ba1
  ic50[3,]=c(0.702, 10, 1.64) #ba11
  ic50[4,]=c(10, 2.530,0.0384) #ba2
  ic50[5,]=c(10, 4.510, 0.727) #ba45

  conv=	561



  for (i in 1:3) {

    #times=seq(0,2000,length.out = n)

    #get the abs in mg/L:
    dose_m=ab_vs_time_curve2(times,ab0[i],half_t[i])

    #plot(times,dose_m)


    #covert all dose mg/L to get the times to dose fold convalscent
    fact=ic50[j,i]*conv

    dose_f=dose_m/fact # fold convalscent scale

    #efficacy from infection to hospitalisation

    e_t=get_theraputic_hos_curve(dose_f,max_eff) #efficacy for theraputic

    #risk from exposure to infections
    #dose_i=dose_m/fact #fold IC50 inputs

   # e_p=get_prophy_inf_curve(dose_f) #efficacy
    #e_exp_inf[,i]=1-e_p #risk

    #risk of hospitalisation:

    #e_h=((1-e_p)*(1-e_t))
    e_hos[,i]=1-e_t #risk from infection to hospitalisation

  }



  #plot(times,e_p)

  #plot(times,e_h)
  colnames(e_hos)=c("Adintrevimab", "Casirivimab/imdevimab", "Tixagevimab/cilgavimab")


  e_hos=data.frame(e_hos,"time"=times,"dose"=dose_f)

  e_hos


}



