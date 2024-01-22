#protection from progression to pre-exposure to infection

#maximum (log-transformed), slope (log-transformed), IC50 (log-transformed):

prophy_dose_response_curve<- function(d,par){
  exp(par[1])/(1+(2*exp(par[1])-1)*exp(-exp(par[2])*(log10(d)-log10(exp(par[3])))))
  }


