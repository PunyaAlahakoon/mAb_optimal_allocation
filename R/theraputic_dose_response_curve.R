
#protection from progression from infection to hospitalisation
theraputic_dose_response_curve <- function(d,par){
  par[3]/(1+exp(-exp(par[2])*(log10(d)-log10(exp(par[1])))))}
