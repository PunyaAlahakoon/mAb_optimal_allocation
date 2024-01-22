

library(reshape2)
library(ggplot2)
library(targets)library(data.table)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(dplyr)
library(tidyr)
library(viridis)

# Run the R scripts in the R/ folder with your custom functions:
. <- lapply(list.files("R", full.names = TRUE), source)

N=1000

drug_number= 3


r_bi=seq(0,1,by=0.1)

#risk of hospitalisations
r_bh=seq(0,1,by=0.1)

risks=expand.grid(r_bi,r_bh)
q=nrow(risks)

overall_risk=risks[,1]*risks[,2]
lr=length(overall_risk)

#available proprtion of doses out of the total population:
av_doses=seq(0.1,1,by=0.1)*N
#av_doses=0.5*N
m=length(av_doses)
#proportion of people that get the treatment
pp=seq(0,1,by=0.1) #prohylaxis proprtion out of total availble doses 


n=length(pp)

comb=expand.grid(av_doses,pp,r_bi,r_bh)

dat=data.frame("availble_doses"=comb[,1] , "prop_prohylaxis"=comb[,2],"risk_infection"=comb[,3],
               "risk_hos"=comb[,4],"avoided_hos"=NA,"theraputic_doses"=NA,"prohy_inf"=NA, "other_inf"=NA,
               "total_inf"=NA,"total_doses"=NA)


nq=nrow(dat)
tspan=seq(0,179,by=1)

dat_hos=matrix(NA,nrow = 0,ncol = 4)

for (i in 1:nq) {
  r_b_i=dat$risk_infection[i]
  r_b_h=dat$risk_hos[i]
  p_p=dat$prop_prohylaxis[i]
  av_d=dat$availble_doses[i]
  out=get_hos_time_pp_ther_prohy2(N,tspan,drug_number,r_b_i,r_b_h,av_d,p_p,0.87,0.9443,0.67)
  dat_hos=rbind(dat_hos,out)
  
}

dat[,5:8]=dat_hos

dat$total_inf=dat$prohy_inf+dat$other_inf
dat$total_doses=((dat$prop_prohylaxis)*dat$availble_doses)+dat$theraputic_doses

dat$overall_risk=rep(overall_risk)
dat$hos_per_dose=dat$avoided_hos/dat$total_doses


new_dat=data.frame("availble_doses"=rep(av_doses,each=q),"risk_infection"=rep(risks[,1]),
                   "risk_hos"=rep(risks[,2]),"optimal_prohylaxis_dose"=NA,"max_avoided_hos_per_dose"=NA,"total_theraputic_administered"=NA,
                   "hos_avoded"=NA)

new_ln=nrow(new_dat)


#subset each risk groups:
for (j in 1:new_ln) {
  risk_dat=subset(dat,risk_infection==new_dat$risk_infection[j] & risk_hos==new_dat$risk_hos[j] & availble_doses==new_dat$availble_doses[j])
  #find which proprtion of that risk group gives the max avoided hos per dose
  navd=new_dat$availble_doses[j] #availble doses 
  
  ind_pro=which.max(risk_dat$hos_per_dose)
  if(length(ind_pro)>0){
    pro=risk_dat$prop_prohylaxis[ind_pro]
    
    new_dat$optimal_prohylaxis_dose[j]=pro
    #which is the maxmimum avoided hos per doses
    m_hos=risk_dat$hos_per_dose[ind_pro]
    new_dat$max_avoided_hos_per_dose[j]=m_hos
    #ttal doses administered out of availble doses 
    
    m_ad_doses=risk_dat$theraputic_doses[ind_pro]
    new_dat$total_theraputic_administered[j]=m_ad_doses/navd
    
    new_dat$hos_avoded[j]=risk_dat$avoided_hos[ind_pro]
  }
  
}


new_dat$availble_doses=new_dat$availble_doses/N

sub_dat=subset(new_dat,availble_doses==0.5)

ind_x=min(which(sub_dat$optimal_prohylaxis_dose==1))
xx=sub_dat$risk_infection[ind_x]

new_dat$availble_doses=factor(new_dat$availble_doses,levels = paste0(seq(0.1,1,by=0.1)))
availble_doses.labs=paste0("Proportion of available doses: ", seq(0.1:1,by=0.1))
names(availble_doses.labs)=paste0(seq(0.1:1,by=0.1))


p1<-ggplot(data=new_dat,aes(x=risk_infection,y=risk_hos,fill=max_avoided_hos_per_dose))+
  geom_tile()+
  scale_fill_viridis()+
  geom_vline(data=NULL,xintercept = xx,linetype="twodash")+
  facet_wrap(~availble_doses,ncol = 5,labeller = labeller(availble_doses=availble_doses.labs))+
  # geom_text(aes(label=optimal_prohylaxis_dose),size=1.7)+
  ylab("Risk of hospitalisation")+
  xlab("Risk of infection")+
  theme_minimal(14)+
  theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(angle = -45),
        #  strip.text.x = element_blank(),
        strip.text.y = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.background = element_blank())+
  theme(legend.position="bottom")+
  labs(fill="Maximum hospitalisations avoided per dose")

p1


ggsave('fig_s5.png',last_plot())

sub_dat=subset(new_dat,availble_doses==0.5)

ind_x=min(which(sub_dat$optimal_prohylaxis_dose==1))
xx=sub_dat$risk_infection[ind_x]




#ggsave("compre_dose_500.png",last_plot())

d5_sub=subset(dat,availble_doses==500 & risk_infection>0 & risk_hos>0)
d5_sub$risk_hos=factor(d5_sub$risk_hos,levels = paste0(seq(0.1:1,by=0.1)))
risk_hos.labs=paste0("Risk of hospitalisation: ", seq(0.1:1,by=0.1))
names(risk_hos.labs)=paste0(seq(0.1:1,by=0.1))
#d5_sub=subset(dat,availble_doses==1000)
#<-grid.arrange(pl1,pl2,ncol=2)

p2<-ggplot(d5_sub,aes(x=prop_prohylaxis,y=hos_per_dose,color=as.factor(risk_infection)))+
  geom_line()+
  # scale_color_brewer(palette = "RdYlBu")+
  facet_wrap(~risk_hos,ncol=5,labeller =  labeller(risk_hos=risk_hos.labs))+
  xlab("Proportion of prohylaxis doses out of availble doses")+
  ylab("Hospitalisations avoided per dose")+
  theme_minimal()+
  theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(angle = -45),
        #  strip.text.x = element_blank(),
        strip.text.y = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.background = element_blank())+
  theme(legend.position="bottom")+
  ggtitle("Proportion of availble doses out of the total population = 0.5")+
  labs(color="Risk of infection")
p2

ggsave("fig_s6.png",last_plot(),height = 8,width = 12)




