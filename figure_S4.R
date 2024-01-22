

library(reshape2)
library(ggplot2)
library(data.table)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(dplyr)
library(tidyr)
library(gridExtra)
library(metR)
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

dat=data.frame("risk_infection"=risks[,1],"risk_hos"=risks[,2],"avoided_hos"=NA,"doses"=NA,"hos_per_dose"=NA,"method"=rep("Theraputic only"))

hos_dat=matrix(NA,ncol = 2,nrow = 0)

#scenarios 1; All infected people get therapeutic:
for (i in 1:q) {
  r_b_i=risks[i,1]
  r_b_h=risks[i,2]
  out_1=get_hos_theraputic2(N,drug_number,r_b_i,r_b_h)
  hos_dat=rbind(hos_dat,out_1)
}

dat[,3:4]=hos_dat

dat$hos_per_dose=dat$avoided_hos/dat$doses

#scenarios 2: everyone in the population get prohylaxis:
dat2=data.frame("risk_infection"=risks[,1],"risk_hos"=risks[,2],"avoided_hos"=NA,"doses"=NA,"hos_per_dose"=NA,"method"=rep("Prophylaxis only"))

hos_dat2=matrix(NA,ncol = 2,nrow = 0)
tspan=seq(0,179,by=1)
#tspan=0
pp=1 #treat total pop as prohylaxis
others=1 # 1 0r 2

for (i in 1:q) {
  r_b_i=risks[i,1]
  r_b_h=risks[i,2]
  out_2=get_hos_time_pp_ther(N,tspan,drug_number,r_b_i,r_b_h,pp,others)
  #out_2=get_hos_no_deacy2(N,drug_number,r_b_i,r_b_h,pp,others)
  hos_dat2=rbind(hos_dat2,out_2[1:2])
}

dat2[,3:4]=hos_dat2

dat2$doses=dat2$doses+N #all get prohylaxis doses 

dat2$hos_per_dose=dat2$avoided_hos/(dat2$doses)

dat3=data.frame("risk_infection"=risks[,1],"risk_hos"=risks[,2],"avoided_hos"=NA,"doses"=N,"hos_per_dose"=NA,"method"=rep("Ratio"))
dat3$hos_per_dose=dat2$hos_per_dose/dat$hos_per_dose #prohylaxis/theraputic
dat3$avoided_hos=dat2$avoided_hos/dat$avoided_hos


all_dat=rbind(dat,dat2)

meth_lvs=c("Theraputic only","Prophylaxis only")
all_dat$method=factor(all_dat$method,levels = meth_lvs)

#which risks are better under prohylaxis:
xx=which(dat3$hos_per_dose>=1)
yy=dat3$risk_hos
zz=dat3$risk_infection

dd=data.frame(xx,min(zz[xx]),(yy[xx]))
colnames(dd)<-c("index","risk_infection","risk_hos")

#which risks have 0.4 hos averted per dose
ind=which(all_dat$hos_per_dose>=0.4)
d_0.4<-data.frame("index"=ind,"risk_infection"=(all_dat$risk_infection[ind]),"risk_hos"=(all_dat$risk_hos[ind]),
                  "method"=all_dat$method[ind],all_dat$hos_per_dose[ind])

d_t=subset(d_0.4,method=="Theraputic only")
d_p=subset(d_0.4,method=="Prophylaxis only")

dd_4=data.frame("risk_infection"=c(min(d_t$risk_infection),min(d_p$risk_infection)),"risk_hos"=c(min(d_t$risk_hos),min(d_p$risk_hos)),
                "method"=c("Theraputic only","Prophylaxis only"))


p1<-ggplot(all_dat,aes(x=risk_infection,y=risk_hos,z=avoided_hos/N))+
  geom_contour()+
  geom_contour_filled()+
  metR::geom_text_contour(aes(z = avoided_hos/N),skip=1,rotate = F,color="black")+
  facet_wrap(~method,ncol =2)+
  ylab("Risk of hospitalisation")+
  xlab("Risk of infection")+
  theme_minimal(12)+
  theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(angle = -45),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        #  strip.text.x = element_blank(),
        strip.text.y = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.background = element_blank())+
  theme(legend.position="bottom")+
  labs(fill="Proportion of hospitalisations avoided")+ 
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.margin = margin(0.75,0.75, 0.75, 0.75, "cm")) +
  labs( tag = "(A)")
p1




ind_x=min(which(dat3$avoided_hos>=1))
xx=dat3$risk_infection[ind_x]

p2<-ggplot(dat3,aes(x=risk_infection,y=risk_hos,z=avoided_hos))+
  geom_tile(alpha=0.9)+
  geom_contour()+
  geom_contour_filled()+
  metR::geom_text_contour(aes(z = avoided_hos),skip=1,rotate = F,color="black")+
  #geom_vline(xintercept = xx,linetype="twodash")+
  #geom_text(aes(x=xx+0.025,y=0.25,label="Ratio = 1"))+
  ylab("Risk of hospitalisation")+
  xlab("Risk of infection")+
  theme_minimal(12)+
  theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        #  strip.text.x = element_blank(),
        strip.text.y = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.background = element_blank())+
  theme(legend.position="bottom")+
  ggtitle("Ratio")+
  labs(fill="Ratio of hospitalisations \n avoided \n between prohylaxis and theraputic")+
  theme(plot.margin = margin(0.75,0.75, 0.75, 0.75, "cm"))+
  labs( tag = "(B)")
p2


library(cowplot)
plot<-plot_grid(p1,p2,  align = "h", axis = "b",rel_heights = c(1,1),rel_widths = c(2,1),ncol=1)
plot
#plot<-grid.arrange(p1,p2,ncol = 1,heights=c(1,1))
ggsave("fig_s4.png",plot,height = 12,width = 10)

#plot

