

#pop size
N=1000
drug_number=3


times=seq(0,179,by=1)


others=1


tspan=times
r_b_i=1
r_b_h=1
pp=1
out=get_hos_time_infections(N,tspan,drug_number,r_b_i,r_b_h,pp,others)


#risk from progression to infection to hospitalisation
#risk_hos=out[,4]*out[,5]
risk_hos=out[,4]
efficacy=1-risk_hos

infections=out[,2]
#hist(infections)

#plot(times,infections)
#plot(efficacy,infections)

#Theraputic efficacy fom infection to hospitalisation:
theraputic<-get_theraputic_risk(1,0,max_eff=0.67)
thera_eff<-1-theraputic[[drug_number]]

dat=data.frame(times,efficacy,infections)





p3<-ggplot(data=dat, aes(x=times, y=efficacy*100)) +
  geom_line(linewidth=2)+
  geom_hline(aes(yintercept = thera_eff*100), colour="red", linetype = "longdash") +
 # geom_text(aes(x=thera_eff*100+5,y=68,label="Efficacy of theraputic treatemnt"))+
  ylim(60,95)+
 # xlim(0,200)+
  ylab("Efficacy \n (progression from infection to hospitalisation)")+
  xlab("Time (days)")+
  theme_clean(12)+
  theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(angle = -45),
        #  strip.text.x = element_blank(),
        strip.text.y = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.background = element_blank())
p3


#ggsave("fig_B.png",last_plot())



