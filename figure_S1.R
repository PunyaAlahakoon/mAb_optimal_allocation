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

dose=seq(0.1,10000,by=0.1)

e_t=get_prophy_hos_curve(dose,0.87)
e_p=get_prophy_inf_curve(dose,0.9443)

e_h=1-((1-e_p)*(1-e_t))

all_dat<-data.frame("Dose"=dose,"Hospitalisation"=e_h*100,"Pre_exposure"=e_p*100,"Infectious"=e_t*100)
all_dat=melt(all_dat,id="Dose")


p1=ggplot(data=all_dat,aes(x=Dose,y=value,group=variable,color=variable))+
  geom_line(linewidth=1)+
  scale_x_continuous(limits = c(0.1,10000),trans='log10')+
  scale_color_brewer(palette="Dark2",labels=c("Protection from hospitalisation \n with prophylaxis","Protection from infection \n with prophylaxis","Protection from infection to \n hospitalisation \n with prophylaxis"))+
  ylab("Efficacy (%)")+
  xlab("Dose administrated (fold convalascent)")+
  theme_minimal(14)+
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(colour = "black"),
        # axis.title.y=element_blank(),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")  +
  theme(legend.title=element_blank())
p1


#ggsave('fig_s1.png',last_plot())
