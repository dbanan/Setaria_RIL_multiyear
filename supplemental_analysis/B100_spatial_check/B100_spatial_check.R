#B100_spatial_check
#5/6/18
#peek at B100 check plot performance (harvest culm height) to see if there are major spatial trends to account for 


library(ggplot2)



#requires height data (all_stack)


#get plot and spot (last few digits of subplot_id) info for 2013, 2014
all_1314<-subset(all_stack1, year %in% c(2013, 2014))
all_1314$plot<-substr(all_1314$subplot_id, 1,2)
all_1314$spot<-substr(all_1314$subplot_id, 3,5)

#and for 2015
all_15<-subset(all_stack1, year %in% 2015)
all_15$plot<-substr(all_15$subplot_id, 2,3)  
all_15$spot<-substr(all_15$subplot_id, 4,5)

#stack back together 
all_plot<-rbind(all_1314, all_15)
all_plot$plot<-as.numeric(all_plot$plot)
all_plot$spot<-as.numeric(all_plot$spot)


b100_plot<-subset(all_plot, genotype %in% "B100")
b100_plot<-subset(b100_plot, trait %in% "culm_height")

#output as single pdf 
pdf("/rsync/box/Setaria/2018 May data meeting/spatial checks/spatial_plots.pdf")

#culm height x awning boxplots for all experiment 
ggplot(data=b100_plot, aes(x=factor(plot), y=data, group=plot))+
  geom_boxplot()+
  labs(x="awning", y="harvest culm height (cm)", title="B100 check plots")+
  facet_wrap(~year+experiment, scales="free_x")

#culm height x subplot_id for density experiments
ggplot(data=subset(b100_plot, experiment %in% "density"), aes(x=spot, y=data))+
  geom_point()+
  labs(x="subplot_id last digits", y="harvest culm height (cm)", title="B100 density experiments")+
  facet_wrap(~year+plot)

ggplot(data=subset(b100_plot, year %in% 2013 & experiment %in% "drought"), aes(x=spot, y=data))+
  geom_point(aes(color=treatment))+
  labs(x="subplot_id last digits", y="harvest culm height (cm)", title="B100 drought 2013")+
  facet_wrap(~plot)

ggplot(data=subset(b100_plot, year %in% 2014 & experiment %in% "drought"), aes(x=spot, y=data))+
  geom_point(aes(color=treatment))+
  labs(x="subplot_id last digits", y="harvest culm height (cm)", title="B100 drought 2014")+
  facet_wrap(~plot)

ggplot(data=subset(b100_plot, year %in% 2015 & experiment %in% "drought"), aes(x=spot, y=data))+
  geom_point(aes(color=treatment))+
  labs(x="subplot_id last digits", y="harvest culm height (cm)", title="B100 drought 2015")+
  facet_wrap(~plot)

dev.off()

#output png individually 
B100_check<-ggplot(data=b100_plot, aes(x=factor(plot), y=data, group=plot))+
  geom_boxplot()+
  labs(x="awning", y="harvest culm height (cm)", title="B100 check plots")+
  facet_wrap(~year+experiment, scales="free_x")
B100_check

B100_density<-ggplot(data=subset(b100_plot, experiment %in% "density"), aes(x=spot, y=data))+
  geom_point()+
  labs(x="subplot_id last digits", y="harvest culm height (cm)", title="B100 density experiments")+
  facet_wrap(~year+plot)
B100_density 

B100_13DR<-ggplot(data=subset(b100_plot, year %in% 2013 & experiment %in% "drought"), aes(x=spot, y=data))+
  geom_point(aes(color=treatment))+
  labs(x="subplot_id last digits", y="harvest culm height (cm)", title="B100 drought 2013")+
  facet_wrap(~plot)
B100_13DR

B100_14DR<-ggplot(data=subset(b100_plot, year %in% 2014 & experiment %in% "drought"), aes(x=spot, y=data))+
  geom_point(aes(color=treatment))+
  labs(x="subplot_id last digits", y="harvest culm height (cm)", title="B100 drought 2014")+
  facet_wrap(~plot)
B100_14DR

B100_15DR<-ggplot(data=subset(b100_plot, year %in% 2015 & experiment %in% "drought"), aes(x=spot, y=data))+
  geom_point(aes(color=treatment))+
  labs(x="subplot_id last digits", y="harvest culm height (cm)", title="B100 drought 2015")+
  facet_wrap(~plot)
B100_15DR

png("./spatial checks/B100_check.png")
B100_check
dev.off()

png("./spatial checks/B100_density.png")
B100_density
dev.off()

png("./spatial checks/B100_13DR.png")
B100_13DR
dev.off()

png("./spatial checks/B100_14DR.png")
B100_14DR
dev.off()

png("./spatial checks/B100_15DR.png")
B100_15DR
dev.off()



