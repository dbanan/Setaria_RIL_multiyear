#Environmental_data_summary 


#summarize seasonal weather averages for each environment 


library(plyr)
library(ggplot2)
library(reshape2)


load("./results/weather_data_all.Rdata")


#make data long 
weatherl<-melt(weather_all, id.vars=c("environment", "DAS"), 
               measure.vars=c("isws_Tmin","isws_Tmax","applied_mm","PPET","isws_RHavg","day_night_ratio"), 
               variable.name="variable", value.name="data")

#environment averages 
envavg<-ddply(weatherl, c("environment","variable"), summarise, 
              avg=mean(data), stderr = sd(data) / sqrt((length(data))), total=sum(data))


#boxplot

png("./results/envAvg_boxplot.png")
ggplot(envavg, aes(x=environment, y=avg, fill=environment))+
  geom_bar(stat="identity")+
  facet_wrap(~variable, scales="free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_errorbar(aes(ymin=avg-stderr, ymax=avg+stderr), width=.2,
                position=position_dodge(.9)) 
dev.off()






