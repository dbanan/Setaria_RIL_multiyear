#Setaria_soil_moisture_fetch


library(reshape2)
library(ggplot2)
library(plyr)

library(tidyr) #separate function


#load soil moisture data from RIL experiments 


sm15dr<-read.csv("./data/soil_moisture_data/15DR_soilmoisture_SASaverages.csv", header=T, stringsAsFactors=FALSE)
#sm14dr<-read.csv()
sm13dr<-read.csv("./data/soil_moisture_data/13DR_soilmoisture_trt_means.csv", header=T, stringsAsFactors=FALSE)



#format 2015
#make data long 
sm15l<-melt(sm15dr, id.vars=c("DOY","Hour"), variable.name="probe",value.name="data")

#data numeric 
sm15l$data<-as.numeric(sm15l$data)

#break "probe" string into useful information (treatment, depth)
sm15l<-separate(sm15l, probe, into=c("treatment","depth","probe"), sep="_")

#convert data into percent 
sm15l$data<-sm15l$data*100

#take averages (day*treatment*depth)
sm15m<-ddply(sm15l, c("DOY","treatment","depth"), summarise, data=mean(data, na.rm=TRUE))

#wide by depth 
sm15w<-dcast(sm15m, treatment+DOY~depth, value.var="data")

#change colnames
colnames(sm15w)[3]<-"middle"
colnames(sm15w)[4]<-"bottom"
colnames(sm15w)[5]<-"top"

#make environment column 
sm15w$environment[sm15w$treatment=="WW"]<-"wet_2015"
sm15w$environment[sm15w$treatment=="D"]<-"dry_2015"

#DOY to DAS
sm15w$sowing<-187
sm15w$DAS<-sm15w$DOY-sm15w$sowing



#format 2013 
sm13dr<-sm13dr[,c(2,3,16,17,18)]
sm13dr$sowing<-188
sm13dr$DAS<-sm13dr$DOY-sm13dr$sowing
sm13dr$environment[sm13dr$treatment=="dry"]<-"dry_2013"
sm13dr$environment[sm13dr$treatment=="wet"]<-"wet_2013"





#soil moisture stack
sm_all<-rbind(sm13dr, sm15w)

sm_all<-sm_all[,c(8,7,3,4,5)]



#need daily resolution to match other environmental variables 
#do we need to predict values? 
#or is it enough to do a running average? 

#try a running average and plot it




####PRODUCT####
save(sm_all, file="./results/sm_data.Rdata")

















