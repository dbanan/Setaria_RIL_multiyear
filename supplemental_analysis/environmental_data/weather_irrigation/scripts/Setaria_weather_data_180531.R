#Setaria_weather_data_180531
#5/31/18 

#weather data bring in SoyFACE and ISWS data
#bring in table of irrigation 

#May meeting action item: calculate background and awning P-PET

#6/10/18 migrate data and script to project 
#6/10/18 version control 

#6/12/18 relative path from working directory 



library(ggplot2)
library(reshape2)
library(plyr)
library(gridExtra)
library(grid)


setwd("/de/github/dbanan/auth/Setaria_weather_irrigation")

save.image("./scripts/Setaria_weather_irrigation.Rdata") 
load("./scripts/Setaria_weather_irrigation.Rdata")

######EXPERIMENT EVENTS########
event_13DN<-data.frame(day=as.Date(c("2013-05-03", "2013-05-14", "2013-07-29", "2013-08-13")), 
                       DOY=c(123, 134, 210, 225),
                       event=c("DN sowing", "DN transplanting", "DN harvest start", "DN harvest end"))

event_13DR<-data.frame(day=as.Date(c("2013-07-07", "2013-07-16", "2013-10-07", "2013-10-31")), 
                       DOY=c(188, 197, 280, 304),
                       event=c("DR sowing", "DR transplanting", "DR harvest start", "DR harvest end"))

event_2013<-rbind(event_13DN, event_13DR)

event_14DR<-data.frame(day=as.Date(c("2014-06-08", "2014-06-17", "2014-08-06", "2014-08-25")), 
                       DOY=c(159, 168, 218, 237),
                       event=c("DR sowing", "DR transplanting", "DR harvest start", "DR harvest end"))

event_14DN<-data.frame(day=as.Date(c("2014-07-17", "2014-07-24", "2014-09-22", "2014-10-05")), 
                       DOY=c(198, 205, 265, 278), 
                       event=c("DN sowing", "DN transplanting", "DN harvest start", "DN harvest end"))

event_2014<-rbind(event_14DR, event_14DN)

event_15DR<-data.frame(day=as.Date(c("2015-07-06", "2015-07-15", "2015-08-28", "2015-09-23")), 
                       DOY=c(187, 196, 240, 266),
                       event=c("DR sowing", "DR transplanting", "DR harvest start", "DR harvest end"))

event_16DR<-data.frame(day=as.Date(c("2016-06-11", "2016-06-21", "2016-07-29", "2016-08-24")), 
                       event=c("DR sowing", "DR transplanting", "DR harvest start", "DR harvest end"))


######IRRIGATION RECORDS#######
#2015
irr_15DR<-read.csv("./data/Setaria_2015_irrigation.csv", header=T, stringsAsFactors=FALSE)

#change NA per.awning to 0 
irr_15DR$per.awning[is.na(irr_15DR$per.awning)]<-0

#convert gallons applied to rainfall centimeters and millimeters 
#3785.41 cubic cm in a gallon, 89250 cm^2 is irrigated footprint 2015
irr_15DR$applied_cm<-(irr_15DR$per.awning*3785.41)/89250
irr_15DR$applied_mm<-irr_15DR$applied_cm*10

#calendar column 
irr_15DR$calendar<-as.Date(irr_15DR$date)

#2013
irr_13DR_truck<-read.csv("./data/Setaria_2013_irrigation_truck.csv", header=T, stringsAsFactors=FALSE)
irr_13DR_drip<-read.csv("./data/Setaria_2013_irrigation_drip.csv", header=T, stringsAsFactors=FALSE)

#format truck irrigation  
#break date strings into useable information
irr_13DR_truck$year<-2000+as.numeric(substr(irr_13DR_truck$date, 1,2))
irr_13DR_truck$month<-substr(irr_13DR_truck$date, 3,4)
irr_13DR_truck$day<-substr(irr_13DR_truck$date, 5,6)
#and stitch back together into date format 
irr_13DR_truck$calendar<-as.Date(paste(irr_13DR_truck$year, irr_13DR_truck$month, irr_13DR_truck$day, sep="-"))
#average truck waterings to day rather than awning 
irr_13DR_truck1<-ddply(irr_13DR_truck, c("calendar"), summarise, difference=mean(difference.gal))
colnames(irr_13DR_truck1)[2]<-"applied_gal"

#format drip irrigation
irr_13DR_drip$year<-2000+as.numeric(substr(irr_13DR_drip$date, 1,2))
irr_13DR_drip$month<-substr(irr_13DR_drip$date, 3,4)
irr_13DR_drip$day<-substr(irr_13DR_drip$date, 5,6)
irr_13DR_drip$calendar<-as.Date(paste(irr_13DR_drip$year, irr_13DR_drip$month, irr_13DR_drip$day, sep="-"))
irr_13DR_drip1<-irr_13DR_drip[,c(6,10)]
colnames(irr_13DR_drip1)[1]<-"applied_gal"

#combine truck and drip irrigation 
irr_13DR<-rbind(irr_13DR_truck1, irr_13DR_drip1)

#convert to rainfall centimeters and millimeters 
#3785.41 cubic cm in a gallon, 89250 cm^2 is irrigated footprint 2015 !!!need to double check the irrigated footprint in 2013, we really stuffed the awnings that year
irr_13DR$applied_cm<-(irr_13DR$applied_gal*3785.41)/89250
irr_13DR$applied_mm<-irr_13DR$applied_cm*10

  
  
#2014
#irrigation data is spotty...due to flooding events and treatment reassignments? 
#chase this down later 


#######ISWS data##########
isws<-read.table("./data/CMIDAY.txt",header=T,sep="\t", stringsAsFactors=FALSE)

#ditch last rows with data metadata and first row with units 
#keep columns of interest 
isws1<-isws[-c(1,10548:10557), c("year","month","day","sol_rad","max_air_temp","min_air_temp","avg_air_temp","max_rel_hum","min_rel_hum","avg_rel_hum","avg_dewpt_temp","precip","pot_evapot")]

#format to numeric 
isws2<-sapply(isws1, as.numeric)
isws3<-as.data.frame(isws2)

#convert negative precip to NA 
isws3$precip[(isws3$precip<0)]<-NA

#make a date column
isws3$calendar<-as.Date(paste(isws3$year, isws3$month, isws3$day, sep="-"))

#convert precip and pot_evapot to mm
isws3$pot_evapot_mm<-isws3$pot_evapot*25.4
isws3$precip_mm<-isws3$precip*25.4

#convert temperatures from F to C 
isws3$isws_air_temp_max_C<-(isws3$max_air_temp-32)*(5/9)
isws3$isws_air_temp_min_C<-(isws3$min_air_temp-32)*(5/9)
isws3$isws_air_temp_avg_C<-(isws3$avg_air_temp-32)*(5/9)


####WANT AWNING AND AMBIENT P-PET####
#ambient ppet (mm)
isws3$ppet_amb<-isws3$precip_mm-isws3$pot_evapot_mm


######JOIN ISWS AND IRRIGATION#####
#2015
#trim down to 2015 field season 
event_15DR<-data.frame(day=as.Date(c("2015-07-06", "2015-07-15", "2015-08-28", "2015-09-23")), 
                       DOY=c(187, 196, 240, 266),
                       event=c("DR sowing", "DR transplanting", "DR harvest start", "DR harvest end"))

#trim isws to experiment time frame  
isws_15DR<-subset(isws3, calendar>="2015-07-06" & calendar<="2015-09-23")

#join isws and irrigation data
isws_15DR<-merge(isws_15DR, irr_15DR, by=c("calendar"), all=TRUE)

#convert NA to 0 for applied_cm and applied_mm
isws_15DR$applied_cm[is.na(isws_15DR$applied_cm)]<-0
isws_15DR$applied_mm[is.na(isws_15DR$applied_mm)]<-0

#calculate awning treatment P-PET (mm) 
isws_15DR$ppet_wet<-isws_15DR$applied_mm-isws_15DR$pot_evapot_mm
isws_15DR$ppet_dry<-0-isws_15DR$pot_evapot_mm

#raw scatter
plot(isws_15DR$calendar, isws_15DR$ppet_amb)
plot(isws_15DR$calendar, isws_15DR$ppet_wet)
plot(isws_15DR$calendar, isws_15DR$ppet_dry)

#now try 7 day running average 
#try some running averages stuff 

f7<-rep(1/7, 7)

y_amb15<-as.data.frame(filter(isws_15DR$ppet_amb, f7, sides=2))
y_wet15<-as.data.frame(filter(isws_15DR$ppet_wet, f7, sides=2))
y_dry15<-as.data.frame(filter(isws_15DR$ppet_dry, f7, sides=2))

y_all15<-cbind(y_amb15, y_wet15, y_dry15)
colnames(y_all15)<-c("running_amb","running_wet","running_dry")

isws_15DR1<-cbind(isws_15DR, y_all15)

plot(isws_15DR1$calendar, isws_15DR1$running_wet, type="n")
lines(isws_15DR1$calendar, isws_15DR1$running_amb, col="grey")
lines(isws_15DR1$calendar, isws_15DR1$running_wet, col="blue")
lines(isws_15DR1$calendar, isws_15DR1$running_dry, col="red")

PPET_15<-ggplot(data=isws_15DR1)+
  geom_line(aes(x=calendar, y=running_amb), color="grey")+
  geom_line(aes(x=calendar, y=running_wet), color="blue")+
  geom_line(aes(x=calendar, y=running_dry), color="red")+
  geom_hline(yintercept=0, linetype=3)+
  geom_vline(data=event_15DR, aes(xintercept=as.numeric(day)))+
  geom_text(data=event_15DR, mapping=aes(x=day, y=0.01, label=event), angle=90, vjust=-0.4, hjust=0, size=3)+
  theme_classic()+
  scale_y_continuous(name="2015 P-PET mm", expand=c(0,0), limits=c(-10,30))

PPET_15


#2013
#trim to 2013 experiment timeframe (both experiments)
isws_13<-subset(isws3, calendar>="2013-05-03" & calendar<="2013-10-31")
#join 2013 isws and irrigation data 
isws_13<-merge(isws_13, irr_13DR, by=c("calendar"), all=TRUE)

#convert NA to 0 for applied_cm and applied_mm
isws_13$applied_cm[isws_13$calendar>"2013-07-07" & is.na(isws_13$applied_cm)]<-0
isws_13$applied_mm[isws_13$calendar>"2013-07-07" & is.na(isws_13$applied_mm)]<-0

#calculate awning treatment P-PET (mm)
isws_13$ppet_wet<-isws_13$applied_mm-isws_13$pot_evapot_mm
isws_13$ppet_dry<-0-isws_13$pot_evapot_mm

for (i in 1:nrow(isws_13)){
  if (isws_13$calendar[i] > "2013-07-07"){
  isws_13$ppet_dry[i]<-0-isws_13$pot_evapot_mm[i]
  }
  else {isws_13$ppet_dry[i]<-NA}
}


#calculate running averages 
y_amb13<-as.data.frame(filter(isws_13$ppet_amb, f7, sides=2))
y_wet13<-as.data.frame(filter(isws_13$ppet_wet, f7, sides=2))
y_dry13<-as.data.frame(filter(isws_13$ppet_dry, f7, sides=2))

y_all13<-cbind(y_amb13, y_wet13, y_dry13)
colnames(y_all13)<-c("running_amb","running_wet","running_dry")

isws_131<-cbind(isws_13, y_all13)

plot(isws_131$calendar, isws_131$running_wet, type="n")
lines(isws_131$calendar, isws_131$running_amb, col="grey")
lines(isws_131$calendar, isws_131$running_wet, col="blue")
lines(isws_131$calendar, isws_131$running_dry, col="red")


PPET_13<-ggplot(data=isws_131)+
  geom_line(aes(x=calendar, y=running_amb), color="grey")+
  geom_line(aes(x=calendar, y=running_wet), color="blue")+
  geom_line(aes(x=calendar, y=running_dry), color="red")+
  geom_hline(yintercept=0, linetype=3)+
  geom_vline(data=event_2013, aes(xintercept=as.numeric(day)))+
  geom_text(data=event_2013, mapping=aes(x=day, y=0.01, label=event), angle=90, vjust=-0.4, hjust=0, size=3)+
  theme_classic()+
  scale_y_continuous(name="2013 P-PET mm", expand=c(0,0), limits=c(-10,70))

PPET_13

#2014
#will just have ambient PPET, still tracking down irrigation notes from this season 
isws_14<-subset(isws3, calendar>="2014-06-08" & calendar<="2014-10-05")

y_amb14<-as.data.frame(filter(isws_14$ppet_amb, f7, sides=2))
y_all14<-y_amb14
colnames(y_all14)<-"running_amb"

isws_141<-cbind(isws_14, y_all14)

plot(isws_141$calendar, isws_141$running_amb, type="n")
lines(isws_141$calendar, isws_141$running_amb, col="grey")


PPET_14<-ggplot(data=isws_141)+
  geom_line(aes(x=calendar, y=running_amb), color="grey")+
  geom_hline(yintercept=0, linetype=3)+
  geom_vline(data=event_2014, aes(xintercept=as.numeric(day)))+
  geom_text(data=event_2014, mapping=aes(x=day, y=0.01, label=event), angle=90, vjust=-0.4, hjust=0, size=3)+
  theme_classic()+
  scale_y_continuous(name="2014 P-PET mm", expand=c(0,0), limits=c(-10,20))

PPET_14



######OUTPUT RESULTS######

png(file="./results/PPET_RIL.png", width=900, height=550)
grid.draw(rbind(ggplotGrob(PPET_15), ggplotGrob(PPET_14), ggplotGrob(PPET_13), size="last"))
dev.off()











######ARCHIVE######

#join SoyFACE and ISWS data
big_weather<-merge(isws3, ave24, by=c("calendar"))



ggplot(data=big_weather, aes(x=AirTemp_Max, y=isws_air_temp_max_C))+geom_point()
ggplot(data=big_weather, aes(x=AirTemp_Min, y=isws_air_temp_min_C))+geom_point()


ggplot(data=big_weather, aes(x=RH_Min, y=min_rel_hum))+geom_point()

ggplot(data=big_weather, aes(x=RAIN_CSI_Tot, y=RAIN_ETI_Tot))+geom_point()

ggplot(data=big_weather)+
  geom_line(aes(x=calendar, y=isws_air_temp_max_C), color="red")+
  geom_line(aes(x=calendar, y=AirTemp_Max), color="blue")

ggplot(data=big_weather)+
  geom_point(aes(x=calendar, y=min_rel_hum), color="red")+
  geom_point(aes(x=calendar, y=RH_Min), color="blue")

ggplot(data=big_weather)+
  geom_bar(aes(x=calendar, y=RAIN_ANDY_Tot), stat="identity", color="red")+
  geom_bar(aes(x=calendar, y=precip), stat="identity", color="blue")

