#Setaria environmental data merge 

library(reshape2)


#generate dataset that is long by experiment and DAS, wide by environmental variable 


#bring in relevant data (probably two versions: one with 2014 and without, due to current incomplete irrigation records)


#environmental variables: 
#T, rel_hum, precip, pot_evap, 
#eventually find a way to include soil moisture (density lacks tubes, 2015 used prongs)





#2015

#isws_15DR1
#convert calendar to DAS 
isws_15DR1$sowing<-as.Date("2015-07-06")
isws_15DR1$sowingdoy<-as.numeric(strftime(isws_15DR1$sowing, format="%j"))
isws_15DR1$doy<-as.numeric(strftime(isws_15DR1$calendar, format="%j"))
isws_15DR1$DAS<-isws_15DR1$doy-isws_15DR1$sowingdoy




#variables of interest for wet and dry 
wet_15<-isws_15DR1[,c(36,9,10,11,17,18,19,27,15,31)]
wet_15$environment<-"wet_2015"
colnames(wet_15)[10]<-"PPET"

dry_15<-isws_15DR1[,c(36,9,10,11,17,18,19,15,32)]
dry_15$environment<-"dry_2015"
colnames(dry_15)[9]<-"PPET"
dry_15$applied_mm<-0


#2014 density 
#subset, convert dates 
thick_14<-subset(isws_141, calendar>="2014-07-17" & calendar<="2014-10-05")
thick_14$sowing<-as.Date("2014-07-17")
thick_14$sowingdoy<-as.numeric(strftime(thick_14$sowing, format="%j"))
thick_14$doy<-as.numeric(strftime(thick_14$calendar, format="%j"))
thick_14$DAS<-thick_14$doy-thick_14$sowingdoy

#trim
thick_14<-thick_14[,c(25,8,9,10,17,18,19,16,15,21)]
thick_14$environment<-"thick_2014"
colnames(thick_14)[8]<-"applied_mm"
colnames(thick_14)[10]<-"PPET"

#2014 drought 
#subset and date format 
drought_14<-subset(isws_141, calendar>="2014-06-08" & calendar<="2014-08-25")
drought_14$sowing<-"2014-06-08"
drought_14$sowingdoy<-as.numeric(strftime(drought_14$sowing, format="%j"))
drought_14$doy<-as.numeric(strftime(drought_14$calendar, format="%j"))
drought_14$DAS<-drought_14$doy-drought_14$sowingdoy

#split wet and dry 
wet_14<-drought_14[,c(25,8,9,10,17,18,19,15)]
wet_14$environment<-"wet_2014"
wet_14$applied_mm<-NA
wet_14$PPET<-NA

dry_14<-drought_14[,c(25,8,9,10,17,18,19,15)]
dry_14$environment<-"dry_2014"
dry_14$applied_mm<-0
dry_14$PPET<-0


#2013 density 
#subset to 2013 density dates 
thick_13<-subset(isws_131, calendar>="2013-05-03" & calendar<="2013-08-13")

#convert calendar to DAS 
thick_13$sowing<-as.Date("2013-05-03")
thick_13$sowingdoy<-as.numeric(strftime(thick_13$sowing, format="%j"))
thick_13$doy<-as.numeric(strftime(thick_13$calendar, format="%j"))
thick_13$DAS<-thick_13$doy-thick_13$sowingdoy

#trim to relevant columns 
thick_13<-thick_13[,c(32,9,10,11,17,18,19,15,16,26)]
thick_13$environment<-"thick_2013"
colnames(thick_13)[9]<-"applied_mm"
colnames(thick_13)[10]<-"PPET"


#2013 drought 
#subset to 2013 drought dates, convert to DAS
drought_13<-subset(isws_131, calendar>="2013-07-07" & calendar<="2013-10-31")
drought_13$sowing<-as.Date("2013-07-07")
drought_13$sowingdoy<-as.numeric(strftime(drought_13$sowing, format="%j"))
drought_13$doy<-as.numeric(strftime(drought_13$calendar, format="%j"))
drought_13$DAS<-drought_13$doy-drought_13$sowingdoy


#super buggy way to deal with double 9-14 data, fix this later in orignal irrigation table 
drought_13<-drought_13[-c(71),]

#break into wet and dry 
wet_13<-drought_13[,c(32,9,10,11,17,18,19,15,23,27)]
wet_13$environment<-"wet_2013"
colnames(wet_13)[10]<-"PPET"

dry_13<-drought_13[,c(32,9,10,11,17,18,19,15,28)]
dry_13$environment<-"dry_2013"
dry_13$applied_mm<-0
colnames(dry_13)[9]<-"PPET"





#stack 
all_env<-rbind(thick_13,thick_14,wet_14,wet_15,dry_13,dry_14,dry_15)

all_env<-all_env[,c(11,1:10)]

all_env$PPET[is.na(all_env$PPET)]<-0

#####PRODUCT#####
save(all_env, file="./results/weather_irr_wide.Rdata")







