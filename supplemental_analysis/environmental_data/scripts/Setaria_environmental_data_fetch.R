#Updated weather data infile 

#load both ISWS and SoyFACE weather data





#SoyFACE weather data 

#ISWS weather data (PET)

#Irrigation data 
#P-PET 
#soil moisture 

#^calculate running averages 

#PAR? have on ten minute resolution, how to tabulate daily? min PAR is nighttime...0; average daytime PAR? 

#day length (NOAA, Naval observatory)





#####SOIL MOISTURE#####
load("./results/sm_data.Rdata")

#####IRRIGATION######
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

#fix duplicate row for 9/14/13
irr_13DR<-irr_13DR[-c(48),]


#2014
#irrigation data is spotty...due to flooding events and treatment reassignments? 
#chase this down later 

#trim
irr_13DR<-irr_13DR[,c(1,4)]
irr_15DR<-irr_15DR[,c(8,7)]


#####DAYLENGTH#####
daylength<-read.csv("./data/daylength_long.csv", header=T)
daylength13<-daylength
daylength13$year<-2013
daylength14<-daylength
daylength14$year<-2014
daylength15<-daylength
daylength15$year<-2015

daylength<-rbind(daylength13, daylength14, daylength15)
daylength$calendar<-as.Date(paste(daylength$year, daylength$month, daylength$day, sep="-"))
daylength<-daylength[,c(6,3,4)]

#convert to time format 
daylength$sunrise1<-sprintf("%04d", daylength$sunrise)
daylength$sunrise1<-as.POSIXct(daylength$sunrise1, format="%H%M")

daylength$sunset1<-sprintf("%04d", daylength$sunset)
daylength$sunset1<-as.POSIXct(daylength$sunset1, format="%H%M")

#day length traits 
daylength$daylight<-as.numeric(daylength$sunset1-daylength$sunrise1)
daylength$night<-24-daylength$daylight
daylength$day_night_ratio<-daylength$daylight/daylength$night

daylength<-daylength[,c(1,6,7,8)]



######SoyFACE######
#infile 24 hour data
ave24header<-read.csv("./data/24hr_weather_16sept2016.csv", skip=1, header=F, nrows=1, as.is=T)
ave24<-read.csv("./data/24hr_weather_16sept2016.csv", header=F, skip=4)
colnames(ave24)<-ave24header
#convert timestamp format to julian
#ave24$calendar<-as.Date(ave24$TIMESTAMP)
ave24$calendar<-as.Date(as.POSIXct(ave24$TIMESTAMP, format="%m/%d/%Y %H:%M"))
#to DOY
ave24$DOY<-as.numeric(strftime(ave24$calendar, format="%j"))

#trim to variables of interest 
ave24<-ave24[,c(3,5,7,8,10,12,13,14,15,19,20)]

colnames(ave24)<-c("soy_Tmax","soy_RHmax","soy_windMax","soy_Tmin","soy_RHmin","soy_windMin","soy_precipCSI","soy_precipETI","soy_precipANDY","calendar","DOY")


#####ISWS#####
#load and format data
isws<-read.table("./data/CMIDAY.txt",header=T,sep="\t", stringsAsFactors=FALSE)

#ditch last rows with data metadata and first row with units 
#keep columns of interest 
isws<-isws[-c(1,10548:10557), c("year","month","day","sol_rad","max_air_temp","min_air_temp","avg_air_temp","max_rel_hum","min_rel_hum","avg_rel_hum","avg_dewpt_temp","precip","pot_evapot")]

#format to numeric 
isws<-sapply(isws, as.numeric)
isws<-as.data.frame(isws)

#convert negative precip to NA 
isws$precip[(isws$precip<0)]<-NA

#make a date column
isws$calendar<-as.Date(paste(isws$year, isws$month, isws$day, sep="-"))

#convert precip and pot_evapot to mm
isws$pot_evapot_mm<-isws$pot_evapot*25.4
isws$precip_mm<-isws$precip*25.4

#convert temperatures from F to C 
isws$isws_air_temp_max_C<-(isws$max_air_temp-32)*(5/9)
isws$isws_air_temp_min_C<-(isws$min_air_temp-32)*(5/9)
isws$isws_air_temp_avg_C<-(isws$avg_air_temp-32)*(5/9)

#trim to variables of interest and rename 
isws<-isws[,c(14,4,8,9,10,15,16,17,18,19)]
colnames(isws)<-c("calendar","isws_solRad","isws_RHmax","isws_RHmin","isws_RHavg","isws_PET","isws_precip","isws_Tmax","isws_Tmin","isws_Tavg")


#####EVENTS#####
event_13DN<-data.frame(calendar=as.Date(c("2013-05-03", "2013-05-14", "2013-07-29", "2013-08-13")), 
                       DOY=c(123, 134, 210, 225),
                       event=c("DN sowing", "DN transplanting", "DN harvest start", "DN harvest end"))

event_13DR<-data.frame(calendar=as.Date(c("2013-07-07", "2013-07-16", "2013-10-07", "2013-10-31")), 
                       DOY=c(188, 197, 280, 304),
                       event=c("DR sowing", "DR transplanting", "DR harvest start", "DR harvest end"))

event_14DR<-data.frame(calendar=as.Date(c("2014-06-08", "2014-06-17", "2014-08-06", "2014-08-25")), 
                       DOY=c(159, 168, 218, 237),
                       event=c("DR sowing", "DR transplanting", "DR harvest start", "DR harvest end"))

event_14DN<-data.frame(calendar=as.Date(c("2014-07-17", "2014-07-24", "2014-09-22", "2014-10-05")), 
                       DOY=c(198, 205, 265, 278), 
                       event=c("DN sowing", "DN transplanting", "DN harvest start", "DN harvest end"))

event_15DR<-data.frame(calendar=as.Date(c("2015-07-06", "2015-07-15", "2015-08-28", "2015-09-23")), 
                       DOY=c(187, 196, 240, 266),
                       event=c("DR sowing", "DR transplanting", "DR harvest start", "DR harvest end"))

#experiment time frames 
date13DN<-seq(as.Date("2013-05-03"), as.Date("2013-08-13"), by=1)
date13DR<-seq(as.Date("2013-07-07"), as.Date("2013-10-31"), by=1)
date14DN<-seq(as.Date("2014-07-17"), as.Date("2014-10-05"), by=1)
date14DR<-seq(as.Date("2014-06-08"), as.Date("2014-08-25"), by=1)
date15DR<-seq(as.Date("2015-07-06"), as.Date("2015-09-23"), by=1)


#####MERGE#####
weather<-merge(isws, ave24, by=c("calendar"), all=TRUE)
weather<-merge(weather, daylength, by=c("calendar"), all=TRUE)

#restrict to experimental timeframes 
weather13DN<-subset(weather, calendar %in% date13DN) 
weather13DR<-subset(weather, calendar %in% date13DR)
weather14DN<-subset(weather, calendar %in% date14DN)
weather14DR<-subset(weather, calendar %in% date14DR)
weather15DR<-subset(weather, calendar %in% date15DR)

#join with events (join from plyr)
weather13DN<-join(weather13DN, event_13DN)
weather13DR<-join(weather13DR, event_13DR)
weather14DN<-join(weather14DN, event_14DN)
weather14DR<-join(weather14DR, event_14DR)
weather15DR<-join(weather15DR, event_15DR)

#join with irrigation
weather13DR<-join(weather13DR, irr_13DR, match="all")
weather15DR<-join(weather15DR, irr_15DR, match="all")

#split weather data into environments 
thick13<-weather13DN
dry13<-weather13DR
thick14<-weather14DN
dry14<-weather14DR
wet14<-weather14DR
dry15<-weather15DR
wet15<-weather15DR

#add environment columns 
thick13$environment<-"thick_2013"
dry13$environment<-"dry_2013"
thick14$environment<-"thick_2014"
dry14$environment<-"dry_2014"
wet14$environment<-"wet_2014"
dry15$environment<-"dry_2015"
wet15$environment<-"wet_2015"




#add "applied_mm" to non-irrigated experiments (blank for 14DR (for now), precip for density)
thick13$applied_mm<-thick13$isws_precip
dry13$applied_mm<-0
thick14$applied_mm<-thick14$isws_precip
dry14$applied_mm<-0
wet14$applied_mm<-wet14$isws_precip #still need to find irrigation records for 2014 Drought experiment, use background precip for now
dry15$applied_mm<-0
#wet15 already merged in 



#stack individual environments 
weather_all<-rbind(dry13, dry14, dry15, thick13, thick14, wet14, wet15)

weather_all$applied_mm[is.na(weather_all$applied_mm)]<-0


#convert dates to DAS 
weather_all$sowing[weather_all$environment=="thick_2013"]<-123
weather_all$sowing[weather_all$environment=="dry_2013"]<-188
weather_all$sowing[weather_all$environment=="thick_2014"]<-198
weather_all$sowing[weather_all$environment=="dry_2014"]<-159
weather_all$sowing[weather_all$environment=="wet_2014"]<-159
weather_all$sowing[weather_all$environment=="dry_2015"]<-187
weather_all$sowing[weather_all$environment=="wet_2015"]<-187

weather_all$DAS<-weather_all$DOY-weather_all$sowing

#join with soil moisture data
weather_all<-join(weather_all, sm_all)

rownames(weather_all)<-paste(weather_all$environment, weather_all$DAS, sep="_")


#####RUNNING#####
#running averages (precip, PET, PPET, soil moisture)

#PPET
#calculate P-PET (applied_mm-isws_PET)
weather_all$PPET<-weather_all$applied_mm-weather_all$isws_PET


#calculate 7 day running averages on "applied_mm" and "PPET" and soil moisture 
f7<-rep(1/7, 7)

running_PPET<-as.data.frame(filter(weather_all$PPET, f7, sides=2))
colnames(running_PPET)<-"running_PPET"
running_PPET$running_PPET[is.na(running_PPET$running_PPET)]<-0

running_precip<-as.data.frame(filter(weather_all$applied_mm, f7, sides=2))
colnames(running_precip)<-"running_precip"
running_precip$running_precip[is.na(running_precip$running_precip)]<-0

running_top<-as.data.frame(filter(weather_all$top, f7, sides=2))
colnames(running_top)<-"running_top"
running_top$running_top[is.na(running_top$running_top)]<-0

running_middle<-as.data.frame(filter(weather_all$middle, f7, side=2))
colnames(running_middle)<-"running_middle"
running_middle$running_middle[is.na(running_middle$running_middle)]<-0

running_bottom<-as.data.frame(filter(weather_all$bottom, f7, side=2))
colnames(running_bottom)<-"running_bottom"
running_bottom$running_bottom[is.na(running_bottom$running_bottom)]<-0

#join with the other traits 
weather_all<-cbind(weather_all, running_PPET, running_precip, running_top, running_middle, running_bottom)



#####PRODUCT#####
save(weather_all, file="./results/weather_data_all.Rdata")










