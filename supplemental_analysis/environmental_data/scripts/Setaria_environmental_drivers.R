#Setaria_environmental_drivers 

#attempting to answer:
#How do our environments differ?
#What environmental variables drive variation in physiology?


library(ggbiplot)
library(plyr)
library(ggplot2)



#load relevant weather data
load("./results/weather_data_all.Rdata")


#trim to traits, environments of interest 



#####PCA#####
#environments of interest 

all_env<-weather_all[-c(20,24)]

#variables of interest 
just_wide1<-all_env[complete.cases(all_env),]

#pull out environment column 
env<-just_wide1[,23]

#join genotype and environment
just_wide1$name<-paste(just_wide1$environment, just_wide1$calendar, sep="_")
rownames(just_wide1)<-just_wide1$name
just_wide1$name<-NULL

just_wide2<-just_wide1[,c(2:22,24:27)]


pca_try<-prcomp(just_wide2, center=TRUE, scale.=TRUE)


pca_try$sdev
pca_try$rotation
pca_try$x

components<-pca_try$x



plot(pca_try, type="l")



plot(pca_try$x[,1:2])
biplot(pca_try)


pca_try_table<-rbind(pca_try$rotation, pca_try$sdev)

#visualize

ggbiplot(pca_try, choices=c(1,2),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_try, choices=c(1,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_try, choices=c(2,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)




#####PCA OVER TIME#####



#change rownames to observation column 
comp<-as.data.frame(cbind(name=rownames(components), components))


compl<-melt(comp, id.vars=c("name"), measure.vars=c("PC1","PC2","PC3"), variable.name="component", value.name="data") 

compl$data<-as.numeric(compl$data)

#break "name" into plottable attributes 
compl<-separate(compl, name, into=c("treatment","year","calendar"), sep="_")
compl$environment<-paste(compl$treatment, compl$year, sep="_")
compl$calendar<-as.Date(compl$calendar)




ggplot(compl, aes(x=calendar, y=data, group=component, color=component))+
  geom_smooth()+
  facet_wrap(~environment, scale="free")









