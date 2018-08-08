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

all_env<-weather_all
all_env$name<-paste(all_env$environment, all_env$DAS, sep="_")

#trim to variables of interest 
all_env<-all_env[,c(38,25,28,2,6,10,5,23,33,34)]

all_env1<-all_env[complete.cases(all_env),]

env<-all_env1[,2]

rownames(all_env1)<-all_env1$name




#variables of interest 
just_wide1<-all_env[complete.cases(all_env),]

#pull out environment column 
env<-just_wide1[,23]

#join genotype and environment
just_wide1$name<-paste(just_wide1$environment, just_wide1$DAS, sep="_")
rownames(just_wide1)<-just_wide1$name
just_wide1$name<-NULL

#trim data to variables of interest for PCA
just_wide2<-just_wide1[,c(2:22,24:27)]

just_wide2<-all_env1[,c(4:10)]

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


ggbiplot(pca_try, choices=c(1,4),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_try, choices=c(2,4),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_try, choices=c(3,4),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)


#####PCA OVER TIME#####



#change rownames to observation column 
comp<-as.data.frame(cbind(name=rownames(components), components))


compl<-melt(comp, id.vars=c("name"), measure.vars=c("PC1","PC2","PC3"), variable.name="component", value.name="data") 

compl$data<-as.numeric(compl$data)

#break "name" into plottable attributes 
compl<-separate(compl, name, into=c("treatment","year","DAS"), sep="_")
compl$environment<-paste(compl$treatment, compl$year, sep="_")
compl$DAS<-as.numeric(compl$DAS)




ggplot(compl, aes(x=DAS, y=data, group=component, color=component))+
  geom_smooth()+
  facet_wrap(~environment)









