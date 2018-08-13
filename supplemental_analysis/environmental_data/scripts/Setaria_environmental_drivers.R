#Setaria_environmental_drivers 

#attempting to answer:
#How do our environments differ?
#What environmental variables drive variation in physiology?


library(ggbiplot)
library(plyr)
library(ggplot2)



#load weather data
load("./results/weather_data_all.Rdata")


#trim to traits, environments of interest 



#####PCA#####
#environments of interest 

all_env<-weather_all
all_env$name<-paste(all_env$environment, all_env$DAS, sep="_")

#trim to variables of interest 
all_env<-all_env[,c(38,25,28,2,6,10,5,23,33,34)] #this one for common variables 

#all_env<-all_env[,c(38,25,28,2,6,10,5,23,33,34,35,36,37)] #this one adds soil moisture 

all_env1<-all_env[complete.cases(all_env),]
all_env1<-subset(all_env1, environment %in% c("dry_2013","dry_2015","wet_2015"))

env<-all_env1[,2]

rownames(all_env1)<-all_env1$name


#columns of interest
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


write.csv(pca_try_table, "./results/pca_table.csv")

#visualize

png("./results/PC12_env.png", width=900, height=700)
ggbiplot(pca_try, choices=c(1,2),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
dev.off()

png("./results/PC13_env.png", width=900, height=700)
ggbiplot(pca_try, choices=c(1,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
dev.off()

png("./results/PC23_env.png", width=900, height=700)
ggbiplot(pca_try, choices=c(2,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
dev.off()



#####PCA OVER TIME#####
#change rownames to observation column 
comp<-as.data.frame(cbind(name=rownames(components), components))

compl<-melt(comp, id.vars=c("name"), measure.vars=c("PC1","PC2","PC3"), variable.name="component", value.name="data") 

compl$data<-as.numeric(compl$data)

#break "name" into plottable attributes 
compl<-separate(compl, name, into=c("treatment","year","DAS"), sep="_")
compl$environment<-paste(compl$treatment, compl$year, sep="_")
compl$DAS<-as.numeric(compl$DAS)

#visualize
png("./results/envPC_vTime.png", width=900, height=700)
ggplot(compl, aes(x=DAS, y=data, group=component, color=component))+
  geom_smooth()+
  facet_wrap(~environment)
dev.off()



#######AVERAGE PC VALUE######
#first do simple averages over experimental time period 
PCavg<-ddply(compl, c("environment","component"), summarise, avg=mean(data), stderr = sd(data) / sqrt((length(data))))

png("./results/envPC_avg.png", width=800, height=400)
ggplot(PCavg, aes(x=environment, y=avg, fill=environment))+
  geom_bar(stat="identity")+
  facet_wrap(~component)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_errorbar(aes(ymin=avg-stderr, ymax=avg+stderr), width=.2,
                position=position_dodge(.9)) 
dev.off()

envPCavg<-PCavg
save(envPCavg, file="./results/envPCavg.Rdata")





########CORRELATIONS########
#some pairs plots colored by environment 

common_wide<-all_env

common_wide$environment=factor(levels=c("thick_2013","dry_2013","thick_2014","dry_2014","wet_2014","dry_2015","wet_2015"),
                               ordered=T)

cols<-character(nrow(common_wide))
cols[]<-"black"
cols[common_wide$environment=="dry_2013"]<-"red"
cols[common_wide$environment=="dry_2015"]<-"orange"
cols[common_wide$environment=="thick_2013"]<-"green"
cols[common_wide$environment=="thick_2014"]<-"black"
cols[common_wide$environment=="dry_2014"]<-"pink"
cols[common_wide$environment=="wet_2014"]="blue"
cols[common_wide$environment=="wet_2015"]<-"purple"




png("./results/env_corr.png", width=900, height=700)
pairs(common_wide[,c(4:10)], pch=21, col=cols)
dev.off()






##########RANK ORDER PCA###########
#rank order PC
comp<-as.data.frame(cbind(name=rownames(pca_DAS_components), pca_DAS_components))
comp$PC1<-as.numeric(as.character(comp$PC1))
comp$PC2<-as.numeric(as.character(comp$PC2))
comp$PC3<-as.numeric(as.character(comp$PC3))

compl<-melt(comp, id.vars=c("name"), measure.vars=c("PC1","PC2","PC3"), variable.name="component", value.name="data", factorsAsStrings=F) 

#break "name" into plottable attributes 
library(tidyr)
compl<-separate(compl, name, into=c("geno", "id", "experiment", "year", "treatment"), sep="_")

compl1<-compl[-which(compl$geno %in% c("A10","B100")),]
compl2<-subset(compl, geno %in% c("A10", "B100"))

compl1$genotype<-paste(compl1$geno, compl1$id, sep="_")
compl1$environment<-paste(compl1$treatment, compl1$year, sep="_")
compl1<-compl1[,c(8,9,6,7)]

compl2$genotype<-compl2$geno
compl2$environment<-paste(compl2$year, compl2$experiment, sep="_")
compl2<-compl2[,c(8,9,6,7)]

compl3<-rbind(compl2, compl1)


#global genotype mean PC
global<-ddply(compl3, c("genotype", "component"), summarise, avg=mean(data), covar=(sd(data)/mean(data)))

#merge with individual values 
compl4<-join(compl3, global)
compl4<-compl4[order(compl4[,3], compl4[,5]),]

compl4$genotype1<-as.factor(compl4$genotype)


ggplot(data=compl4, aes(x=genotype1, y=data))+geom_smooth(aes(group=environment, color=environment))+facet_wrap(~component)


load("../supplemental_analysis/environmental_data/results/envPCavg.Rdata")



