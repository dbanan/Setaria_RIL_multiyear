#Setaria_PCA_phenotypes.R 

#6/12/18
#migrate analysis from May data meeting 
#6/26/18
#now on git, this script (fingers crossed) should run smoothly from the BLUP calculation output 

library(plyr)
library(ggbiplot)
library(reshape2)




#load BLUPs
load("RIL_BLUP.Rdata")





#####PCA BLUP######

#just on thick planted 
just_thick<-rils.blups[-which(rils.blups$treatment=="sparse"),]


#ditch 2014 drought for this run because i dont know better... 
just_thick$yr_exp<-paste(just_thick$year, just_thick$experiment, sep="_")
just_thick<-just_thick[-which(just_thick$yr_exp=="2014_drought")]


#lists for just common traits (version for many environments but few traits; few environments but many traits)
#future: need to test mass at harvest and mass per DAS

common<-c("panicle_emergence_DAS", 
          "leaf_mass_at_harvest", 
          "panicle_mass_at_harvest", 
          "stem_mass_at_harvest", 
          "vegetative_mass_at_harvest", 
          "total_mass_at_harvest", 
          "reproductive_vegetative_mass_ratio",
          "culm_height",
          "tiller_number_cbrt",
          "d13C"
)

common_no_14dr<-c("panicle_emergence_DAS", 
          "leaf_mass_at_harvest", 
          "panicle_mass_at_harvest", 
          "stem_mass_at_harvest", 
          "vegetative_mass_at_harvest", 
          "total_mass_at_harvest", 
          "reproductive_vegetative_mass_ratio",
          "culm_height",
          "tiller_number_cbrt",
          "d13C",
          "lfblade_area",
          "lfblade_weight",
          "sim_CN_ratio",
          "sim_gC.m2",
          "sim_gN.m2",
          "SLA"
)






#just_common<-subset(just_thick, trait %in% common)
just_common<-subset(just_thick, trait %in% common_no_14dr)

#need genotype*treatment averages (on BLUP predicted values)
just_geno<-ddply(just_common, c("year", "experiment", "treatment", "genotype", "trait"), summarise, data=mean(predicted, na.rm=TRUE))


just_geno$environment<-paste(just_geno$treatment, just_geno$year, sep="_")

#wide by trait and still long genotype and environment
just_wide<-dcast(just_geno, environment+genotype~trait, value.var="data")

#complete cases?
just_wide1<-just_wide[complete.cases(just_wide),]

#pull out environment column 
env<-just_wide1[,1]

#join genotype and environment
just_wide1$name<-paste(just_wide1$genotype, just_wide1$environment, sep="_")
rownames(just_wide1)<-just_wide1$name
just_wide1$name<-NULL

just_wide2<-just_wide1[,c(3:13)]


pca_try<-prcomp(just_wide2, center=TRUE, scale.=TRUE)


pca_try$sdev
pca_try$rotation
pca_try$x



plot(pca_try, type="l")



plot(pca_try$x[,1:2])
biplot(pca_try)


pca_try_table<-rbind(pca_try$rotation, pca_try$sdev)

#visualize

ggbiplot(pca_try, choices=c(1,2),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)

ggbiplot(pca_try, choices=c(1,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)

ggbiplot(pca_try, choices=c(2,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)

png("./results/PC12.png", width=900, height=700)
ggbiplot(pca_try, choices=c(1,2),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
dev.off()

png("./results/PC13.png", width=900, height=700)
ggbiplot(pca_try, choices=c(1,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
dev.off()

png("./results/PC23.png", width=900, height=700)
ggbiplot(pca_try, choices=c(2,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)
dev.off()








####CORRELATIONS####

#should eventually move this to a separate script? treat below as early visualization for 7/24 meeting. 



#some pairs plots colored by environment 


cols<-character(nrow(just_wide1))
cols[]<-"black"
cols[just_wide$environment=="dry_2013"]<-"red"
cols[just_wide$environment=="dry_2015"]<-"orange"
cols[just_wide$environment=="thick_2013"]<-"green"
cols[just_wide$environment=="thick_2014"]<-"light blue"
cols[just_wide$environment=="wet_2013"]<-"blue"
cols[just_wide$environment=="wet_2015"]<-"purple"

#all
png("./results/pairs_all.png", width=800, height=800)
pairs(just_wide1[3:18], pch=21, col=cols)
dev.off()      

#split into useful groups (hopefully)

#leaf and architecture 
png("./results/pairs_leaf_arch.png", width=900, height=700)
pairs(just_wide1[,c(4,6,7,11,12,13,14,3,16)], pch=21, col=cols)
dev.off()

#leaf and devo 
png("./results/pairs_leaf_devo.png", width=900, height=700)
pairs(just_wide1[,c(4,6,7,11,12,13,14,8,9,10)], pch=21, col=cols)
dev.off()

#leaf and mass 
png("./results/pairs_leaf_mass.png", width=900, height=700)
pairs(just_wide1[,c(4,6,7,11,12,13,14,5,9,15,17,18)], pch=21, col=cols)
dev.off()

#mix
png("./results/pairs_mix_dim.png", width=900, height=700)
pairs(just_wide1[,c(6,7,14,8,9,10,3,16,17)], pch=21, col=cols)
dev.off()

png("./results/pairs_mix_elem.png", width=900, height=700)
pairs(just_wide1[,c(4,11,12,13,8,9,10,3,16,17)], pch=21, col=cols)
dev.off()

  
  


