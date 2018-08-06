#Setaria_PCA_phenotypes.R 

#6/12/18
#migrate analysis from May data meeting 
#6/26/18
#now on git, this script (fingers crossed) should run smoothly from the BLUP calculation output 

library(plyr)
library(ggbiplot)
library(reshape2)
library(FactoMineR)
#load BLUPs
load("RIL_BLUP.Rdata")

####PCA BLUP######

#just on removing low density plants from the dataset 
just_thick<-rils.blups[-which(rils.blups$treatment=="sparse"),]
table(just_thick$year)

#lists for just common traits (version for many environments but few traits; few environments but many traits)
#future: need to test mass at harvest and mass per DAS
trait.matrix=as.data.frame.array(table(just_thick$trait,just_thick$environment))
trait.check=apply(trait.matrix,1,function(i) any(i == 0))
common=names(which(trait.check ==F))

just_common<-subset(just_thick, trait %in% common)

less.common=trait.matrix[-which(rownames(trait.matrix) %in% common),]

#need genotype*treatment averages (on BLUP predicted values)
common_geno<-ddply(just_common, c("year", "experiment",'environment', "treatment", "genotype", "trait"), summarise, data=mean(predicted, na.rm=TRUE))

#wide by trait and still long genotype and environment
common_wide<-dcast(common_geno, year+experiment+environment+genotype+treatment~trait, value.var="data")
write.csv(common_wide, './results/Set_RIL_field_data_BLUPS_wide.csv', row.names = F)
#removing lines that are missing values for the common traits
common.wide.complete<-common_wide[complete.cases(common_wide),]
colnames(common.wide.complete)
#pull out environment column 
env<-paste(common.wide.complete$environment,common.wide.complete$treatment, sep="_")
geno=common.wide.complete$genotype
#PCA with plant traits at harvest, rather than as DAS
pca.matrix1=common.wide.complete[,c(6,7,8,10:14,16:18,21,22,24)]
pca.matrix1=common.wide.complete[,c(6:16,24)]
colnames(pca.matrix1)
#*Darshi had 3 lines of code to do the one line of code below. Rachel updated it.
rownames(pca.matrix1)=paste(common.wide.complete$genotype, common.wide.complete$environment,common.wide.complete$treatment, sep="_")

pca_try<-prcomp(pca.matrix1, center=TRUE, scale.=TRUE)

# pca.try2=PCA(pca.matrix1)
# 
# trait.pc.cor=pca.try2$var$cor
# obs.contrib=pca.try2$ind$contrib
# 
# trait.pc.cor2=pca.try2$var$cor
# obs.contrib2=pca.try2$ind$contrib

pca_try$sdev
pca_try$rotation
pca_try$x

plot(pca_try, type="l")

plot(pca_try$x[,1:2])
biplot(pca_try2)

pca_try_table<-rbind(pca_try$rotation, pca_try$sdev)

#visualize
ggbiplot(pca_try, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_try, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_try, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)

png("./results/PC12.png", width=900, height=700)
ggbiplot(pca_try, choices=c(1,2),labels.size = 2,varname.adjust = 1,obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)+xlim(c(-6,11))+ylim(c(-4,6.5))
dev.off()

png("./results/PC13.png", width=900, height=700)
ggbiplot(pca_try, choices=c(1,3),labels.size = 2,varname.adjust = 1,obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)+xlim(c(-6,11))+ylim(c(-4,5))
dev.off()

png("./results/PC23.png", width=900, height=700)
ggbiplot(pca_try, choices=c(2,3),labels.size = 2,varname.adjust = 1,obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)+xlim(c(-4,6.5))+ylim(c(-4,5))
dev.off()

#PCA with plant harvest traits as DAS
pca.matrix2=common.wide.complete[,c(6,7,9:13,15:17,19,21,23,25)]
pca.matrix2=common.wide.complete[,c(6,7,9,11:13,15,17,19,21,23,25)]
colnames(pca.matrix2)
rownames(pca.matrix2)=paste(common.wide.complete$genotype, common.wide.complete$environment,common.wide.complete$treatment, sep="_")

pca_DAS<-prcomp(pca.matrix2, center=TRUE, scale.=TRUE)
pca_DAS<-PCA(pca.matrix2)

pca_DAS$sdev
pca_DAS$rotation
pca_DAS$x

plot(pca_DAS, type="l")

plot(pca_DAS$x[,1:2])
biplot(pca_DAS)

pca_DAS_table<-rbind(pca_DAS$rotation, pca_DAS$sdev)

#visualize
ggbiplot(pca_DAS, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_DAS, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_DAS, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)

png("./results/PC12_DAS.png", width=900, height=700)
ggbiplot(pca_DAS, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)+xlim(c(-6,11))+ylim(c(-4,6.5))
dev.off()
png("./results/PC13_DAS.png", width=900, height=700)
ggbiplot(pca_DAS, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)+xlim(c(-6,11))+ylim(c(-4,5))
dev.off()
png("./results/PC23_DAS.png", width=900, height=700)
ggbiplot(pca_DAS, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)+xlim(c(-4,6.5))+ylim(c(-4,5))
dev.off()

#PCA without drought 2015
pca.matrix3=common.wide.complete[-which(common.wide.complete$environment=='drought_2015'),c(6,7,8,10:14,16:18,21,22,24)]
pca.matrix3=common.wide.complete[-which(common.wide.complete$environment=='drought_2015'),c(6,7,8,11:14,17:18,21,22,24)]

colnames(pca.matrix3)
rownames(pca.matrix3)=paste(common.wide.complete$genotype[-which(common.wide.complete$environment=='drought_2015')], common.wide.complete$environment[-which(common.wide.complete$environment=='drought_2015')],common.wide.complete$treatment[-which(common.wide.complete$environment=='drought_2015')], sep="_")

pca_no_dr15<-prcomp(pca.matrix3, center=TRUE, scale.=TRUE)

pca_no_dr15$sdev
pca_no_dr15$rotation
pca_no_dr15$x

pca_no_dr15_table<-rbind(pca_DAS$rotation, pca_DAS$sdev)

env2=paste(common.wide.complete$environment[-which(common.wide.complete$environment=='drought_2015')], common.wide.complete$treatment[-which(common.wide.complete$environment=='drought_2015')], sep = '_')
#visualize
ggbiplot(pca_no_dr15, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env2, ellipse=TRUE)
ggbiplot(pca_no_dr15, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env2, ellipse=TRUE)
ggbiplot(pca_no_dr15, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env2, ellipse=TRUE)

png("./results/PC12_no_dr15.png", width=900, height=700)
ggbiplot(pca_no_dr15, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env2, ellipse=TRUE)+xlim(c(-8,15))+ylim(c(-5,5))
dev.off()
png("./results/PC13_no_dr15.png", width=900, height=700)
ggbiplot(pca_no_dr15, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env2, ellipse=TRUE)+xlim(c(-8,15))+ylim(c(-7,5))
dev.off()
png("./results/PC23_no_dr15.png", width=900, height=700)
ggbiplot(pca_no_dr15, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env2, ellipse=TRUE)+xlim(c(-5,5))+ylim(c(-7,5))
dev.off()
#harvest traits as per DAS without DR15
pca.matrix4=common.wide.complete[-which(common.wide.complete$environment=='drought_2015'),c(6:11,17:24)]
pca.matrix4=common.wide.complete[-which(common.wide.complete$environment=='drought_2015'),c(6,7,9,11:13,15,17,19,21,23,25)]

colnames(pca.matrix4)
rownames(pca.matrix4)=paste(common.wide.complete$genotype[-which(common.wide.complete$environment=='drought_2015')], common.wide.complete$environment[-which(common.wide.complete$environment=='drought_2015')],common.wide.complete$treatment[-which(common.wide.complete$environment=='drought_2015')], sep="_")

pca_no_dr15_DAS<-prcomp(pca.matrix4, center=TRUE, scale.=TRUE)
pca_no_dr15_DAS$sdev
#visualize

png("./results/PC12_no_dr15_DAS.png", width=900, height=700)
ggbiplot(pca_no_dr15_DAS, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env2, ellipse=TRUE)
dev.off()
png("./results/PC13_no_dr15_DAS.png", width=900, height=700)
ggbiplot(pca_no_dr15_DAS, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env2, ellipse=TRUE)
dev.off()
png("./results/PC23_no_dr15_DAS.png", width=900, height=700)
ggbiplot(pca_no_dr15_DAS, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env2, ellipse=TRUE)
dev.off()
#----------------------------------------------------------------------------------------------------------#
####CORRELATIONS####

#should eventually move this to a separate script? treat below as early visualization for 7/24 meeting. 

#some pairs plots colored by environment 

common_wide$environment=factor(paste(common_wide$treatment, common_wide$year, sep = '_'),
                                  levels=c("thick_2013","dry_2013","thick_2014","dry_2014","wet_2014","dry_2015","wet_2015"),
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

pairs(common_wide[,c(6:16,22:24)], pch=21, col=cols)
pairs(common_wide[,c(6:11,17:24)], pch=21, col=cols)
#all
png("./results/pairs_all.png", width=800, height=800)
pairs(common_wide[,c(13,6,22,24,18,8,14,16,10,21,11,12,7,17)], pch=21, col=cols)
dev.off()      

#split into useful groups (hopefully)

#leaf and architecture 
png("./results/pairs_leaf_arch.png", width=900, height=700)
pairs(common.wide[,c(4,6,7,11,12,13,14,3,16)], pch=21, col=cols)
dev.off()

#leaf and devo 
png("./results/pairs_leaf_devo.png", width=900, height=700)
pairs(common.wide.complete[,c(4,6,7,11,12,13,14,8,9,10)], pch=21, col=cols)
dev.off()

#leaf and mass 
png("./results/pairs_leaf_mass.png", width=900, height=700)
pairs(common.wide.complete[,c(4,6,7,11,12,13,14,5,9,15,17,18)], pch=21, col=cols)
dev.off()

#mix
png("./results/pairs_mix_dim.png", width=900, height=700)
pairs(common.wide.complete[,c(6,7,14,8,9,10,3,16,17)], pch=21, col=cols)
dev.off()

png("./results/pairs_mix_elem.png", width=900, height=700)
pairs(common.wide.complete[,c(4,11,12,13,8,9,10,3,16,17)], pch=21, col=cols)
dev.off()

  
  


