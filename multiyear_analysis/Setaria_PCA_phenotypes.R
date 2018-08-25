#Setaria_PCA_phenotypes.R 



#6/12/18
#migrate analysis from May data meeting 
#6/26/18
#now on git, this script (fingers crossed) should run smoothly from the BLUP calculation output 
library(tidyverse)
library(GGally)
#library(plyr)
#library(reshape2)
library(FactoMineR)
library(ggbiplot)
####Data prep####
#load BLUPs
load("RIL_BLUP.Rdata")
#just on removing low density plants from the dataset 
just_thick<-rils.blups[-which(rils.blups$treatment=="sparse"),]
table(just_thick$year)

#lists for just common traits (version for many environments but few traits; few environments but many traits)
trait.matrix=as.data.frame.array(table(just_thick$trait,just_thick$environment))
trait.check=apply(trait.matrix,1,function(i) any(i == 0))
common=names(which(trait.check ==F))
just_common<-subset(just_thick, trait %in% common)

less.common=trait.matrix[-which(rownames(trait.matrix) %in% common),]
geno.table=as.data.frame.array(table(just_thick$genotype,just_thick$environment))
geno.check=apply(geno.table,1,function(i) any(i == 0))
common.geno=names(which(geno.check ==F))
#need genotype*treatment averages (on BLUP predicted values)
#common_geno<-ddply(just_common, c("year", "experiment",'environment', "treatment", "genotype", "trait"), summarise, data=mean(predicted, na.rm=TRUE))
geno_means=just_common%>%group_by(year, experiment, treatment, genotype, trait)%>%summarise(value=mean(value, na.rm=T))
#subsetting genotype means to just the lines that were planted in all environments
common_geno=subset(geno_means, genotype %in% common.geno)

#outputting boxplots of common traits for common genotypes across environments
png('./results/Set_RIL_field_common_traits_common_genos_summary_boxplots.png',width=900, height=700)
ggplot(common_geno, aes(x=year, y=value, group=interaction(year,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c('red', 'mediumorchid4', 'dodgerblue'))+
  facet_wrap(~trait, scale="free")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+
  theme(rect = element_rect(fill = "transparent"))
dev.off()
#wide by trait and still long genotype and environment
#common_wide<-dcast(common_geno, year+experiment+genotype+treatment~trait, value.var="data")
common_wide=spread(common_geno, key=trait, value=value)
common_wide$environment=paste(common_wide$treatment,common_wide$year, sep="_")
#removing lines that are missing values for the common traits
common.wide.complete<-common_wide[complete.cases(common_wide),]
colnames(common.wide.complete)
save(common.wide.complete, file = 'Set_multiyear_common_traits_geno_means.Rdata')
#pull out environment column 
env<-common.wide.complete$environment
geno=common.wide.complete$genotype

####PCA BLUP######
#PCA run throughs to get decide the appropriate traits to include and which way to report plant biomass
{
#PCA with plant traits at harvest, rather than as DAS
pca.matrix1=common.wide.complete[,c(5:15,21:23)]
#pca.matrix1=common.wide.complete[,c(5:15,21,23)]
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
biplot(pca_try)

pca_try_table<-rbind(pca_try$rotation, pca_try$sdev)

#visualize
ggbiplot(pca_try, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_try, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_try, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(pca_try, choices=c(1,4),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)

png("./results/PC12.png", width=900, height=700)
ggbiplot(pca_try, choices=c(1,2),labels.size = 2,varname.adjust = 1,obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)+
  theme_bw()+ylim(c(-3.5,6.5))
dev.off()

png("./results/PC13.png", width=900, height=700)
ggbiplot(pca_try, choices=c(1,3),labels.size = 2,varname.adjust = 1,obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)+
  theme_bw()+ylim(c(-3.5,6.5))
dev.off()

png("./results/PC23.png", width=900, height=700)
ggbiplot(pca_try, choices=c(2,3),labels.size = 2,varname.adjust = 1,obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)+
  theme_bw()+ylim(c(-3.5,6.5))
dev.off()

#PCA with plant harvest traits as DAS
pca.matrix2=common.wide.complete[,c(5:10,16:23)]

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
ggbiplot(pca_DAS, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)+
  theme_bw()
dev.off()
png("./results/PC13_DAS.png", width=900, height=700)
ggbiplot(pca_DAS, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)+
  theme_bw()
dev.off()
png("./results/PC23_DAS.png", width=900, height=700)
ggbiplot(pca_DAS, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)+
  theme_bw()
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
}
#PCA with plant harvest traits as masses at harvest and without LMR
colnames(common.wide.complete)
pca.matrix2=as.matrix(common.wide.complete[,c(5:15,21,23)])
#pca.matrix2=common.wide.complete[,c(5:10,16:21,23)]
colnames(pca.matrix2)
rownames(pca.matrix2)=paste(common.wide.complete$genotype, common.wide.complete$environment, sep="_")

PCA_multiyear<-prcomp(pca.matrix2, center=TRUE, scale.=TRUE)

#PCA_multiyear<-PCA(pca.matrix2)
#summary(PCA_multiyear,nbelements=12, file = './results/PCA_multiyear_summary_results.txt')
#write.table(PCA_multiyear$var$coord, file = './results/PCA_multiyear_trait_eigenvalues.csv', row.names = F)
#HCPC(PCA_multiyear, nb.clust = -1, min=4)

PCA_multiyear$sdev
PCA_multiyear$rotation
PCA_multiyear$x
plot(PCA_multiyear)
plot(PCA_multiyear, type="l")

plot(PCA_multiyear$x[,1:2])
biplot(PCA_multiyear)

PCA_multiyear_table<-rbind(PCA_multiyear$rotation, PCA_multiyear$sdev)
write.csv(PCA_multiyear_table, file = './results/PCA_multiyear_trait_eigenvalues.csv')

#output PC predictions
pca_multiyear_x<-as.data.frame(cbind(name=rownames(PCA_multiyear$x), PCA_multiyear$x))
save(pca_multiyear_x, file="./pheno_PCA_predictions.Rdata")

#visualize
ggbiplot(PCA_multiyear, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(PCA_multiyear, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(PCA_multiyear, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(PCA_multiyear, choices=c(1,4),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)

png("./results/PC12_21aug18.png", width=900, height=700)
ggbiplot(PCA_multiyear, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
dev.off()
png("./results/PC13_21aug18.png", width=900, height=700)
ggbiplot(PCA_multiyear, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
dev.off()
png("./results/PC23_21aug18.png", width=900, height=700)
ggbiplot(PCA_multiyear, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
dev.off()




#----------------------------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------------------------#
####Heirarchical clustering####
library(dendextend);library(circlize)
hclust.matrix=common.wide.complete[,c(5:10,16:20,23)]
rownames(hclust.matrix)=paste(common.wide.complete$genotype, common.wide.complete$treatment, common.wide.complete$year, sep="_")
heir.clust=hclust(dist(hclust.matrix))
#colors for the genotype labels on the dendrogram
lab.color=matrix("black",nrow(hclust.matrix))
rownames(lab.color)=rownames(hclust.matrix)
lab.color<-character(nrow(common.wide.complete))
lab.color[]<-"black"
lab.color[which(common.wide.complete$environment=="dry_2013")]="red"
lab.color[which(common.wide.complete$environment=="dry_2015")]="orange"
lab.color[which(common.wide.complete$environment=="thick_2013")]="green"
lab.color[which(common.wide.complete$environment=="thick_2014")]="black"
lab.color[which(common.wide.complete$environment=="dry_2014")]="pink"
lab.color[which(common.wide.complete$environment=="wet_2014")]="blue"
lab.color[which(common.wide.complete$environment=="wet_2015")]="purple"
#plotting the dendrogram in a way that it can be colored and the arrangement can be changed
dend=as.dendrogram(heir.clust)
labels_colors(dend)=lab.color[order.dendrogram(dend)]
plot(dend, cex=0.5)
circlize_dendrogram(dend,labels_track_height = NA)


#### Multiple factor analysis####
#trying something new
colnames(common.wide.complete)
mfa.data=common.wide.complete[,c(1,4,5:10,23,21,11:15)]
colnames(mfa.data)
try=MFA(mfa.data, group = c(2,4,3,6), type = c('n','s','s','s'), 
        name.group = c('environment','leaf','architecture','biomass'),
        num.group.sup=c(1))
barplot(try$eig[,1],main="Eigenvalues",names.arg=1:nrow(try$eig))
plotellipses(try)
summary(try)
plot(try)
plot(try, choix = 'axes')
plot(try, choix = 'axes', axes = c(1,3))
plot(try, choix = 'axes', axes = c(2,3))
plot(try, choix = 'group')
plot(try, choix = 'group', axes = c(1,3))
plot(try, choix = 'group', axes = c(2,3))
plot(try, choix = 'ind')
plot(try, choix = 'var')
plot(try, choix = 'var', axes = c(1,3))
plot(try, choix = 'var', axes = c(2,3))

