#rerunning trait space analysis without 2013 data after having done some initial PCAs to understand data

library(FactoMineR)
library(ggbiplot)
library(tidyverse)
library(GGally)
####Data prep####
#load BLUPs
load("RIL_BLUP.Rdata")
#just on removing low density plants from the dataset 
just_thick<-rils.blups[-which(rils.blups$treatment=="sparse"),]
table(just_thick$year)
env.subset=just_thick[-which(just_thick$year==2013),]
table(env.subset$environment)
#lists for just common traits (version for many environments but few traits; few environments but many traits)
trait.matrix=as.data.frame.array(table(env.subset$trait,as.character(env.subset$environment)))
trait.check=apply(trait.matrix,1,function(i) any(i == 0))
common=names(which(trait.check ==F))
just_common<-subset(env.subset, trait %in% common)
#identifying genotypes planted in environments of interest
geno.table=as.data.frame.array(table(env.subset$genotype,as.character(env.subset$environment)))
geno.check=apply(geno.table,1,function(i) any(i == 0))
common.geno=names(which(geno.check ==F))
#subsetting data to just the genotypes that were planted in environments of interest
common_geno=subset(just_common, genotype %in% common.geno)
#need genotype*treatment averages (on BLUP predicted values)
geno_means=common_geno%>%group_by(year, experiment, treatment, genotype, trait)%>%summarise(value=mean(predicted, na.rm=T))
geno_means=geno_means[-which(geno_means$trait %in% c('vegetative_mass_per_DAS','leaf_mass_per_DAS','tiller_number',
                                                     'stem_mass_per_DAS','panicle_mass_per_DAS','total_mass_per_DAS')),]
#outputting boxplots of common traits for common genotypes across environments
png('./results/Set_RIL_field_20142015_common_traits_common_genos_summary_boxplots.png',width=900, height=700)
ggplot(geno_means, aes(x=year, y=value, group=interaction(year,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c('red', 'mediumorchid4', 'dodgerblue'))+
  facet_wrap(~trait, scale="free")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+
  theme(rect = element_rect(fill = "transparent"))
dev.off()
#wide by trait and still long genotype and environment
#common_wide<-dcast(common_geno, year+experiment+genotype+treatment~trait, value.var="data")
common_wide=spread(geno_means, key=trait, value=value)
common_wide$environment=paste(common_wide$treatment,common_wide$year, sep="_")
#removing lines that are missing values for the common traits
common.wide.complete<-common_wide[complete.cases(common_wide),]
colnames(common.wide.complete)
#pull out environment column 
env<-common.wide.complete$environment
geno=common.wide.complete$genotype
colnames(common.wide.complete)
####PCA just to look at stuff quickly####
pca.matrix=as.matrix(common.wide.complete[,5:21])
colnames(pca.matrix)
rownames(pca.matrix)=paste(common.wide.complete$genotype, common.wide.complete$environment, sep="_")

PCA_multiyear<-prcomp(pca.matrix, center=TRUE, scale.=TRUE)
biplot(PCA_multiyear)
plot(PCA_multiyear)
plot(PCA_multiyear, type="l")
#visualize
ggbiplot(PCA_multiyear, choices=c(1,2),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(PCA_multiyear, choices=c(1,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)
ggbiplot(PCA_multiyear, choices=c(2,3),labels.size = 2,obs.scale=1,varname.adjust = 1, var.scale=1, groups=env, ellipse=TRUE)

####MFA####
colnames(common.wide.complete)
try=MFA(common.wide.complete[,c(22,4,5:12,20,21,13:17,18,19)], group = c(1,1,6,4,7), type = c('n','n','s','s','s'), 
        name.group = c('environment','genotype','leaf','architecture','biomass'),
        num.group.sup=c(1,2))
barplot(try$eig[,1],main="Eigenvalues",names.arg=1:nrow(try$eig))
plotellipses(try, cex=1.5)

library(factoextra)
fviz_contrib(try, "group", axes = 1)
fviz_contrib(try, "group", axes = 2)
fviz_contrib(try, "group", axes = 3)
fviz_mfa_var(try, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)
fviz_mfa_var(try, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")
fviz_contrib(try, choice = "quanti.var", axes = 1,
             palette = "jco")
fviz_contrib(try, choice = "quanti.var", axes = 2,
             palette = "jco")
fviz_contrib(try, choice = "quanti.var", axes = 3,
             palette = "jco")
fviz_mfa_var(try, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))
fviz_mfa_axes(try)
fviz_mfa_ind(try, partial = "all") 
fviz_ellipses(try,c('environment','genotype'))
