#Setaria_environment_phenotype_correlation


library(corrplot)

#correlations between environment PCA results and phenotype PCA results 


#load environmental PCA averages 
load("../supplemental_analysis/environmental_data/results/envPCavg.Rdata")

colnames(envPCavg)[3]<-"env_pred_avg"
colnames(envPCavg)[4]<-"env_pred_stderr"

#load phenotype PCA averages 
load("./pheno_PCA_predictions.Rdata")


##########RANK ORDER PCA###########
#rank order PC
pheno_pred<-pca_DAS_x

pheno_pred$PC1<-as.numeric(as.character(pheno_pred$PC1))
pheno_pred$PC2<-as.numeric(as.character(pheno_pred$PC2))
pheno_pred$PC3<-as.numeric(as.character(pheno_pred$PC3))


pheno_predl<-melt(pheno_pred, id.vars=c("name"), measure.vars=c("PC1","PC2","PC3"), variable.name="component", value.name="data", factorsAsStrings=F) 

#break "name" into plottable attributes 
library(tidyr)
pheno_predl<-separate(pheno_predl, name, into=c("geno", "id", "experiment", "year", "treatment"), sep="_")



pheno_predl1<-pheno_predl[-which(pheno_predl$geno %in% c("A10","B100")),]
pheno_predl2<-subset(pheno_predl, geno %in% c("A10", "B100"))

pheno_predl1$genotype<-paste(pheno_predl1$geno, pheno_predl1$id, sep="_")
pheno_predl1$environment<-paste(pheno_predl1$treatment, pheno_predl1$year, sep="_")
pheno_predl1<-pheno_predl1[,c(8,9,6,7)]

pheno_predl2$genotype<-pheno_predl2$geno
pheno_predl2$environment<-paste(pheno_predl2$year, pheno_predl2$experiment, sep="_")
pheno_predl2<-pheno_predl2[,c(8,9,6,7)]

pheno_predl3<-rbind(pheno_predl2, pheno_predl1)

###global environment mean pheno-PC 
global_env_pheno<-ddply(pheno_predl3, c("environment", "component"), summarise, pheno_pred_avg=mean(data))




#join env and pheno predicted averages 
pred_both<-join(global_env_pheno, envPCavg)

predl<-melt(pred_both, id.vars=c("environment","component"), measure.vars=c("pheno_pred_avg","env_pred_avg"), variable.name="source", value.name="data")
  
predl$pred_name<-paste(predl$source, predl$component, sep="_")
  
predw<-dcast(predl, environment~pred_name, value.var="data")

pairs(predw[,c(2:7)], pch=21, col=cols)
pairs(predw[,c(2:7)], panel=panel.lm, pch=21, col=cols)

cols<-character(nrow(predw))
cols[]<-"black"
cols[predw$environment=="dry_2013"]<-"red"
cols[predw$environment=="dry_2014"]<-"orange"
cols[predw$environment=="dry_2015"]<-"green"
cols[predw$environment=="thick_2013"]<-"light blue"
cols[predw$environment=="thick_2014"]<-"blue"
cols[predw$environment=="wet_2014"]="purple"
cols[predw$environment=="wet_2015"]<-"pink"

png("./results/env_pheno_PCavg_pairs.png", width=700, height=700)
pairs(predw[,c(2:7)], pch=21, col=cols)
dev.off()

panel.lm <- function (x, y,  pch = par("pch"), col.lm = "black",  ...) {   
  ymin <- min(y)
  ymax <- max(y)
  xmin <- min(x)
  xmax <- max(x)
  ylim <- c(min(ymin,xmin),max(ymax,xmax))
  xlim <- ylim
  points(x, y, pch = pch,ylim = ylim, xlim= xlim,...)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    abline(lm(y[ok]~ x[ok]), 
           col = col.lm, ...)
}

pred_cor<-cor(predw[,c(2:7)])
corrplot.mixed(pred_cor, upper="ellipse")


png("./results/env_pheno_PCavg_corr.png", width=700, height=700)
corrplot.mixed(pred_cor, upper="ellipse")
dev.off()





#not quite right...

ggplot(data=pred_both, aes(x=env_pred_avg, y=pheno_pred_avg))+geom_point()+facet_wrap(~component)









###global genotype mean PC
global<-ddply(pheno_predl3, c("genotype", "component"), summarise, avg=mean(data), covar=(sd(data)/mean(data)))

#merge with individual values 
pheno_predl4<-join(pheno_predl3, global)

globalw<-dcast(pheno_predl4, genotype+component+avg~environment, value.var="data")
globalw$component<-as.character(as.factor(globalw$component))

globalw1<-globalw %>% filter(component == "PC1")
globalw11<-globalw1 %>% mutate(genotype=reorder(genotype, avg))
png("./results/rankPhenoPC1_env.png", width=900, height=550)
ggplot(data=globalw11)+geom_point(aes(x=genotype, y=avg))+
  geom_point(aes(x=genotype, y=dry_2013), color="red")+
  geom_point(aes(x=genotype, y=dry_2014), color="orange")+
  geom_point(aes(x=genotype, y=dry_2015), color="green")+
  geom_point(aes(x=genotype, y=thick_2013), color="light blue")+
  geom_point(aes(x=genotype, y=thick_2014), color="blue")+
  geom_point(aes(x=genotype, y=wet_2014), color="purple")+
  geom_point(aes(x=genotype, y=wet_2015), color="pink")+
  theme_bw()
dev.off()


globalw2<-subset(globalw, component %in% "PC2")
globalw22<-globalw2 %>% mutate(genotype=reorder(genotype, avg))
png("./results/rankPhenoPC2_env.png", width=900, height=550)
ggplot(data=globalw22)+geom_point(aes(x=genotype, y=avg))+
  geom_point(aes(x=genotype, y=dry_2013), color="red")+
  geom_point(aes(x=genotype, y=dry_2014), color="orange")+
  geom_point(aes(x=genotype, y=dry_2015), color="green")+
  geom_point(aes(x=genotype, y=thick_2013), color="light blue")+
  geom_point(aes(x=genotype, y=thick_2014), color="blue")+
  geom_point(aes(x=genotype, y=wet_2014), color="purple")+
  geom_point(aes(x=genotype, y=wet_2015), color="pink")+
  theme_bw()
dev.off()

globalw3<-subset(globalw, component %in% "PC3")
globalw33<-globalw3 %>% mutate(genotype=reorder(genotype, avg))
png("./results/rankPhenoPC3_env.png", width=900, height=550)
ggplot(data=globalw33)+geom_point(aes(x=genotype, y=avg))+
  geom_point(aes(x=genotype, y=dry_2013), color="red")+
  geom_point(aes(x=genotype, y=dry_2014), color="orange")+
  geom_point(aes(x=genotype, y=dry_2015), color="green")+
  geom_point(aes(x=genotype, y=thick_2013), color="light blue")+
  geom_point(aes(x=genotype, y=thick_2014), color="blue")+
  geom_point(aes(x=genotype, y=wet_2014), color="purple")+
  geom_point(aes(x=genotype, y=wet_2015), color="pink")+
  theme_bw()
dev.off()










globalw1$geno<-factor(globalw1$genotype, levels=global1$genotype[order(globalw1$avg)])

globalw1<-globalw1[order(globalw1$avg),]
globalw1$geno<-as.factor(as.character(globalw1$genotype))
ggplot(data=globalw1, aes(x=geno, y=avg))+geom_point()


global1<-global[order(global[,"component"], global[,"avg"]),]
global1$genotype1<-as.factor(global1$genotype)


global1<-subset(global, component=="PC1")
global1$geno<-factor(global1$genotype, levels=global1$genotype[order(global1$avg)])

ggplot(data=global1, aes(x=geno, y=avg))+geom_point()












pheno_predl4<-pheno_predl4[order(pheno_predl4[,3], pheno_predl4[,5]),]
pheno_predl4$genotype1<-as.factor(pheno_predl4$genotype)
ggplot(data=pheno_predl4, aes(x=genotype1, y=data))+geom_smooth(aes(group=environment, color=environment))+facet_wrap(~component)

