#leaf level trait correlations across environments
library(plyr);library(reshape2);library(ggplot2)
load('RIL_BLUP.Rdata')
levels(rils.blups$trait)
leaf=rils.blups[which(rils.blups$trait %in% c("lfblade_area","lfblade_length","lfblade_width",                    
                "lfblade_weight","SLA","sim_gN.m2",                         
                "sim_CN_ratio","sim_gC.m2","stag_gN.m2",                        
                "stag_CN_ratio","stag_gC.m2","d13C",
                "reproductive_vegetative_mass_ratio","leaf_mass_ratio","total_mass_per_DAS","total_mass_at_harvest",
                "leaf_number_cbrt", "basal_circumference", "tiller_number_cbrt")),]
leaf.thick=leaf[-which(leaf.wide$treatment == "sparse"),]
leaf.wide=dcast(leaf, year+experiment+genotype+treatment~trait, value.var="predicted", mean)
leaf.wide$environment=paste(leaf.wide$treatment, leaf.wide$year, sep='_')
leaf.wide2=leaf.wide[-which(leaf.wide$treatment == "sparse"),]
cols<-character(nrow(leaf.wide2))
cols[]<-"black"
cols[leaf.wide2$environment=="dry_2013"]<-"red"
cols[leaf.wide2$environment=="dry_2015"]<-"orange"
cols[leaf.wide2$environment=="thick_2013"]<-"green"
cols[leaf.wide2$environment=="thick_2014"]<-"black"
cols[leaf.wide2$environment=="dry_2014"]<-"pink"
cols[leaf.wide2$environment=="wet_2014"]="blue"
cols[leaf.wide2$environment=="wet_2015"]<-"purple"
cols[leaf.wide2$environment=="sparse_2013"]<-"yellow2"
cols[leaf.wide2$environment=="sparse_2014"]<-"seashell4"
pairs(leaf.wide2[,5:16], col=cols)
pairs(leaf.wide2[,c(5:12,16:21)], col=cols)
pairs(leaf.wide2[,c(5:12,16)], col=cols)
pairs(leaf.wide2[,c(5:12,16,17)], col=cols)
pairs(leaf.wide2[,c(5:12,16,18)], col=cols)
pairs(leaf.wide2[,c(5:12,16,19)], col=cols)
pairs(leaf.wide2[,c(5:12,16,20)], col=cols)
pairs(leaf.wide2[,c(5:12,16,21)], col=cols)
pairs(leaf.wide2[,c(5:12,16,22)], col=cols)
pairs(leaf.wide2[,c(5:8,:22)], col=cols)

leaf.pca=prcomp()

cor.test(leaf.wide$sim_gN.m2, leaf.wide$SLA)
ggplot(leaf.wide, aes(sim_gN.m2, SLA, color=environment)) + geom_point(size=2)+theme_bw()
ggplot(leaf.wide, aes(sim_gN.m2, SLA, color=environment)) + geom_point(size=2)+theme_bw()+geom_smooth(method='lm')
ggplot(leaf.wide, aes(stag_gN.m2, SLA, color=environment)) + geom_point(size=2)+theme_bw()+geom_smooth(method='lm')
ggplot(leaf.wide2, aes(SLA, lfblade_area, color=environment)) + geom_point(size=2)+theme_bw()
ggplot(leaf.wide2, aes(sim_gN.m2, lfblade_area, color=environment)) + geom_point(size=2)+theme_bw()


ggplot(leaf.wide, aes(SLA, total_mass_per_DAS, color=environment)) + geom_point(size=2)+theme_bw()
ggplot(leaf.wide, aes(SLA, total_mass_at_harvest, color=environment)) + geom_point(size=2)+theme_bw()
ggplot(leaf.wide2, aes(SLA, total_mass_at_harvest, color=environment)) + geom_point(size=2)+theme_bw()

ggplot(leaf.wide2, aes(sim_gN.m2, total_mass_at_harvest, color=environment)) + geom_point(size=2)+theme_bw()
ggplot(leaf.wide2, aes(SLA, total_mass_at_harvest, color=environment)) + geom_point(size=2)+theme_bw()


ggplot(leaf.wide2, aes(SLA, basal_circumference, color=environment)) + geom_point(size=2)+theme_bw()
ggplot(leaf.wide2, aes(SLA, tiller_number_cbrt, color=environment)) + geom_point(size=2)+theme_bw()
ggplot(leaf.wide2, aes(tiller_number_cbrt, basal_circumference, color=environment)) + geom_point(size=2)+theme_bw()

#within and across environment correlations
leaf.thick$environment=paste(leaf.thick$treatment, leaf.thick$year, sep='_')
leaf.wide.env=dcast(leaf.thick, genotype~environment+trait, value.var="predicted", mean)
leaf.geno.mean=dcast(leaf.thick, genotype~trait, value.var="predicted", mean)
cv=function (x){
  y=sd(x)/mean(x)
  return(y)
}
leaf.geno.CV=dcast(leaf.thick, genotype~trait, value.var="predicted", cv)
leaf.wide.wide=cbind(leaf.geno.mean,leaf.wide.env)
p1=ggplot(leaf.wide.wide, aes(reorder(genotype,lfblade_area), lfblade_area))+geom_point()+theme(axis.text.x = element_text(angle = 90))
p1+geom_point(aes(y=dry_2013_lfblade_area), color='red')+
  geom_point(aes(y=dry_2015_lfblade_area), color='orange')+
  geom_point(aes(y=dry_2014_lfblade_area), color='pink')+
  geom_point(aes(y=wet_2014_lfblade_area), color='blue')+
  geom_point(aes(y=wet_2015_lfblade_area), color='green')
p2=ggplot(leaf.wide.wide, aes(reorder(genotype,SLA), SLA))+geom_point()+theme(axis.text.x = element_text(angle = 90))
p2+geom_point(aes(y=dry_2013_SLA), color='red')+
  geom_point(aes(y=dry_2015_SLA), color='orange')+
  geom_point(aes(y=dry_2014_SLA), color='pink')+
  geom_point(aes(y=wet_2014_SLA), color='blue')+
  geom_point(aes(y=wet_2015_SLA), color='green')
p3=ggplot(leaf.wide.wide, aes(reorder(genotype,sim_gN.m2), sim_gN.m2))+geom_point()+theme(axis.text.x = element_text(angle = 90))
p3+geom_point(aes(y=dry_2013_sim_gN.m2), color='red')+
  geom_point(aes(y=dry_2015_sim_gN.m2), color='orange')+
  geom_point(aes(y=wet_2015_sim_gN.m2), color='green')
ggplot(leaf.geno.CV, aes(reorder(genotype,lfblade_area), lfblade_area))+geom_point()+theme(axis.text.x = element_text(angle = 90))
ggplot(leaf.geno.CV, aes(reorder(genotype,SLA), SLA))+geom_point()+theme(axis.text.x = element_text(angle = 90))
ggplot(leaf.geno.CV, aes(reorder(genotype,sim_gN.m2), sim_gN.m2))+geom_point()+theme(axis.text.x = element_text(angle = 90))
ggplot(leaf.geno.CV, aes(reorder(genotype,reproductive_vegetative_mass_ratio), reproductive_vegetative_mass_ratio))+geom_point()+theme(axis.text.x = element_text(angle = 90))
ggplot(leaf.geno.CV, aes(reorder(genotype,leaf_mass_ratio), leaf_mass_ratio))+geom_point()+theme(axis.text.x = element_text(angle = 90))
pairs(leaf.geno.CV[2:20])
ggplot(leaf.geno.CV, aes(sim_gN.m2, tiller_number_cbrt))+geom_point()
cor.test(leaf.geno.CV$sim_gN.m2, leaf.geno.CV$tiller_number_cbrt)
cor.test(leaf.geno.CV$sim_gN.m2, leaf.geno.CV$SLA)
cor.test(leaf.geno.CV$sim_gN.m2, leaf.geno.CV$total_mass_per_DAS)
cor.test(leaf.geno.CV$SLA, leaf.geno.CV$total_mass_per_DAS)
cor.test(leaf.geno.CV$d13C, leaf.geno.CV$sim_gN.m2)
cor.test(leaf.geno.CV$d13C, leaf.geno.CV$SLA)
ggplot(leaf.geno.CV, aes(d13C, sim_gN.m2))+geom_point()
