#Setaria_BLUP_calculation.R - Step 2

library(tidyverse)
library(lme4)
#library(nlme)

#load merged leaf-plant data 
load("./merged_phenotypes.Rdata")
#### Parent BLUP test for correct model ####
#pull out parent data to use for linear modeling
parent.data=lp[which(lp$genotype %in% c('A10','B100')),]
parent.data$environment=paste(parent.data$experiment,parent.data$year, sep='_')
for (i in 1:nrow(parent.data)){
  if (parent.data$year[i] %in% c(2015)){
    parent.data$plot[i]=substr(parent.data$subplot_id[i],2,3)
  }
  else {
    parent.data$plot[i]=substr(parent.data$subplot_id[i],1,2)
  }
}

table(parent.data$trait)
table(parent.data$environment)

exp.combos=c("drought_2013","drought_2014","drought_2015")
trait=c('panicle_mass','leaf_mass','stem_mass','culm_height','panicle_emergence_DAS')
parent.blup<-c()
pdf('./results/parent_blups.pdf')
for(d in 1:length(exp.combos)){
  for(k in 1:length(trait)){
    temp=parent.data[parent.data$environment == exp.combos[d] & parent.data$trait==trait[k],]
    model=lmer(data ~ (1|genotype) + treatment + plot %in% treatment, data=temp)
    assign(paste(exp.combos[d],'model',sep='.'),model)
    temp=temp[complete.cases(temp),]
    temp$predicted=predict(model)
    boxplot(temp$predicted~temp$treatment+temp$genotype, xlab=paste(exp.combos[d],'model',sep='.'), ylab=trait[k])
    temp$residual=residuals(model)
    qqnorm(temp$residual)
    parent.blup=rbind(parent.blup, temp)
  }
}
dev.off()

#now that we've looked at the parents for a few key traits and decided on environments,
#we need to run a blup model for all the traits measured across the experiments within each environment
#--------------------------------------------------------------------------------------------------------#
#### Preparing RIL data ####
#removing 2013 drought well-watered data because the research team decided to omit that data from the analysis
all=lp[-which(lp$year==2013 & lp$treatment=='wet'),]
all=all[-which(all$trait %in% c('leaf_mass_per_DAS','stem_mass_per_DAS','panicle_mass_per_DAS','total_mass_per_DAS','vegetative_mass_per_DAS')),]
all$value[50616]=NA
#tranforming traits that have issues with residuals
all$new_value=ifelse(all$trait %in% c("leaf_mass_at_harvest","panicle_mass_at_harvest","stem_mass_at_harvest",
                                      "vegetative_mass_at_harvest","total_mass_at_harvest","leaf_number",'tiller_number','panicle_number',
                                      'lfblade_area', 'lfblade_weight', 'lfblade_width','sim_lfblade_area', 'sim_lfblade_weight', 'sim_lfblade_width', 
                                      'stag_lfblade_area', 'stag_lfblade_weight', 'stag_lfblade_width','basal_circumference'), log10(all$value),all$value)
which(is.infinite(all$new_value))

str(all)
head(all)
#converting all the exp.design columns to factors
all$year=factor(all$year)
all$experiment=factor(all$experiment)
all$trait=factor(all$trait)
all$treatment=factor(all$treatment)
all$environment=factor(all$environment)
all$plot=factor(all$plot)

#what are our environments? 
unique(all$environment)
#levels(all$environment)
#### Running BLUP model for A10 and B100 for all traits ####
exp.combos=c("density_2013","density_2014","drought_2014","drought_2015")
parent.blup.all<-c()
for(d in 1:length(exp.combos)){
  temp=all[all$environment == exp.combos[d],]
  traits=unique(temp$trait)
    pdf(paste('./results/',exp.combos[d],'_parent_blups.pdf', sep=""))
    for(k in 1:length(traits)){
      parent.data=temp[which(temp$genotype %in% c('A10','B100') & temp$trait==traits[k]),]
      model=lmer(value ~ (1|genotype) + treatment + plot %in% treatment, data=parent.data)
      parent.data=parent.data[complete.cases(parent.data),]
      parent.data$predicted=predict(model)
      boxplot(parent.data$predicted~parent.data$treatment+parent.data$genotype, xlab=paste(exp.combos[d],'model',sep='.'), ylab=traits[k])
      parent.data$residual=residuals(model)
      qqnorm(parent.data$residual)
      parent.blup.all=rbind(parent.blup.all, parent.data)
    }
  dev.off()
}

#### Running BLUP model for RILs for all traits ####
#all experiments
exp.combos=c("density_2013","density_2014","drought_2014","drought_2015")
rils.blups<-c()
for(d in 1:length(exp.combos)){
  temp=all[all$environment == exp.combos[d],]
  traits=unique(temp$trait)
  #pdf(paste('./results/',exp.combos[d],'_rilblups_GxE.pdf', sep=""))
  for(k in 1:length(traits)){
    timestamp()
    print(c(exp.combos[d],traits[k]))
    ril.data=temp[which(temp$trait==traits[k]),]
    if (nrow(ril.data)==0){
      next
    }
    model=lmer(new_value ~ (1|genotype) + treatment +plot%in%treatment+(1|genotype:treatment), data=ril.data)
    ril.data=ril.data[which(is.na(ril.data$value)==FALSE),]
    ril.data$predicted=predict(model)
    boxplot(ril.data$predicted~ril.data$treatment+ril.data$genotype, xlab=paste(exp.combos[d],'model',sep='.'), ylab=traits[k])
    ril.data$residual=residuals(model)
    qqnorm(ril.data$residual)
    qqline(ril.data$residual)
    plot(ril.data$predicted, ril.data$residual, main = traits[k])
    abline(h=0)
    rils.blups=rbind(rils.blups, ril.data)
  }
  #dev.off()
}

#just 2013 dry. It has different model since there is no treatment
pdf(paste('./results/','drought_2013','_rilblups_GxE.pdf', sep=""))
temp=all[all$environment == 'drought_2013',]
traits=unique(temp$trait)
for(k in 1:length(traits)){
  print(traits[k])
  ril.data=temp[which(temp$trait==traits[k]),]
  if (nrow(ril.data)==0){
    next
  }
  model=lmer(new_value ~ (1|genotype) + plot , data=ril.data)
  ril.data=ril.data[which(is.na(ril.data$value)==FALSE),]
  ril.data$predicted=predict(model)
  boxplot(ril.data$predicted~ril.data$genotype, xlab=paste(exp.combos[d],'model',sep='.'), ylab=traits[k])
  ril.data$residual=residuals(model)
  qqnorm(ril.data$residual)
  qqline(ril.data$residual)
  plot(ril.data$predicted, ril.data$residual, main = traits[k])
  abline(h=0)
  rils.blups=rbind(rils.blups, ril.data)
  }
dev.off()
#outputing descriptive plots for BLUPs
pdf('./results/Set_RIL_BLUP_redidualsvsfitted_gxe.pdf',height=16, width = 24)
ggplot(rils.blups[which(rils.blups$environment=='density_2013'),], aes(predicted, residual, color=treatment))+geom_point()+geom_hline(yintercept = 0)+facet_wrap(~trait, scales = 'free')+ggtitle('density 2013')+ theme(legend.position="bottom")
ggplot(rils.blups[which(rils.blups$environment=='density_2014'),], aes(predicted, residual, color=treatment))+geom_point()+geom_hline(yintercept = 0)+facet_wrap(~trait, scales = 'free')+ggtitle('density 2014')+ theme(legend.position="bottom")
ggplot(rils.blups[which(rils.blups$environment=='drought_2013'),], aes(predicted, residual, color=treatment))+geom_point()+geom_hline(yintercept = 0)+facet_wrap(~trait, scales = 'free')+ggtitle('drought 2013')+ theme(legend.position="bottom")
ggplot(rils.blups[which(rils.blups$environment=='drought_2014'),], aes(predicted, residual, color=treatment))+geom_point()+geom_hline(yintercept = 0)+facet_wrap(~trait, scales = 'free')+ggtitle('drought 2014')+ theme(legend.position="bottom")
ggplot(rils.blups[which(rils.blups$environment=='drought_2015'),], aes(predicted, residual, color=treatment))+geom_point()+geom_hline(yintercept = 0)+facet_wrap(~trait, scales = 'free')+ggtitle('drought 2015')+ theme(legend.position="bottom")
dev.off()
pdf('./results/Set_RIL_BLUP_measuredvsfitted.pdf',height=16, width = 24)
ggplot(rils.blups[which(rils.blups$environment=='density_2013'),], aes(value, predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('density 2013')+ theme(legend.position="bottom")
ggplot(rils.blups[which(rils.blups$environment=='density_2014'),], aes(value, predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('density 2014')+ theme(legend.position="bottom")
ggplot(rils.blups[which(rils.blups$environment=='drought_2013'),], aes(value, predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('drought 2013')+ theme(legend.position="bottom")
ggplot(rils.blups[which(rils.blups$environment=='drought_2014'),], aes(value, predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('drought 2014')+ theme(legend.position="bottom")
ggplot(rils.blups[which(rils.blups$environment=='drought_2015'),], aes(value, predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('drought 2015')+ theme(legend.position="bottom")
dev.off()
#removing redundant traits
rils.blups2=rils.blups[-which(rils.blups$trait %in% c("leaf_number_cbrt","panicle_number_cbrt","tiller_number_cbrt", 'branch_number')),]
rils.blups2$trait=factor(rils.blups2$trait,levels=c("lfblade_area","lfblade_length","lfblade_width","lfblade_weight",'sim_lfblade_area', 'sim_lfblade_weight', 'sim_lfblade_width',"sim_lfblade_length", 
                                                    'stag_lfblade_area', 'stag_lfblade_weight', 'stag_lfblade_width',"stag_lfblade_length","SLA",
                                    "sim_SLA",'stag_SLA',"sim_gN.m2","gN.m2","stag_gN.m2","gC.m2","sim_gC.m2","stag_gC.m2",
                                    "CN_ratio","sim_CN_ratio","stag_CN_ratio","d13C",'sim_d13C',"stag_d13C",
                                    "panicle_emergence_DAS","vegetative_mass_at_harvest","leaf_mass_at_harvest","stem_mass_at_harvest","panicle_mass_at_harvest","total_mass_at_harvest",
                                    "reproductive_vegetative_mass_ratio","leaf_mass_ratio","culm_height","branch_number_cbrt","leaf_number","panicle_number","panicle_length",
                                    "tiller_height","tiller_number","basal_circumference","dead_percent", "green_percent","leaf_number_dead","leaf_number_green" ), ordered = T)

rils.blups2$new_predicted=ifelse(rils.blups2$trait%in% c("leaf_mass_at_harvest","panicle_mass_at_harvest","stem_mass_at_harvest",
                                                         "vegetative_mass_at_harvest","total_mass_at_harvest","leaf_number",'tiller_number','panicle_number',
                                                         'lfblade_area', 'lfblade_weight', 'lfblade_width','sim_lfblade_area', 'sim_lfblade_weight', 'sim_lfblade_width', 
                                                         'stag_lfblade_area', 'stag_lfblade_weight', 'stag_lfblade_width','basal_circumference'), 10^(rils.blups2$predicted),rils.blups2$predicted)
pdf('./results/Set_RIL_BLUP_measuredvsfitted_backtrans.pdf',height=16, width = 24)
ggplot(rils.blups2[which(rils.blups2$environment=='density_2013'),], aes(value, new_predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('density 2013')+ theme(legend.position="bottom")
ggplot(rils.blups2[which(rils.blups2$environment=='density_2014'),], aes(value, new_predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('density 2014')+ theme(legend.position="bottom")
ggplot(rils.blups2[which(rils.blups2$environment=='drought_2013'),], aes(value, new_predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('drought 2013')+ theme(legend.position="bottom")
ggplot(rils.blups2[which(rils.blups2$environment=='drought_2014'),], aes(value, new_predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('drought 2014')+ theme(legend.position="bottom")
ggplot(rils.blups2[which(rils.blups2$environment=='drought_2015'),], aes(value, new_predicted, color=treatment))+geom_point()+facet_wrap(~trait, scales = 'free')+ggtitle('drought 2015')+ theme(legend.position="bottom")
dev.off()
pdf('./results/Set_RIL_field_data_summary_boxplots_BLUPS.pdf', height = 12, width = 14)
ggplot(rils.blups2, aes(x=interaction(year, experiment), y=predicted, group=interaction(year,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c('red', 'goldenrod1', 'mediumorchid4', 'dodgerblue'))+
  facet_wrap(~trait, scale="free_y")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(rils.blups2[-which(rils.blups2$treatment=='sparse'),], aes(x=interaction(year, experiment), y=predicted, group=interaction(year,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c('red', 'mediumorchid4', 'dodgerblue'))+
  facet_wrap(~trait, scale="free_y")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
pdf('./results/Set_RIL_field_data_summary_boxplots_BLUPS_backtrans.pdf', height = 12, width = 14)
ggplot(rils.blups2, aes(x=interaction(year, experiment), y=new_predicted, group=interaction(year,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c('red', 'goldenrod1', 'mediumorchid4', 'dodgerblue'))+
  facet_wrap(~trait, scale="free_y")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(rils.blups2[-which(rils.blups2$treatment=='sparse'),], aes(x=interaction(year, experiment), y=new_predicted, group=interaction(year,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c('red', 'mediumorchid4', 'dodgerblue'))+
  facet_wrap(~trait, scale="free_y")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

####PRODUCT####
#output BLUP values for further analysis
rils.blups2=rils.blups2[!duplicated(rils.blups2),]
save(rils.blups2, file="RIL_BLUP.Rdata")
##END##