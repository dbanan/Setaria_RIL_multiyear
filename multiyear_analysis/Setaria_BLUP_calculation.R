#Setaria_BLUP_calculation.R 





library(ggplot2)
library(lme4)
library(nlme)


#need a better way of getting r object into working environment (regardless of user/machine)
#write a function later?
load("/de/github/dbanan/auth/Setaria_RIL_multiyear/data_wrangle/plant_level/data/harvest_phenotypes_clean_transformed.Rdata")


parent.data=all_stack2[which(all_stack2$genotype %in% c('A10','B100')),]
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

all<-all_stack2

#change 2013 dry to wet 
all$treatment[all$year==2013 & all$treatment=="dry"]<-"wet"


#make environment column
all$environment=paste(all$experiment,all$year, sep='_')

#pull out plot id
for (i in 1:nrow(all)){
  if (all$year[i] %in% c(2015)){
    all$plot[i]=substr(all$subplot_id[i],2,3)
  }
  else {
    all$plot[i]=substr(all$subplot_id[i],1,2)
  }
}
#what are our environments? 
unique(all$environment)




exp.combos=c("density_2013","drought_2013", "drought_2014","density_2014","drought_2015")
parent.blup.all<-c()
rils.blups<-c()
for(d in 1:length(exp.combos)){
  temp=all[all$environment == exp.combos[d],]
  traits=unique(temp$trait)
    pdf(paste('./results/',exp.combos[d],'_blups.pdf', sep=""))
    for(k in 1:length(traits)){
      parent.data=temp[which(temp$genotype %in% c('A10','B100') & temp$trait==traits[k]),]
      model=lmer(data ~ (1|genotype) + treatment + plot %in% treatment, data=parent.data)
      parent.data=parent.data[complete.cases(parent.data),]
      parent.data$predicted=predict(model)
      boxplot(parent.data$predicted~parent.data$treatment+parent.data$genotype, xlab=paste(exp.combos[d],'model',sep='.'), ylab=traits[k])
      parent.data$residual=residuals(model)
      qqnorm(parent.data$residual)
      parent.blup.all=rbind(parent.blup.all, parent.data)
    }
    dev.off()
  }







exp.combos=c("density_2013", "drought_2013","drought_2014","density_2014","drought_2015")
parent.blup.all<-c()
rils.blups<-c()
for(d in 1:length(exp.combos)){
  temp=all[all$environment == exp.combos[d],]
  traits=unique(temp$trait)
  pdf(paste('./results/',exp.combos[d],'_blups.pdf', sep=""))
  for(k in 1:length(traits)){
    ril.data=temp[which(temp$trait==traits[k]),]
    model=lmer(data ~ (1|genotype) + treatment + plot %in% treatment, data=ril.data)
    ril.data=ril.data[complete.cases(ril.data),]
    ril.data$predicted=predict(model)
    boxplot(ril.data$predicted~ril.data$treatment+ril.data$genotype, xlab=paste(exp.combos[d],'model',sep='.'), ylab=traits[k])
    ril.data$residual=residuals(model)
    qqnorm(ril.data$residual)
    rils.blups=rbind(rils.blups, ril.data)
  }
  dev.off()
}



#output BLUP values for further analysis 
save(rils.blups, file="RIL_BLUP.Rdata")




