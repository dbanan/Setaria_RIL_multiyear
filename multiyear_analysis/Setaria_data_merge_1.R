#Setaria_data_merge - step 1

#merge leaf-plant data  

library(tidyverse)

#bring in leaf data
load("../data_wrangle/leaf_level/leaf_phenotypes_clean.Rdata")

#looking at structure of dataset
str(leaf)
which(is.infinite(leaf$data)==T)
length(which(is.na(leaf$data)==T))

table(leaf$year)
table(leaf$experiment)
table(leaf$treatment)
head(leaf)
colnames(leaf)[6]='value'
#quick leaf level visualization 
ggplot(leaf, aes(exp, value, color=treatment))+geom_boxplot()+facet_wrap(~trait, scales = 'free_y')+theme_classic()
ggplot(leaf, aes(exp,value))+geom_boxplot()+facet_wrap(~trait, scales = 'free')+theme_classic()
ggplot(leaf, aes(value, group=exp))+geom_histogram()+facet_wrap(~trait, scales = 'free')+theme_classic()
ggplot(leaf, aes(y=value))+geom_boxplot()+facet_wrap(~trait, scales = 'free')+theme_classic()


test=leaf %>% group_by(exp, subplot_id, trait) %>% mutate(id = 1:n())
leaf.wide=test%>%spread(trait, value)
leaf.wide$length_width_ratio=leaf.wide$lfblade_length/leaf.wide$lfblade_width
pairs(leaf.wide[,9:18])
#removing points that are clearly outliers
leaf$value[which(leaf$trait=='gC.m2' & leaf$value<1)]=NA
leaf$value[which(leaf$trait=='gC.m2' & leaf$value>45)]=NA
leaf$value[which(leaf$trait=='gN.m2' & leaf$value>4)]=NA
leaf$value[which(leaf$trait=='CN_ratio' & leaf$value<1)]=NA
leaf$value[which(leaf$trait=='CN_ratio' & leaf$value>45)]=NA
leaf$value[which(leaf$trait=='lfblade_weight' & leaf$value>0.3)]=NA

ggplot(leaf, aes(exp, value, color=treatment))+geom_boxplot()+facet_wrap(~trait, scales = 'free_y')+theme_classic()
#plotting summary boxplots for leaf data
png('./results/Set_leaf_trait_summary_boxplots2.png', width = 1300, height = 875, bg='transparent', res=100)
ggplot(leaf, aes(x=as.factor(exp), y=value, group=interaction(exp,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c('red', 'goldenrod1', 'mediumorchid4', 'dodgerblue'))+
  facet_wrap(~trait, scale="free")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)
dev.off()
#at a glance, these traits look generally normal and without major outliers

#--------------------------------------------------------------------------------------------#
#plant
load("../data_wrangle/plant_level/data/harvest_phenotypes_clean_transformed.Rdata")
plant<-all_stack2
rm(all_stack2)
#looking at structure of plant level data
str(plant)
table(plant$treatment)
table(plant$trait)

ggplot(plant, aes(as.factor(year), value, color=treatment))+geom_boxplot()+facet_wrap(~trait, scales = 'free_y')+theme_classic()

ggplot(plant, aes(x=treatment, y=value, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free_y")+
  theme_classic()

png('./results/Set_plant_trait_summary_boxplots2.png', width = 1300, height = 875, bg='transparent', res=100)
ggplot(plant[!is.na(plant$trait),], aes(x=interaction(year, experiment), y=value, group=interaction(year,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c('red', 'goldenrod1', 'mediumorchid4', 'dodgerblue'))+
  facet_wrap(~trait, scale="free_y")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

#stack leaf and plant level data 
lp=bind_rows(leaf, plant)
head(lp)
lp$trait[which(lp$exp=='dn13.sim' & lp$trait=="gN.m2")]='sim_gN.m2'
lp$trait[which(lp$exp=='dn13.stag' & lp$trait=="gN.m2")]='stag_gN.m2'
lp$trait[which(lp$exp=='dn13.sim' & lp$trait=="gC.m2")]='sim_gC.m2'
lp$trait[which(lp$exp=='dn13.stag' & lp$trait=="gC.m2")]='stag_gC.m2'
lp$trait[which(lp$exp=='dn13.sim' & lp$trait=="CN_ratio")]='sim_CN_ratio'
lp$trait[which(lp$exp=='dn13.stag' & lp$trait=="CN_ratio")]='stag_CN_ratio'
lp$trait[which(lp$exp=='dn13.sim' & lp$trait=="d13C")]='sim_d13C'
lp$trait[which(lp$exp=='dn13.stag' & lp$trait=="d13C")]='stag_d13C'
lp$trait[which(lp$exp=='dn13.sim' & lp$trait=="SLA")]='sim_SLA'
lp$trait[which(lp$exp=='dn13.stag' & lp$trait=="SLA")]='stag_SLA'
lp$trait[which(lp$exp=='dn13.sim' & lp$trait=="lfblade_area")]='sim_lfblade_area'
lp$trait[which(lp$exp=='dn13.stag' & lp$trait=="lfblade_area")]='stag_lfblade_area'
lp$trait[which(lp$exp=='dn13.sim' & lp$trait=="lfblade_weight")]='sim_lfblade_weight'
lp$trait[which(lp$exp=='dn13.stag' & lp$trait=="lfblade_weight")]='stag_lfblade_weight'
lp$trait[which(lp$exp=='dn13.sim' & lp$trait=="lfblade_length")]='sim_lfblade_length'
lp$trait[which(lp$exp=='dn13.stag' & lp$trait=="lfblade_length")]='stag_lfblade_length'
lp$trait[which(lp$exp=='dn13.sim' & lp$trait=="lfblade_width")]='sim_lfblade_width'
lp$trait[which(lp$exp=='dn13.stag' & lp$trait=="lfblade_width")]='stag_lfblade_width'

#calculating percent of data for each year is NA value
x=table(is.na(lp$value), lp$year)
x[2,]/(x[1,]+x[2,])

#make environment column
lp$environment=paste(lp$experiment,lp$year, sep='_')

#assigning plot based on subplot
for (i in 1:nrow(lp)){
  if (lp$year[i] %in% c(2015)){
    lp$plot[i]=substr(lp$subplot_id[i],2,3)
  }
  else {
    lp$plot[i]=substr(lp$subplot_id[i],1,2)
  }
}
####PRODUCT####
save(lp, file="./merged_phenotypes.Rdata")
write.csv(lp, file = 'Set_RIL_field_merged_phenotypes_all_years_clean.csv', row.names = F)
write.csv(lp, file = 'Set_RIL_field_merged_phenotypes_all_years_clean_sas.csv', row.names = F, na =".")

##END##