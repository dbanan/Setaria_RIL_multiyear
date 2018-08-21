#Setaria_data_merge

#merge leaf-plant data  

library(tidyverse)

#re-running with newest data version 
load("../data_wrangle/leaf_level/leaf_phenotypes_clean.Rdata")

#change "genotype" and "trait" from factor to character
leaf$genotype<-as.character(leaf$genotype)
leaf$trait<-as.character(leaf$trait)
table(leaf$year)
table(leaf$experiment)
table(leaf$treatment)
head(leaf)
colnames(leaf)[6]='value'
#quick leaf level visualization 
ggplot(leaf, aes(x=treatment, y=value, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free")+
  theme_classic()
png('./results/Set_leaf_trait_summary_boxplots.png', width = 1300, height = 875, bg='transparent', res=100)
ggplot(leaf, aes(x=as.factor(year), y=value, group=interaction(year,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
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
table(plant$treatment)
str(plant)
str(leaf)
ggplot(plant, aes(x=treatment, y=value, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free")+
  theme_classic()
png('./results/Set_plant_trait_summary_boxplots.png', width = 1300, height = 875, bg='transparent', res=100)
ggplot(plant, aes(x=as.factor(year), y=value, group=interaction(year,ordered(treatment, levels=c('wet','dry','thick','sparse'))), fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c('red', 'goldenrod1', 'mediumorchid4', 'dodgerblue'))+
  facet_wrap(~trait, scale="free")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)
dev.off()

#stack leaf and plant level data 
lp<-rbind(leaf, plant)

table(is.na(lp$value), lp$year)

####PRODUCT####
save(lp, file="./merged_phenotypes.Rdata")
##END##