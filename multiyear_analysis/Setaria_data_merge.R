#Setaria_data_merge

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

#calculating percent of data for each year is NA value
x=table(is.na(lp$value), lp$year)
x[2,]/(x[1,]+x[2,])

####PRODUCT####
save(lp, file="./merged_phenotypes.Rdata")
##END##