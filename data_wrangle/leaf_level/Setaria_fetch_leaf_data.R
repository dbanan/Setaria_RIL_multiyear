#Setaria_fetch_leaf_data.R 

#load "step_2" leaf data, do any necessary cleaning, formatting before data merge. 


#leaf
#leaf<-read.csv("../data_wrangle/leaf_level/Set_RIL_leaf_data_subplot_level.csv", header=T, stringsAsFactors=FALSE)

#re-running with newest data version 
load("../data_wrangle/leaf_level/Set_RIL_field_leaf_level_subplot_data.Rdata")


#still have some errant genotype names
leaf.level$genotype[leaf.level$genotype==" A10"]<-"A10"
leaf.level$genotype[leaf.level$genotype==" B100"]<-"B100"
leaf.level$genotype[leaf.level$genotype=="RIL_60"]<-"RIL_060"
leaf.level$genotype[leaf.level$genotype=="RIL_98"]<-"RIL_098"
leaf.level$genotype[leaf.level$genotype=="RIL_72"]<-"RIL_072"
leaf.level$genotype[leaf.level$genotype=="RIL_78"]<-"RIL_078"
leaf.level$genotype[leaf.level$genotype=="RIL_18"]<-"RIL_018"

#scale 2015 drought length and width from cm to mm (x10)?


#output and ready for merge 
save(leaf.level, file="../data_wrangle/leaf_level/leaf_phenotypes_clean.Rdata")








#plant
load("../data_wrangle/plant_level/data/harvest_phenotypes_clean_transformed.Rdata")
plant<-all_stack2


#leaf data lacks treatment column? 
#so extract experimental design information from plant data? and merge with leaf data? 
design_only<-unique(plant[,c(1:5)])
leaf<-merge(leaf, design_only, by=c("year","experiment", "genotype", "subplot_id"))


#quick leaf level visualization 
ggplot(leaf, aes(x=treatment, y=data, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free")+
  theme_classic()


#at a glance, these traits look generally normal and without major outliers 
#but 2014 drought experiment SLA, leaf area&length&width, and leaf weight are missing? 


#stack leaf and plant level data 
lp<-rbind(leaf, plant)

#large stretches of NA (missing data, especially in 2015)?

####PRODUCT####
save(lp, file="./merged_phenotypes.Rdata")
write.csv(lp, file="./merged_phenotypes_clean.csv", row.names=FALSE)




