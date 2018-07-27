#Setaria_data_merge



#merge leaf-plant data  


library(ggplot2)
library(reshape2)

#leaf
#leaf<-read.csv("../data_wrangle/leaf_level/Set_RIL_leaf_data_subplot_level.csv", header=T, stringsAsFactors=FALSE)

#re-running with newest data version 
load("../data_wrangle/leaf_level/leaf_phenotypes_clean.Rdata")

leaf<-leaf.level
#change "genotype" and "trait" from factor to character
leaf$genotype<-as.character(as.factor(leaf$genotype))
leaf$trait<-as.character(as.factor(leaf$trait))



#plant
load("../data_wrangle/plant_level/data/harvest_phenotypes_clean_transformed.Rdata")
plant<-all_stack2


#leaf data lacks treatment column? 
#so extract experimental design information from plant data? and merge with leaf data? 
design_only<-unique(plant[,c(1:5)])
leaf<-merge(leaf, design_only, by=c("year","experiment", "genotype", "subplot_id"))

#check back on this: why do we lose leaf data with this merge? 
#Presumably there are observations taken from leaf samples that were not taken from plant samples? 


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
#NaN for some plant traits across multiple years? 



####PRODUCT####
save(lp, file="./merged_phenotypes.Rdata")






























