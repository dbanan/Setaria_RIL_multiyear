#Setaria_data_merge



#merge leaf-plant data  


#leaf
leaf<-read.csv("../data_wrangle/leaf_level/Set_RIL_leaf_data_subplot_level.csv", header=T, stringsAsFactors=FALSE)

#still have some errant genotype names
leaf$genotype[leaf$genotype==" A10"]<-"A10"
leaf$genotype[leaf$genotype==" B100"]<-"B100"
leaf$genotype[leaf$genotype=="RIL_60"]<-"RIL_060"
leaf$genotype[leaf$genotype=="RIL_98"]<-"RIL_098"
leaf$genotype[leaf$genotype=="RIL_72"]<-"RIL_072"
leaf$genotype[leaf$genotype=="RIL_78"]<-"RIL_078"
leaf$genotype[leaf$genotype=="RIL_18"]<-"RIL_018"




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






#stack leaf and plant level data 
lp<-rbind(leaf, plant)

#why are there still large stretches of NA (missing data, especially in 2015)?


























