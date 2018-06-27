#Setaria_fetch_harvest_data

#bring in plant-level harvest data from 2013-2015 RIL experiments 
#raw data is from:
##2013 and 2014 REP and DB SAS "step_2" efforts 
##2015 prelim "ready" efforts 

#this script infiles, formats, and addresses various legacy errors (ex tiller_number=0)
#product will be a cleaned plant-level dataset (see bottom of script for .csv and robject generation)


library(reshape2)
library(ggplot2)
library(plyr)






######INFILE######
#infile the various phenotype files 
#note to future self...consider writing a function to infile data regardless of machine and Rproj (see Feldman & Ellsworth manuscript repo for function examples)
#for now, will use relative path starting at Rproj

#2013 Density 
ht_13DN<-read.csv("../data/raw_trait_data/13DN_BMH_traits_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
hw_13DN<-read.csv("../data/raw_trait_data/13DN_BMH_weights_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
pe_13DN<-read.csv("../data/raw_trait_data/13DN_panicle_emergence_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")

#2013 Drought 
ht_13DR<-read.csv("../data/raw_trait_data/13DR_BMH_traits_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
hw_13DR<-read.csv("../data/raw_trait_data/13DR_BMH_weights_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
pe_13DR<-read.csv("../data/raw_trait_data/13DR_panicle_emergence_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")

#2014 Drought 
ht_14DR<-read.csv("../data/raw_trait_data/14DR_BMH_traits_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
hw_14DR<-read.csv("../data/raw_trait_data/14DR_BMH_weights_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
pe_14DR<-read.csv("../data/raw_trait_data/14DR_panicle_emergence_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")

#2014 Density 
ht_14DN<-read.csv("../data/raw_trait_data/14DN_BMH_traits_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
hw_14DN<-read.csv("../data/raw_trait_data/14DN_BMH_weights_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
pe_14DN<-read.csv("../data/raw_trait_data/14DN_panicle_emergence_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")

#2015 Drought 
pe_15DR<-read.csv("../data/raw_trait_data/15DR panicle emergence ready.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
wp_15DR<-read.csv("../data/raw_trait_data/15DR water potential ready.csv", header=T, stringsAsFactors=FALSE, na.strings=".")
ht_15DR<-read.csv("../data/raw_trait_data/15DR biomass traits ready.csv",header=T, stringsAsFactors=FALSE, na.strings=".")
hw_15DR<-read.csv("../data/raw_trait_data/15DR biomass weights ready.csv",header=T, stringsAsFactors=FALSE, na.strings=".")



#####HARVEST DAS######
#pull out harvest DAS
ht_13DN$harvest_DAS<-ht_13DN$DOY-ht_13DN$sowing_doy

ht_13DR$harvest_DAS<-ht_13DR$DOY-ht_13DR$sowing_doy

ht_14DR$sowing_doy<-159
ht_14DR$harvest_DAS<-ht_14DR$DOY-ht_14DR$sowing_doy

ht_14DN$julian<-as.Date(ht_14DN$date, origin="1899-12-30")
ht_14DN$DOY<-as.numeric(strftime(ht_14DN$julian, format="%j"))
ht_14DN$harvest_DAS<-ht_14DN$DOY-ht_14DN$sowing_doy

ht_15DR_hd<-subset(ht_15DR, trait=="harvest_das")
ht_15DR_hd<-ht_15DR_hd[,c(4,7)]
colnames(ht_15DR_hd)[2]<-"harvest_DAS"
ht_15DR<-merge(ht_15DR, ht_15DR_hd, by=c("subplot_id"))

#later will use this harvest DAS data to show harvest DAS effect on harvest data (supplemental)



#####PANICLE EMERGENCE#####
#panicle emergence format 
colnames(pe_13DN)[11]<-"subplot_id"
colnames(pe_13DN)[17]<-"data"
pe_13DN$trait<-"panicle_emergence_DAS"
pe_13DN<-pe_13DN[,c(1,2,8,9,11,14,17)]

colnames(pe_13DR)[11]<-"subplot_id"
colnames(pe_13DR)[13]<-"data"
pe_13DR$trait<-"panicle_emergence_DAS"
pe_13DR<-pe_13DR[,c(1,2,8,9,11,12,13)]

colnames(pe_14DR)[11]<-"subplot_id"
colnames(pe_14DR)[13]<-"data"
pe_14DR$trait<-"panicle_emergence_DAS"
pe_14DR<-pe_14DR[,c(1,2,8,9,11,12,13)]

colnames(pe_14DN)[11]<-"subplot_id"
colnames(pe_14DN)[14]<-"data"
pe_14DN$trait<-"panicle_emergence_DAS"
pe_14DN<-pe_14DN[,c(1,2,8,9,11,12,14)]

pe_15DR<-pe_15DR[,c(2:6)]
pe_15DR$trait<-"panicle_emergence_DAS"
pe_15DR$year<-2015
pe_15DR$experiment<-"drought"

#stack
all_pe<-rbind(pe_13DN, pe_13DR, pe_14DR, pe_14DN, pe_15DR)

#column format 
all_pe$data<-as.numeric(all_pe$data)

all_pe$treatment[all_pe$treatment=="high_density"]<-"thick"
all_pe$treatment[all_pe$treatment=="low_density"]<-"sparse"

#basic visualization 
ggplot(all_pe, aes(x=treatment, y=data, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~year)+
  theme_classic()

#distributions looks pretty good, no major outliers to flag/remove 


######TRAITS######
#format
colnames(ht_13DN)[11]<-"subplot_id"
colnames(ht_13DN)[15]<-"rep"
ht_13DN<-ht_13DN[,c(1,2,8,9,11,15,16,18,17,19)]

colnames(ht_13DR)[12]<-"subplot_id"
colnames(ht_13DR)[15]<-"rep"
ht_13DR<-ht_13DR[,c(1,2,9,10,12,14,15,17,16,18)]

colnames(ht_14DR)[9]<-"subplot_id"
ht_14DR<-ht_14DR[,c(1,2,6,7,9,13,14,16,15,18)]

colnames(ht_14DN)[11]<-"subplot_id"
ht_14DN<-ht_14DN[,c(1,2,8,9,11,14,15,17,16,20)]

ht_15DR<-ht_15DR[,c(1,3:8)]
ht_15DR$year<-2015
ht_15DR$experiment<-"drought"
ht_15DR$flag<-NA

#stack
all_ht<-rbind(ht_13DN, ht_13DR, ht_14DR, ht_14DN, ht_15DR)

#format
all_ht$data<-as.numeric(all_ht$data)
all_ht$harvest_DAS<-as.numeric(all_ht$harvest_DAS)

all_ht$treatment[all_ht$treatment=="high_density"]<-"thick"
all_ht$treatment[all_ht$treatment=="low_density"]<-"sparse"

#break out harvest_DAS 
all_hd<-ddply(all_ht, c("year", "experiment", "subplot_id"), summarise, harvest_DAS=mean(harvest_DAS))



#consistent trait names 
all_ht$trait[all_ht$trait=="Panicle_number_per_plant"]<-"panicle_number"
all_ht$trait[all_ht$trait=="panicle_number_per_plant"]<-"panicle_number"
all_ht$trait[all_ht$trait=="leaf_number_per_plant"]<-"leaf_number"
all_ht$trait[all_ht$trait=="second_tiller_height"]<-"tiller_height"
all_ht$trait[all_ht$trait=="Panicle_Length"]<-"panicle_length"
all_ht$trait[all_ht$trait=="pan_number"]<-"panicle_number"
all_ht$trait[all_ht$trait=="culm_basal_c"]<-"basal_circumference"
all_ht$trait[all_ht$trait=="generic_1"]<-"leaf_number_green"
all_ht$trait[all_ht$trait=="dead"]<-"leaf_number_dead"
all_ht$trait[all_ht$trait=="branch_number_per_plant"]<-"branch_number"
all_ht$trait[all_ht$trait=="tiller_number_per_plant"]<-"tiller_number"

#remove some traits (will transform consistently in a separate script)
all_ht<-all_ht[!(all_ht$trait==""),]
all_ht<-all_ht[!(all_ht$trait=="harvest_DAS"),]
all_ht<-all_ht[!(all_ht$trait=="harvest_das"),]
all_ht<-all_ht[!(all_ht$trait=="tiller_diff"),]
all_ht<-all_ht[!(all_ht$trait=="branch_number_per_plant_sqrt"),]
all_ht<-all_ht[!(all_ht$trait=="tiller_number_per_plant_sqrt"),]
all_ht<-all_ht[!(all_ht$trait=="leaf_number_per_plant_sqrt"),]
all_ht<-all_ht[!(all_ht$trait=="panicle_number_per_plant_sqrt"),]

#basic visualization 
ggplot(all_ht, aes(x=treatment, y=data, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free")+
  theme_classic()





#some things to fix 
#errant NA
aggregate(data~year+experiment+trait, data=all_ht, function(x) {sum(is.na(x))}, na.action=NULL)
#errant data=0
aggregate(data~year+experiment+trait, data=all_ht, function(x) {sum(is.na(x))}, na.action=NULL)

#tiller number = 0 should be changed to = 1 
#branch number = NA should be = 0

#tiller number fix 
all_ht$data[all_ht$trait=="tiller_number"&all_ht$data<1]<-1

#branch number fix 
#swing branch number wide with culm height and if culm height is present and branch number is NA, change to 0
#trim down to just relevant traits and experiment 
branch_fix<-all_ht[which(all_ht$year==2013 & all_ht$trait %in% c("culm_height", "branch_number")),]
#swing
branch_wide<-dcast(branch_fix, year+experiment+genotype+treatment+subplot_id+rep~trait, value.var="data")
#switch na to 0 where appropriate 
branch_wide$branch_number[is.na(branch_wide$branch_number) & branch_wide$culm_height>1]<-0

#reformat new branch data 
colnames(branch_wide)[7]<-"data"
branch_wide$flag<-NA
branch_wide$harvest_DAS<-NA
branch_wide$trait<-"branch_number"
branch_wide$culm_height<-NULL

#join branch data to rest of harvest traits 
all_ht1<-all_ht[-which(all_ht$year==2013 & all_ht$trait=="branch_number"),]
all_ht1<-rbind(all_ht1, branch_wide)

#outlier flags 
#id obs already flagged from previous analysis 
outliers_ht<-subset(all_ht1, flag>0)
#assign flag=0 to NA in flag column 
all_ht1$flag[is.na(all_ht1$flag)]<-0
#remove flag=2
all_ht2<-subset(all_ht1, flag<2)




#visualize post flag removal 
ggplot(all_ht2, aes(x=treatment, y=data, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free")+
  theme_classic()


#average to subplot_id
all_hts<-ddply(all_ht2, c("year", "experiment", "genotype", "treatment", "subplot_id", "trait"), summarise,
               data=mean(data, na.rm=TRUE))

#NaN's result from division of 0? why would this still be here? 








######WEIGHTS#######

colnames(hw_13DN)[11]<-"subplot_id"
hw_13DN<-hw_13DN[,c(1,2,8,9,11,13,15)]

colnames(hw_13DR)[12]<-"subplot_id"
hw_13DR<-hw_13DR[,c(1,2,9,10,12,15,17)]

colnames(hw_14DR)[11]<-"subplot_id"
hw_14DR<-hw_14DR[,c(1,2,8,9,11,13,15)]

colnames(hw_14DN)[11]<-"subplot_id"
hw_14DN<-hw_14DN[,c(1,2,8,9,11,13,14)]

hw_15DR<-hw_15DR[,c(2:6)]
hw_15DR$year<-2015
hw_15DR$experiment<-"drought"

#stack weights together 
all_hw<-rbind(hw_13DN, hw_13DR, hw_14DR, hw_14DN, hw_15DR)

#format
all_hw$data<-as.numeric(as.character(all_hw$data))

all_hw$treatment[all_hw$treatment=="high_density"]<-"thick"
all_hw$treatment[all_hw$treatment=="low_density"]<-"sparse"

#consistent trait names 
all_hw$trait[all_hw$trait=="leaf_mass_per_plant"]<-"leaf_mass"
all_hw$trait[all_hw$trait=="panicle_mass_per_plant"]<-"panicle_mass"
all_hw$trait[all_hw$trait=="stem_mass_per_plant"]<-"stem_mass"
all_hw$trait[all_hw$trait=="total_mass_per_plan"]<-"total_mass_per_plant"

#remove some traits 
#remove and re-do per area consistently 
all_hw<-all_hw[!(all_hw$trait=="leaf_mass_per_m2"),]
all_hw<-all_hw[!(all_hw$trait=="panicle_mass_per_m2"),]
all_hw<-all_hw[!(all_hw$trait=="stem_mass_per_m2"),]
all_hw<-all_hw[!(all_hw$trait=="total_mass_per_m2"),]
#remove and re-do totals and ratios 
all_hw<-all_hw[!(all_hw$trait=="total_mass_per_plant"),]
all_hw<-all_hw[!(all_hw$trait=="vegetative_mass_per_plant"),]
all_hw<-all_hw[!(all_hw$trait=="reproductive_vegetative_mass_ratio"),]




#Swing wide and derive some traits 
all_hww<-dcast(all_hw, year+experiment+genotype+treatment+subplot_id~trait, value.var="data")
#calculate some traits 
all_hww$vegetative_mass<-all_hww$leaf_mass+all_hww$stem_mass
all_hww$total_mass<-all_hww$leaf_mass+all_hww$stem_mass+all_hww$panicle_mass
all_hww$reproductive_vegetative_mass_ratio<-all_hww$panicle_mass/all_hww$vegetative_mass

#back long 
all_hwl<-melt(all_hww, id.vars=c("year","experiment","genotype","treatment","subplot_id"), measure.vars=c("leaf_mass", "panicle_mass", "stem_mass", "vegetative_mass", "total_mass", "reproductive_vegetative_mass_ratio"), variable.name="trait",value.name="data")

#########STACK##########
all_stack<-rbind(all_pe, all_hwl, all_hts)

#fix these errant genotypes (either no data or data cannot be attributed to actual observations)
#BLANK
#missing
#NO TAG
#RIL-093

all_stack<-all_stack[!(all_stack$genotype %in% c("BLANK", "missing", "NO TAG", "RIL-093")),]

#visualize 
ggplot(all_stack, aes(x=treatment, y=data, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free")+
  theme_classic()








######PRODUCT######

save(all_stack, file="../data/harvest_phenotypes_clean.Rdata")


write.csv(all_stack, file="../data/harvest_phenotypes_clean.csv", row.names=FALSE)




