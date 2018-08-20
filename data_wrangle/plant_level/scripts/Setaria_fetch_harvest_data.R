#Setaria_fetch_harvest_data

#bring in plant-level harvest data from 2013-2015 RIL experiments 
#raw data is from:
##2013 and 2014 REP and DB SAS "step_2" efforts 
##2015 prelim "ready" efforts 

#this script infiles, formats, and addresses various legacy errors (ex tiller_number=0)
#product will be a cleaned plant-level dataset (see bottom of script for .csv and robject generation)

library(tidyverse)

######INFILE######
#infile the various phenotype files 
#note to future self...consider writing a function to infile data regardless of machine and Rproj (see Feldman & Ellsworth manuscript repo for function examples)
#for now, will use relative path starting at Rproj

#2013 Density 
ht_13DN<-read.csv("../data/raw_trait_data/DN13_bmh_traits_long_new.csv",header=T, stringsAsFactors=FALSE)
hw_13DN<-read.csv("../data/raw_trait_data/DN13_biomass_harvest_data_long_new.csv",header=T, stringsAsFactors=FALSE)
pe_13DN<-read.csv("../data/raw_trait_data/13DN_panicle_emergence_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=".")

#2013 Drought 
ht_13DR<-read.csv("../data/raw_trait_data/13DR_BMH_traits_step_2.csv",header=T, stringsAsFactors=FALSE, na.strings=c(".",""))
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
pe_15DR<-read.csv("../data/raw_trait_data/15DR panicle emergence ready.csv",header=T, stringsAsFactors=FALSE)
wp_15DR<-read.csv("../data/raw_trait_data/15DR water potential ready.csv", header=T, stringsAsFactors=FALSE)
ht_15DR<-read.csv("../data/raw_trait_data/15DR biomass traits ready.csv",header=T, stringsAsFactors=FALSE)
hw_15DR<-read.csv("../data/raw_trait_data/15DR biomass weights ready.csv",header=T, stringsAsFactors=FALSE)

#####PANICLE EMERGENCE#####
#panicle emergence format 
colnames(pe_13DN)[11]<-"subplot_id"
colnames(pe_13DN)[17]<-"data"

colnames(pe_13DR)[11]<-"subplot_id"
colnames(pe_13DR)[13]<-"data"

colnames(pe_14DR)[11]<-"subplot_id"
colnames(pe_14DR)[13]<-"data"

colnames(pe_14DN)[11]<-"subplot_id"
colnames(pe_14DN)[14]<-"data"

pe_15DR$year<-2015
pe_15DR$experiment<-"drought"

#stack
all_pe<-bind_rows(pe_13DN, pe_13DR, pe_14DR, pe_14DN, pe_15DR)
all_pe=all_pe[,c(1:14,17)]
#column format 
all_pe$trait='panicle_emergence_DAS'
all_pe$treatment[all_pe$treatment=="high_density"]<-"thick"
all_pe$treatment[all_pe$treatment=="low_density"]<-"sparse"
colnames(all_pe)[15]='value'
#basic visualization 
ggplot(all_pe, aes(x=treatment, y=value, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~year)+
  theme_classic()

#distributions looks pretty good, no major outliers to flag/remove 

#####HARVEST DAS######
#calculating harvest DAS
ht_13DN$harvest_DAS<-ht_13DN$DOY-ht_13DN$sowing_doy

ht_13DR$harvest_DAS<-ht_13DR$DOY-ht_13DR$sowing_doy

ht_14DR$sowing_doy<-159
ht_14DR$harvest_DAS<-ht_14DR$DOY-ht_14DR$sowing_doy

ht_14DN$julian<-as.Date(ht_14DN$date, origin="1899-12-30")
ht_14DN$DOY<-as.numeric(strftime(ht_14DN$julian, format="%j"))
ht_14DN$harvest_DAS<-ht_14DN$DOY-ht_14DN$sowing_doy

ht_15DR_hd<-ht_15DR%>%filter(trait=='harvest_das')%>%group_by(subplot_id)%>%summarise(harvest_DAS=mean(data))
ht_15DR.new<-full_join(ht_15DR, ht_15DR_hd, by=c("subplot_id"))

#later will use this harvest DAS data to show harvest DAS effect on harvest data (supplemental)

######TRAITS######
#formatting each experiment so there are consistent column names
str(ht_13DN)
colnames(ht_13DN)
colnames(ht_13DN)[c(11,13)]<-c("subplot_id",'rep')

str(ht_13DR)
colnames(ht_13DR)
colnames(ht_13DR)[c(12,15)]<-c("subplot_id",'rep')
table(ht_13DR$flag)
ht_13DR.no.outliers=ht_13DR[-which(ht_13DR$flag==2),]

str(ht_14DR)
colnames(ht_14DR)
colnames(ht_14DR)[9]<-"subplot_id"
table(ht_14DR$flag)
ht_14DR.no.outliers=ht_14DR[-which(ht_14DR$flag==2),]
ht_14DR.no.outliers=ht_14DR.no.outliers[,c(1:9,11:18)]

str(ht_14DN)
colnames(ht_14DN)
colnames(ht_14DN)[11]<-"subplot_id"
table(ht_14DN$flag)
ht_14DN.no.outliers=ht_14DN[-which(ht_14DN$flag==2),]

str(ht_15DR.new)
ht_15DR.new$year<-2015
ht_15DR.new$experiment<-"drought"
ht_15DR.new$plot=as.numeric(substr(ht_15DR.new$subplot_id,2,3))

#stack
all_ht<-bind_rows(ht_13DN, ht_13DR.no.outliers, ht_14DR.no.outliers, ht_14DN.no.outliers, ht_15DR.new)
table(all_ht$experiment)
table(all_ht$treatment)
all_ht$treatment[which(all_ht$treatment=="high_density")]<-"thick"
all_ht$treatment[which(all_ht$treatment=="low_density")]<-"sparse"
str(all_ht)

#looking at the variables in the combined dataset
var.num.check=summarise_all(all_ht, funs(sum(!is.na(.))))%>%gather()
common.var.check=all_ht%>%group_by(year, experiment)%>%summarise_all(funs(sum(!is.na(.))))
common.var.check2=summarise_all(common.var.check[3:22],funs(sum(.!=0)))%>%gather()
common.var.list=common.var.check2%>%filter(value==5)%>%pull(key)%>%c(.,'year','experiment')
all_ht.trimmed=all_ht[,colnames(all_ht) %in% common.var.list]
all_ht.trimmed%>%group_by(year,experiment,genotype,plot,subplot_id,rep,trait)%>%duplicated()%>%table()
#break out harvest_DAS 
all_hd=all_ht.trimmed %>%group_by(year, experiment, subplot_id) %>% summarise(harvest_DAS=mean(harvest_DAS))
sum(is.na(all_hd$harvest_DAS)) # what is this for??

#consistent trait names 
table(all_ht.trimmed$trait)
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="Panicle_number_per_plant")]<-"panicle_number"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="panicle_number_per_plant")]<-"panicle_number"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="leaf_number_per_plant")]<-"leaf_number"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="second_tiller_height")]<-"tiller_height"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="Panicle_Length")]<-"panicle_length"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="pan_number")]<-"panicle_number"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="culm_basal_c")]<-"basal_circumference"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="generic_1")]<-"leaf_number_green"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="dead")]<-"leaf_number_dead"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="branch_number_per_plant")]<-"branch_number"
all_ht.trimmed$trait[which(all_ht.trimmed$trait=="tiller_number_per_plant")]<-"tiller_number"
table(all_ht.trimmed$trait)
all_ht.trimmed%>%group_by(year,experiment,genotype,plot,subplot_id,rep,trait)%>%duplicated()%>%table()

#remove some traits (will transform consistently in a separate script)
all.trimmed.raw.traits=all_ht.trimmed %>% filter(!trait %in% c('','branch_number_per_plant_sqrt',
                                                               'harvest_das','tiller_diff','tiller_number_per_plant_sqrt'))
table(all.trimmed.raw.traits$trait)
all.trimmed.raw.traits%>%group_by(year,experiment,genotype,plot,subplot_id,rep,trait)%>%duplicated()%>%table()


#basic visualization 
ggplot(all.trimmed.raw.traits, aes(x=treatment, y=data, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free")+
  theme_classic()

#some things to fix 
#errant NA
#aggregate(data~year+experiment+trait, data=all_ht, function(x) {sum(is.na(x))}, na.action=NULL)
missing.value.check=all.trimmed.raw.traits%>%group_by(year, experiment, trait)%>%summarise(na.count=sum(is.na(data)))
missing.value.check%>%group_by(trait,year)%>%summarize(sum(na.count))
#errant data=0
#aggregate(data~year+experiment+trait, data=all_ht, function(x) {sum(is.na(x))}, na.action=NULL)
zero.value.check=all.trimmed.raw.traits%>%filter(data==0)%>%group_by(year, experiment, trait)%>%summarise(n=n())
#63 obs for tiller number at zero. tiller number = 0 should be changed to = 1 
all.trimmed.raw.traits$data[which(all.trimmed.raw.traits$trait=='tiller_number' & all.trimmed.raw.traits$data==0)]=1
all.trimmed.raw.traits%>%group_by(year,experiment,genotype,plot,subplot_id,rep,trait)%>%duplicated()%>%table()
#branch number = NA should be = 0
#why so many NA for leaf_number and panicle number 2013 drought? is it because we only measured in density that year?
#tiller_height NA in 2015 drought is because we did not measure it that year? 

#branch number fix 
#swing branch number wide with culm height and if culm height is present and branch number is NA, change to 0
#trim down to just relevant traits and experiment 
branch_fix<-all.trimmed.raw.traits[which(all.trimmed.raw.traits$year==2013 & all.trimmed.raw.traits$trait %in% c("culm_height", "branch_number")),]
branch_fix%>%group_by(year,experiment,genotype,plot,subplot_id,rep,trait)%>%duplicated()%>%table()

#swing
branch_wide<-dcast(branch_fix, year+experiment+genotype+treatment+subplot_id+rep~trait, value.var="data")
branch_wide<-branch_fix%>%group_by(subplot_id,rep)%>%spread(key=trait, value=data)

#switch na to 0 where appropriate 
branch_wide$branch_number[which(is.na(branch_wide$branch_number) & branch_wide$culm_height>1)]<-0

#reformat new branch data 
branch.merge=gather(branch_wide, key=trait, value=data, c(9:10))

#join branch data to rest of harvest traits 
all_ht1<-all.trimmed.raw.traits[-which(all.trimmed.raw.traits$year==2013 & all.trimmed.raw.traits$trait %in% c("culm_height", "branch_number")),]
nrow(all.trimmed.raw.traits)-nrow(all_ht1)==nrow(branch_fix)
all_ht2<-bind_rows(all_ht1, branch.merge)

#visualize
ggplot(all_ht2, aes(x=treatment, y=data, fill=treatment))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free")+
  theme_classic()


#average to subplot_id
all.ht.means=all_ht2%>%group_by(year, experiment, genotype, treatment, subplot_id, trait) %>%summarise(value=mean(data, na.rm=T))
#NaN's result from division of 0? why would this still be here? 
#* The NaN means new NAs were created. probably because a mean couldn't be taken for NA values. Inf is the result of dividing by 0

######WEIGHTS#######
colnames(hw_13DN)
colnames(hw_13DN)[11]<-"subplot_id"
colnames(hw_13DN)[13]="number_plants_harvested"

colnames(hw_13DR)
colnames(hw_13DR)[12]<-"subplot_id"

colnames(hw_14DR)
colnames(hw_14DR)[11]<-"subplot_id"

colnames(hw_14DN)
colnames(hw_14DN)[11]<-"subplot_id"

colnames(hw_15DR)
hw_15DR$year<-2015
hw_15DR$experiment<-"drought"

#stack weights together 
all_hw<-bind_rows(hw_13DN, hw_13DR, hw_14DR, hw_14DN, hw_15DR)
str(all_hw)
table(all_hw$treatment)
all_hw$treatment[all_hw$treatment=="high_density"]<-"thick"
all_hw$treatment[all_hw$treatment=="low_density"]<-"sparse"

#looking at the variables in the combined dataset
var.num.check=summarise_all(all_hw, funs(sum(!is.na(.))))%>%gather()
common.var.check=all_hw%>%group_by(year, experiment)%>%summarise_all(funs(sum(!is.na(.))))
common.var.check2=summarise_all(common.var.check[3:19],funs(sum(.!=0)))%>%gather()
common.var.list=common.var.check2%>%filter(value==5)%>%pull(key)%>%c(.,'year','experiment')
all_hw.trimmed=all_hw[,colnames(all_hw) %in% common.var.list]
all_hw.trimmed%>%group_by(year,experiment,genotype,subplot_id,trait)%>%duplicated()%>%table()

#consistent trait names 
table(all_hw.trimmed$trait)
all_hw.trimmed$trait[all_hw.trimmed$trait=="leaf_mass_per_plant"]<-"leaf_mass_at_harvest"
all_hw.trimmed$trait[all_hw.trimmed$trait=="panicle_mass_per_plant"]<-"panicle_mass_at_harvest"
all_hw.trimmed$trait[all_hw.trimmed$trait=="stem_mass_per_plant"]<-"stem_mass_at_harvest"
all_hw.trimmed$trait[all_hw.trimmed$trait=="total_mass_per_plan"]<-"total_mass_per_plant"

#remove some traits 
#remove and re-do per area consistently 
all_hw.trimmed<-all_hw.trimmed[-which(all_hw.trimmed$trait=="leaf_mass_per_m2"),]
all_hw.trimmed<-all_hw.trimmed[-which(all_hw.trimmed$trait=="panicle_mass_per_m2"),]
all_hw.trimmed<-all_hw.trimmed[-which(all_hw.trimmed$trait=="stem_mass_per_m2"),]
all_hw.trimmed<-all_hw.trimmed[-which(all_hw.trimmed$trait=="total_mass_per_m2"),]
#remove and re-do totals and ratios 
all_hw.trimmed<-all_hw.trimmed[-which(all_hw.trimmed$trait=="total_mass_per_plant"),]
all_hw.trimmed<-all_hw.trimmed[-which(all_hw.trimmed$trait=="vegetative_mass_per_plant"),]
all_hw.trimmed<-all_hw.trimmed[-which(all_hw.trimmed$trait=="reproductive_vegetative_mass_ratio"),]


#Swing wide and derive some traits 
all_hww=all_hw.trimmed%>%group_by(year, experiment, genotype, treatment, subplot_id)%>%spread(trait, data)
#combine with harvest DAS
all_hww2=merge(all_hww, all_hd, by=c("year","experiment","subplot_id"),all.x=T)
#calculate some traits 
all_hww2$vegetative_mass_at_harvest<-all_hww2$leaf_mass_at_harvest+all_hww2$stem_mass_at_harvest
all_hww2$total_mass_at_harvest<-all_hww2$leaf_mass_at_harvest+all_hww2$stem_mass_at_harvest+all_hww2$panicle_mass_at_harvest
all_hww2$reproductive_vegetative_mass_ratio<-all_hww2$panicle_mass_at_harvest/all_hww2$vegetative_mass
all_hww2$leaf_mass_ratio<-all_hww2$leaf_mass_at_harvest/all_hww2$total_mass_at_harvest
all_hww2$leaf_mass_per_DAS=all_hww2$leaf_mass_at_harvest/all_hww2$harvest_DAS
all_hww2$stem_mass_per_DAS=all_hww2$stem_mass_at_harvest/all_hww2$harvest_DAS
all_hww2$panicle_mass_per_DAS=all_hww2$panicle_mass_at_harvest/all_hww2$harvest_DAS
all_hww2$vegetative_mass_per_DAS=all_hww2$vegetative_mass_at_harvest/all_hww2$harvest_DAS
all_hww2$total_mass_per_DAS=all_hww2$total_mass_at_harvest/all_hww2$harvest_DAS
#back long 
all_hwl=gather(all_hww2, key=trait, value = value, c(6:8,10:18))

#quick visualization
ggplot()+geom_boxplot(data=all_hwl, aes(factor(treatment), data, fill=factor(treatment)))+
  facet_wrap(~trait+year, scales="free")


#########STACK##########
all_stack<-bind_rows(all_pe, all_hwl, all.ht.means)
geno.check=as.data.frame(table(all_stack$genotype))
#fix these errant genotypes (either no data or data cannot be attributed to actual observations)
#BLANK
#missing
#NO TAG
#RIL-093

all_stack<-all_stack[-which(all_stack$genotype %in% c("BLANK", "missing", "NO TAG", "RIL-093")),]
all_stack%>%group_by(year,experiment,genotype,plot,subplot_id,trait)%>%duplicated()%>%table()

#trim columns
var.num.check=summarise_all(all_stack, funs(sum(!is.na(.))))%>%gather(value=count)
common.var.list=var.num.check%>%filter(count>70000)%>%pull(key)
all_stack.trimmed=all_stack[,colnames(all_stack) %in% common.var.list]
all_stack.trimmed%>%group_by(year,experiment,genotype,subplot_id,trait)%>%duplicated()%>%table()

#visualize 
ggplot(all_stack, aes(x=as.factor(treatment), y=data, fill=as.factor(treatment)))+
  geom_boxplot()+
  scale_x_discrete(limits=c("wet", "dry", "thick", "sparse"))+
  scale_fill_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scale="free")+
  theme_classic()


######PRODUCT######

save(all_stack.trimmed, file="../data/harvest_phenotypes_clean.Rdata")

