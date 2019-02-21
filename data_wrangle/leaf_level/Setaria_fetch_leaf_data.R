#Setaria_fetch_leaf_data.R 
library(tidyverse)
#load "step_2" leaf data, do any necessary cleaning, formatting before data merge. 

#re-running with newest data version 
load("Set_RIL_field_leaf_level_subplot_data.Rdata")

table(leaf.level$genotype)
leaf.level%>% group_by(exp, genotype, subplot_id, trait)%>%summarize(leaf.level, n())
#still have some errant genotype names
leaf.level$genotype[leaf.level$genotype==" A10"]<-"A10"
leaf.level$genotype[leaf.level$genotype==" B100"]<-"B100"
leaf.level$genotype[leaf.level$genotype=="RIL_60"]<-"RIL_060"
leaf.level$genotype[leaf.level$genotype=="RIL_98"]<-"RIL_098"
leaf.level$genotype[leaf.level$genotype=="RIL_72"]<-"RIL_072"
leaf.level$genotype[leaf.level$genotype=="RIL_78"]<-"RIL_078"
leaf.level$genotype[leaf.level$genotype=="RIL_18"]<-"RIL_018"

#checking traits
table(leaf.level$trait)
trait.list=unique(leaf.level$trait)
for(i in 1:length(trait.list)){
  temp=leaf.level[which(leaf.level$trait == trait.list[i]),]
  hist(temp$data, main = paste(trait.list[i]))
  rm(temp)
}
#removing extreme value for CN_ratio
leaf.level$data[which(leaf.level$trait=='CN_ratio' & leaf.level$data >60)]=NA

for(i in 1:length(trait.list)){
  temp=leaf.level[which(leaf.level$trait == trait.list[i]),]
  hist(temp$data, main = paste(trait.list[i]))
  rm(temp)
}
rm(i,trait.list)

#adding treatment column
leaf.level$plot=as.numeric(substr(leaf.level$subplot_id,1,2))
table(leaf.level$plot)
leaf.level$plot[which(leaf.level$year==2015)]=as.numeric(substr(leaf.level$subplot_id[which(leaf.level$year==2015)],1,3))
for(i in 1:nrow(leaf.level)){
  if (leaf.level$experiment[i] == 'density'){
    leaf.level$treatment[i]=ifelse(leaf.level$plot[i] %in% c(31,33),'thick','sparse')
  }
  else if (leaf.level$year[i]==2013){
    leaf.level$treatment[i]=ifelse(leaf.level$plot[i] %in% c(15,16,21,22,23,24),'wet',
                                ifelse(leaf.level$plot[i] %in% c(17,18,19,20,25,26),'dry', NA))
  }
  else if (leaf.level$year[i]==2014){
    leaf.level$treatment[i]=ifelse(leaf.level$plot[i] %in% c(15,16,17,18,19,20),'wet',
                                ifelse(leaf.level$plot[i] %in% c(21,22,23,24,25,26),'dry', NA))
  }
  else if (leaf.level$year[i]==2015){
    leaf.level$treatment[i]=ifelse(leaf.level$plot[i] %in% c(501, 502, 507, 508, 509, 510),'wet',
                                ifelse(leaf.level$plot[i] %in% c(503, 504, 505, 506,511,512),'dry', NA))
  }
  else {
    leaf.level$treatment[i]=NA
  }
}
table(leaf.level$treatment)
leaf=leaf.level
#output and ready for merge 
save(leaf, file="leaf_phenotypes_clean.Rdata")

##END##