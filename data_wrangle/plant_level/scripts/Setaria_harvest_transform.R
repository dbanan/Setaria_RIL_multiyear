#Setaria_derive_transform

# GIT first entry

#just transformations of plant level harvest data (mainly counts I imagine)
#in pre-merge dataset, keep both original and transformed data (keep dataset long for now)
#in a separate script, post-merge, derive traits (probably from original observations), and ?re-transform as necessary? 
#thinking further ahead, BLUPs will be calcualted on a trimmed dataset (keep transformed, ditch corresponding original observations)


#load object with cleaned plant_level data 
load("../data/harvest_phenotypes_clean.Rdata")

#sort traits into groups for closer look  
counts<-c("branch_number", "tiller_number", "leaf_number", "panicle_number")
mass<-c("leaf_mass_at_harvest","panicle_mass_at_harvest","stem_mass_at_harvest","vegetative_mass_at_harvest","total_mass_at_harvest",
        "leaf_mass_per_DAS","panicle_mass_per_DAS","stem_mass_per_DAS","vegetative_mass_per_DAS","total_mass_per_DAS")
ratio<-c("reproductive_vegetative_mass_ratio","leaf_mass_ratio")
size<-c("culm_height", "tiller_height", "basal_circumference")
devo<-c("panicle_emergence_DAS", "dead_percent", "green_percent", "leaf_number_dead", "leaf_number_green")

ggplot(subset(all_stack[-which(all_stack$treatment=='sparse'),], trait %in% counts), aes(x=data))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="raw")

ggplot(subset(all_stack[-which(all_stack$treatment=='sparse'),], trait %in% mass), aes(x=data))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="raw")

ggplot(subset(all_stack[-which(all_stack$treatment=='sparse'),], trait %in% ratio), aes(x=data))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="raw")

ggplot(subset(all_stack[-which(all_stack$treatment=='sparse'),], trait %in% devo), aes(x=data))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="raw")

ggplot(subset(all_stack[-which(all_stack$treatment=='sparse'),], trait %in% size), aes(x=data))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="raw")

##transformations 
all_stack1<-all_stack.trimmed

#square root, cube root, log transform 
all_stack1$sqrt<-sqrt(all_stack1$value)
all_stack1$cbrt<-(all_stack1$value)^(1/3)
all_stack1$log<-log(all_stack1$value)

#compare distributions 
ggplot(subset(all_stack1[-which(all_stack1$treatment=='sparse'),], trait %in% counts), aes(x=sqrt))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="sqrt")

ggplot(subset(all_stack1[-which(all_stack1$treatment=='sparse'),], trait %in% counts), aes(x=cbrt))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="cbrt")

ggplot(subset(all_stack1[-which(all_stack1$treatment=='sparse'),], trait %in% counts), aes(x=log))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="log")

#looks like cube root is the way to go for counts, replace raw count data with cube root transformed values 
all_counts_trans<-subset(all_stack1, trait %in% counts)
all_counts_trans1<-all_counts_trans[,c(1:6,9)]
colnames(all_counts_trans1)[7]<-"value"
all_counts_trans1$new_trait<-paste(all_counts_trans1$trait, "cbrt", sep="_")
all_counts_trans1<-all_counts_trans1[,c(1:5,7,8)]
colnames(all_counts_trans1)[7]<-"trait"

#combining transformed and original dataset 
all_stack2<-rbind(all_counts_trans1, all_stack.trimmed)

#transformed r object for merge with megadataset and further dataset 
save(all_stack2, file="../data/harvest_phenotypes_clean_transformed.Rdata")