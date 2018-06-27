#Setaria_derive_transform_ver2

# GIT first entry

#just transformations of plant level harvest data (mainly counts I imagine)
#in pre-merge dataset, keep both original and transformed data (keep dataset long for now)
#in a separate script, post-merge, derive traits (probably from original observations), and ?re-transform as necessary? 
#thinking further ahead, BLUPs will be calcualted on a trimmed dataset (keep transformed, ditch corresponding original observations)


#outliers 
#id obs already flagged from previous analysis 
outliers_ht<-subset(all_ht, flag>0)
#assign flag=0 to NA in flag column 
all_ht$flag[is.na(all_ht$flag)]<-0
#remove flag=2
all_ht1<-subset(all_ht, flag<2)



#sort traits into groups for closer look  
counts<-c("branch_number", "tiller_number", "leaf_number", "panicle_number")
mass<-c("leaf_mass","panicle_mass","stem_mass","vegetative_mass","total_mass","reproductive_vegetative_mass_ratio")
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

ggplot(subset(all_stack[-which(all_stack$treatment=='sparse'),], trait %in% devo), aes(x=data))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="raw")

ggplot(subset(all_stack[-which(all_stack$treatment=='sparse'),], trait %in% size), aes(x=data))+
  geom_density(aes(group=treatment, color=treatment))+
  scale_color_manual(values=c("red", "orange", "purple", "blue"))+
  facet_wrap(~trait+year, scales="free")+theme_classic()+labs(title="raw")

##transformations 
all_stack1<-all_stack

#square root, cube root, log transform 
all_stack1$sqrt<-sqrt(all_stack1$data)
all_stack1$cbrt<-(all_stack1$data)^(1/3)
all_stack1$log<-log(all_stack1$data)

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
all_counts_trans<-subset(all_stack2, trait %in% counts)
all_counts_trans1<-all_counts_trans[,c(1:6,9)]
colnames(all_counts_trans1)[7]<-"data"
all_counts_trans1$new_trait<-paste(all_counts_trans1$trait, "cbrt", sep="_")
all_counts_trans1<-all_counts_trans1[,c(1:5,7,8)]
colnames(all_counts_trans1)[7]<-"trait"

all_stack3<-all_stack1[-which(all_stack1$trait %in% counts),]


all_stack4<-rbind(all_stack3, all_counts_trans1)

pe_bmh_trans<-all_stack4





