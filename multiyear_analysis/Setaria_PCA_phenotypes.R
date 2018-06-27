#Setaria_PCA_phenotypes.R 

#6/12/18

#migrate analysis from May data meeting 


#####PCA BLUPLESS######

#this is currently blupless data 
#working from output of data transformation script

#change 2013 dry to wet
pe_bmh_trans$treatment[pe_bmh_trans$year==2013&pe_bmh_trans$treatment=="dry"]<-"wet"

#just on thick planted 
pe_bmh_thick<-pe_bmh_trans[-which(pe_bmh_trans$treatment=="sparse"),]

#and on just common traits 
common<-c("panicle_emergence_DAS", 
          "leaf_mass", 
          "panicle_mass", 
          "stem_mass", 
          "vegetative_mass", 
          "total_mass", 
          "reproductive_vegetative_mass_ratio",
          "culm_height",
          "tiller_number_cbrt"
)

pe_bmh_common<-subset(pe_bmh_thick, trait %in% common)

#need genotype*treatment averages 
pe_bmh_g<-ddply(pe_bmh_common, c("year", "experiment", "treatment", "genotype", "trait"), summarise, data=mean(data, na.rm=TRUE))

pe_bmh_g$environment<-paste(pe_bmh_g$treatment, pe_bmh_g$year, sep="_")

#wide by trait and still long genotype and environment
pe_bmh_w<-dcast(pe_bmh_g, environment+genotype~trait, value.var="data")

#complete cases?
pe_bmh_w1<-pe_bmh_w[complete.cases(pe_bmh_w),]

#pull out environment column 
env<-pe_bmh_w1[,1]

#join genotype and environment
pe_bmh_w1$name<-paste(pe_bmh_w1$genotype, pe_bmh_w1$environment, sep="_")
rownames(pe_bmh_w1)<-pe_bmh_w1$name
pe_bmh_w1$name<-NULL

pe_bmh_w2<-pe_bmh_w1[,c(3:10)]


pca_try<-prcomp(pe_bmh_w2, center=TRUE, scale.=TRUE)


pca_try$sdev
pca_try$rotation
pca_try$x



plot(pca_try, type="l")



plot(pca_try$x[,1:2])
biplot(pca_try)


pca_try_table<-rbind(pca_try$rotation, pca_try$sdev)


ggbiplot(pca_try, choices=c(1,2),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)

ggbiplot(pca_try, choices=c(1,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)

ggbiplot(pca_try, choices=c(2,3),obs.scale=1, var.scale=1, groups=env, ellipse=TRUE)










#####PCA BLUP#####

#2013 not working? 













