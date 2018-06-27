#Setaria_PCA_phenotypes.R 

#6/12/18
#migrate analysis from May data meeting 
#6/26/18
#now on git, this script (fingers crossed) should run smoothly from the BLUP calculation output 

library(plyr)
library(ggbiplot)
library(reshape2)




load("RIL_BLUP.Rdata")

#####PCA BLUP######

#just on thick planted 
just_thick<-rils.blups[-which(rils.blups$treatment=="sparse"),]

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

just_common<-subset(just_thick, trait %in% common)

#need genotype*treatment averages (on BLUP predicted values)
just_geno<-ddply(just_common, c("year", "experiment", "treatment", "genotype", "trait"), summarise, data=mean(predicted, na.rm=TRUE))

just_geno$environment<-paste(just_geno$treatment, just_geno$year, sep="_")

#wide by trait and still long genotype and environment
just_wide<-dcast(just_geno, environment+genotype~trait, value.var="data")

#complete cases?
just_wide1<-just_wide[complete.cases(just_wide),]

#pull out environment column 
env<-just_wide1[,1]

#join genotype and environment
just_wide1$name<-paste(just_wide1$genotype, just_wide1$environment, sep="_")
rownames(just_wide1)<-just_wide1$name
just_wide1$name<-NULL

just_wide2<-just_wide1[,c(3:10)]


pca_try<-prcomp(just_wide2, center=TRUE, scale.=TRUE)


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





