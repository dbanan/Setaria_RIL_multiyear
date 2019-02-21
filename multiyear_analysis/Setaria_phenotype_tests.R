#Setaria_statistics 

install.packages("tidyverse")
library(tidyverse)




install.packages("ggsignif")
library(ggsignif)


install.packages("rstatix")
library(rstatix)

install.packages("ggrepel")
library(ggrepel)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)


devtools::install_github("kassambara/rstatix")
library(rstatix)

library(dplyr)
library(ggplot2)
library(plyr)

#statistics, run mixed model on 7 different environments for each trait

#70 genotype core 
#genotype as fixed effect 
#environment as random or fixed? 

#bring in BLUPs, trim to 70 genotype core 

####Data prep####
#load BLUPs
load("RIL_BLUP.Rdata")

#change year to numeric 
rils.blups$year<-as.numeric(as.character(rils.blups$year))
rils.blups$treatment<-as.character(rils.blups$treatment)
rils.blups$environment<-paste(rils.blups$treatment, rils.blups$year, sep="_")

#just on removing low density plants from the dataset 
just_thick<-rils.blups[-which(rils.blups$year==2013 | rils.blups$treatment=="sparse"),]
table(just_thick$year)
table(just_thick$treatment)



#lists for just common traits (version for many environments but few traits; few environments but many traits)
trait.matrix=as.data.frame.array(table(just_thick$trait,just_thick$environment))
trait.check=apply(trait.matrix,1,function(i) any(i == 0))
common=names(which(trait.check ==F))
just_common<-subset(just_thick, trait %in% common)

less.common=trait.matrix[-which(rownames(trait.matrix) %in% common),]
geno.table=as.data.frame.array(table(just_thick$genotype,just_thick$environment))
geno.check=apply(geno.table,1,function(i) any(i == 0))
common.geno=names(which(geno.check ==F))
#need genotype*treatment averages (on BLUP predicted values)
#common_geno<-ddply(just_common, c("year", "experiment",'environment', "treatment", "genotype", "trait"), summarise, data=mean(predicted, na.rm=TRUE))
geno_means=just_common%>%group_by(year, experiment, treatment, genotype, trait)%>%summarise(value=mean(value, na.rm=T))
#subsetting genotype means to just the lines that were planted in all environments
common_geno=subset(geno_means, genotype %in% common.geno)

#environment column
common_geno$environment<-paste(common_geno$treatment, common_geno$year, sep="_")

#get rid of harvest_per_DAS 
common_geno<-common_geno[-which(common_geno$trait %in% c("leaf_mass_per_DAS", "panicle_mass_per_DAS","stem_mass_per_DAS","total_mass_per_DAS","vegetative_mass_per_DAS", "tiller_number")),]








#outputting boxplots of common traits for common genotypes across environments

png("./results/boxplot_core_pheno.png", width=900, height=700)
ggplot(common_geno, aes(x=environment, y=value))+
  geom_boxplot(aes(fill=environment))+
  facet_wrap(~trait, scale="free")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+
  theme(rect = element_rect(fill = "transparent"))
dev.off()








#calculate quartiles and generate quartile table (trait*environment)                           
quant<-(aggregate(value~trait+environment, data=common_geno, FUN=fivenum))
values<-as.data.frame(quant$value)
colnames(values)<-c("min","Q1","median","Q3","max")
#calculate IQR and outliers 
values$IQR<-values$Q3-values$Q1
values$out_upper<-values$Q3+values$IQR*1.5
values$out_lower<-values$Q1-values$IQR*1.5

headers<-quant[,c(1,2)]
quantiles<-cbind(headers, values)
quantiles$trait<-as.character(quantiles$trait)


#merge with data 
common1<-merge(common_geno, quantiles, by=c("trait","environment"))

#identify outlier observations 
common1$outlier[common1$value>common1$out_upper | common1$value<common1$out_lower]<-common1$genotype

#boxplots with outliers labeled 

ggplot(common1, aes(x=environment, y=value))+
  geom_boxplot(aes(fill=environment))+
  geom_text_repel(aes(label=outlier), na.rm=TRUE)+
  facet_wrap(~trait, scale="free")+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+
  theme(rect = element_rect(fill = "transparent"))



ggplot(common1, aes(x=environment, y=value))+
  geom_boxplot(aes(color=environment))+
  geom_line(data=subset(common1, genotype %in% interest), aes(group=genotype, linetype=genotype))+
  stat_summary(fun.y=mean, color="red", geom="point")+
  #geom_text_repel(data=subset(common1, genotype %in% interest), aes(label=genotype))+
  facet_wrap(~trait, scale="free")
  


  
interest<-c("A10","B100","RIL_170","RIL_033","RIL_118")






just_height<-subset(common_geno, trait=="culm_height")

ggplot(just_height, aes(x=environment, y=value))+
  geom_boxplot(aes(fill=environment))+
  theme_classic()+xlab(label = NULL)+ylab(label = NULL)+
  stat_compare_means()+
  theme(rect = element_rect(fill = "transparent"))





kruskal.test(value~factor(environment), data=just_height)
summary(aov(value~factor(environment)+genotype, data=just_height))

summary(aov(value~genotype+environment, data=just_height))


Anova(lm(value~environment, data=just_height))

model=lm(value~environment, data=just_height)
leastsquare = lsmeans(model,
                      pairwise ~ environment,
                      adjust = "tukey")
cld(leastsquare,
    alpha   = 0.05,
    Letters = letters,
    adjust="tukey")


#make significance group key for plotting
environment<-c("thick_2014","dry_2014","dry_2015","wet_2014","wet_2015")
abc<-c("a","a","a","b","c")
abc_group<-data.frame(environment,abc)


png("./results/mixedModel_boxplot_CH.png")
ggplot(data=just_height, aes(x=environment, y=value))+
  geom_boxplot(aes(fill=environment))+
  stat_summary(fun.y=mean, color="red", geom="point")+
  geom_text(data=abc_group, aes(label=abc, y=900))
dev.off()





#loop this method through the whole trait set 







sstable <- Anova(model, type = 3) # Type III sums of squares is typical in social science research (it's the default in SPSS)

model<-lm(value~genotype + environment, data=just_height)


sstable<-Anova(model, type=3)

model <- lm(breaks ~ wool * tension, 
            data = warpbreaks, 
            contrasts = list(wool = "contr.sum", tension = "contr.poly"))






library(lme4)
library(lsmeans)
library(multcompView)
library(car)
install.packages("lsmeans")
install.packages("multcompView")
install.packages("car")


stat.test <- ToothGrowth %>%
  group_by(dose) %>%
  t_test(len ~ supp) %>%
  adjust_pvalue() %>%
  mutate(y.position = 35)
stat.test





stat.test<-common_geno %>%
  group_by(trait) %>%
  anova_test(value~environment) %>%
  adjust_pvalue()
stat.test


stat.test<-just_height %>%
  kruskal.test(value~environment) %>%
  adjust_pvalue()
stat.test



















#try test to output data 


install.packages("lme4")
library(lme4)




data(mtcars)
mtcars <- mtcars


vars = names(mtcars)[c(1,5,6,7)]

models = lapply(setNames(vars, vars), function(var) {
  form = paste("cyl ~ disp + hp + ", var, "+ (carb|gear)")
  lmer(form, data=mtcars)
})









library(lme4)
vars = names(mtcars)[c(1,5,6,7)]
otherVars <- c("disp","hp","(carb|gear)")
formList <- lapply(vars,function(x) {
  reformulate(c(otherVars,x),response="cyl")
})
modList <- lapply(formList,lmer,data=mtcars)





###

library(ggplot2)
library(ggsignif)

data("ToothGrowth")
data('iris')

iris2<-iris[c(1:10,50:60,100:110,61:70,11:20,111:118),]

big_data<-cbind(iris2,ToothGrowth) #dummy data


plot<-ggplot(big_data, aes(Species, len)) +
  geom_boxplot() +
  geom_signif(comparisons =list(c("setosa", "virginica"),c('setosa','versicolor'),c('virginica','versicolor')),
              step_increase = 0.1)+
  facet_wrap(~supp) #create initial plot

pg<-ggplot_build(plot) #disassemble plot and obtain information

pv<-pg$data[[2]]$annotation #seek out p values

new<-as.factor(paste('p=',pv)) #add the desired prefix

pg$data[[2]]$annotation<-new #swap out the original annotation

q<-ggplot_gtable(pg) #reassemble the plot

plot(q) #generate new plot






