#Setaria RIL field multiyear analysis trait correlations

####data prep####
load("RIL_BLUP.Rdata")
load('Set_multiyear_common_traits_geno_means.Rdata')
####Heatmap####
heatmap.matrix=as.matrix(common.wide.complete[,c(5:10,16:23)])
rownames(heatmap.matrix)=paste(common.wide.complete$genotype, common.wide.complete$environment, sep="_")
rc = rainbow(nrow(heatmap.matrix))
rc[which(common.wide.complete$environment=='thick_2013')]='#80FF00FF'
rc[which(common.wide.complete$environment=='thick_2014')]='#00FF80FF'
rc[which(common.wide.complete$environment=='dry_2013')]='#FF0080FF'
rc[which(common.wide.complete$environment=='dry_2014')]='#FF00FFFF'
rc[which(common.wide.complete$environment=='dry_2015')]='#FF0000FF'
rc[which(common.wide.complete$environment=='wet_2014')]='#0080FFFF'
rc[which(common.wide.complete$environment=='wet_2015')]='#0000FFFF'
table(rc)
heatmap(heatmap.matrix, scale = 'column',RowSideColors = rc, col = terrain.colors(25))
#plant biomass is washing out the heatmap so subsetting to only include some of the plant level traits
colnames(common.wide.complete)
heatmap.matrix=as.matrix(common.wide.complete[,c(5:10,17:19,21:23)])
rownames(heatmap.matrix)=paste(common.wide.complete$genotype, common.wide.complete$environment, sep="_")
heatmap(heatmap.matrix, scale = 'column',col = terrain.colors(25))
#SLA and culm height are an out group no matter how 
heatmap.matrix=as.matrix(common.wide.complete[,c(5,6,8:10,20:22)])
rownames(heatmap.matrix)=paste(common.wide.complete$genotype, common.wide.complete$environment, sep="_")
rc = rainbow(nrow(heatmap.matrix))
rc[which(common.wide.complete$environment=='thick_2013')]='#80FF00FF'
rc[which(common.wide.complete$environment=='thick_2014')]='#00FF80FF'
rc[which(common.wide.complete$environment=='dry_2013')]='#FF0080FF'
rc[which(common.wide.complete$environment=='dry_2014')]='#FF00FFFF'
rc[which(common.wide.complete$environment=='dry_2015')]='#FF0000FF'
rc[which(common.wide.complete$environment=='wet_2014')]='#0080FFFF'
rc[which(common.wide.complete$environment=='wet_2015')]='#0000FFFF'
table(rc)
heatmap(heatmap.matrix, scale = 'column',RowSideColors = rc)

####CORRELATIONS####
#some pairs plots colored by environment 

common.wide.complete$environment=factor(paste(common_wide$treatment, common_wide$year, sep = '_'),
                               levels=c("thick_2013","dry_2013","thick_2014","dry_2014","wet_2014","dry_2015","wet_2015"),
                               ordered=T)

cols<-character(nrow(common_wide))
cols[]<-"black"
cols[common_wide$environment=="dry_2013"]<-"red"
cols[common_wide$environment=="dry_2015"]<-"orange"
cols[common_wide$environment=="thick_2013"]<-"green"
cols[common_wide$environment=="thick_2014"]<-"black"
cols[common_wide$environment=="dry_2014"]<-"pink"
cols[common_wide$environment=="wet_2014"]="blue"
cols[common_wide$environment=="wet_2015"]<-"purple"

pairs(common_wide[,c(6:16,22:24)], pch=21, col=cols)
pairs(common_wide[,c(6:11,17:24)], pch=21, col=cols)
#all
png("./results/pairs_all.png", width=800, height=800)
pairs(common_wide[,c(13,6,22,24,18,8,14,16,10,21,11,12,7,17)], pch=21, col=cols)
dev.off()      

#split into useful groups (hopefully)

#leaf and architecture 
png("./results/pairs_leaf_arch.png", width=900, height=700)
pairs(common.wide[,c(4,6,7,11,12,13,14,3,16)], pch=21, col=cols)
dev.off()

#leaf and devo 
png("./results/pairs_leaf_devo.png", width=900, height=700)
pairs(common.wide.complete[,c(4,6,7,11,12,13,14,8,9,10)], pch=21, col=cols)
dev.off()

#leaf and mass 
png("./results/pairs_leaf_mass.png", width=900, height=700)
pairs(common.wide.complete[,c(4,6,7,11,12,13,14,5,9,15,17,18)], pch=21, col=cols)
dev.off()

#mix
png("./results/pairs_mix_dim.png", width=900, height=700)
pairs(common.wide.complete[,c(6,7,14,8,9,10,3,16,17)], pch=21, col=cols)
dev.off()

png("./results/pairs_mix_elem.png", width=900, height=700)
pairs(common.wide.complete[,c(4,11,12,13,8,9,10,3,16,17)], pch=21, col=cols)
dev.off()