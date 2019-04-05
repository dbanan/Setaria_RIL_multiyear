##formatting BLUP data for QTL analysis

library(tidyverse)
load('RIL_BLUP.Rdata')
# QTL dataset needs following columns in this order:
# Obs	experiment	year	treatment	plot	subplot_id	id	sampling

#making it wide
head(rils.blups2)
x=rils.blups2%>%select(environment,experiment, year, treatment, plot, subplot_id, genotype, trait,new_predicted)%>%split(.$environment)

den13_wide=x[[1]]%>%spread(key=trait, value=new_predicted)%>%
  mutate(Obs=1:nrow(.), experiment='DN13',sampling="null")%>%nest(sim_lfblade_area:tiller_number)%>%
  select(Obs,experiment, year, treatment, plot, subplot_id, genotype, sampling, data)%>%unnest()

den14_wide=x[[2]]%>%spread(key=trait, value=new_predicted)%>%
  mutate(Obs=1:nrow(.), experiment='DN14',sampling="null")%>%nest(lfblade_area:basal_circumference)%>%
  select(Obs,experiment, year, treatment, plot, subplot_id, genotype, sampling, data)%>%unnest()

dr13_wide=x[[3]]%>%spread(key=trait, value=new_predicted)%>%
  mutate(Obs=1:nrow(.), experiment='DR13',sampling="null")%>%nest(lfblade_area:tiller_number)%>%
  select(Obs,experiment, year, treatment, plot, subplot_id, genotype, sampling, data)%>%unnest()

dr14_wide=x[[4]]%>%spread(key=trait, value=new_predicted)%>%
  mutate(Obs=1:nrow(.), experiment='DR14',sampling="null")%>%nest(lfblade_area:basal_circumference)%>%
  select(Obs,experiment, year, treatment, plot, subplot_id, genotype, sampling, data)%>%unnest()

dr15_wide=x[[5]]%>%spread(key=trait, value=new_predicted)%>%
  mutate(Obs=1:nrow(.), experiment='DR15',sampling="null")%>%nest(lfblade_area:leaf_number_green)%>%
  select(Obs,experiment, year, treatment, plot, subplot_id, genotype, sampling, data)%>%unnest()
#write csv for all qtl formatted files
write.csv(den13_wide, './results/Set_RIL_datafiles_for_qtl/DN13_field_ril_data_for_qtl.csv',row.names = F)
write.csv(den14_wide, './results/Set_RIL_datafiles_for_qtl/DN14_field_ril_data_for_qtl.csv',row.names = F)
write.csv(dr13_wide, './results/Set_RIL_datafiles_for_qtl/DR13_field_ril_data_for_qtl.csv',row.names = F)
write.csv(dr14_wide, './results/Set_RIL_datafiles_for_qtl/DR14_field_ril_data_for_qtl.csv',row.names = F)
write.csv(dr15_wide, './results/Set_RIL_datafiles_for_qtl/DR15_field_ril_data_for_qtl.csv',row.names = F)
#END#