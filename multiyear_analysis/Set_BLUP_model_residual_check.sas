title"Calculating BLUPs for Setaria RIL field data";

*bringing in all ril field data collected in 2013 -2015;
data field_data;
length experiment $7 genotype $7 trait$35 exp $9 treatment $6 environment $12;
infile "\\file-server.igb.illinois.edu\themes\GEGC\LeakeyLab\Setaria\Setaria_RIL_multiyear\multiyear_analysis\Set_RIL_field_merged_phenotypes_all_years_clean_sas.csv" DSD dlm=',' firstobs=2 lrecl=20000;
input year experiment$ genotype$ subplot_id trait$ value exp$ plot treatment$ environment$;
run;
data ril_data; set field_data;
if trait in ('leaf_mass_per_DAS','stem_mass_per_DAS','panicle_mass_per_DAS','total_mass_per_DAS','vegetative_mass_per_DAS', ) then delete;
if trait in ('leaf_mass_at_harvest', 'panicle_mass_at_harvest','stem_mass_at_harvest','total_mass_at_harvest','vegetative_mass_at_harvest', 'lfblade_area') then new_value=log10(value);
ifenvironment 
else new_value=value;
run;

*splitting data into each experiment since the experimental design will decide the model;
/*Density 2013*/
data den2013; set ril_data;
where environment = 'density_2013';
if trait in ('leaf_number', 'lfblade_weight','panicle_number','sim_SLA','tiller_number') then new_value=log10(value);
run;

proc sort data=den2013; by trait; run;
proc univariate data = den2013 normal;by trait;
class treatment;
var new_value;
histogram / normal;
probplot;
run;
*testing if the variance of the residuals is equal;
proc glm data=den2013;by trait;class treatment;
model value =treatment;
means treatment / hovtest;
run;
proc glm data=den2013;by trait;class treatment;
model new_value =treatment;
means treatment / hovtest;
run;
*running mixed model to get predicted values;
PROC MIXED data=den2013 covtest ; by trait;/*Covtest provides estimates of variance components for your random model terms*/
class treatment plot genotype; 
model new_value = treatment plot(treatment)/ ddfm=kr outp=den2013_mixed_out residual; /*solution gives fixed effect, ddfm=kr uses the kenward rogers adjustment (everyone likes it), outp gives model estimates and outpm gives the mean model estimates, residual is residual*/
random genotype genotype*treatment; 
run;quit;

proc gplot data=den2013_mixed_out; 
by trait;
plot resid*pred=treatment; run;quit;
proc glm data=den2013_mixed_out;by trait;class treatment;
model resid =treatment;
means treatment / hovtest;
run;
PROC MIXED data=den2013 covtest ; by trait;/*Covtest provides estimates of variance components for your random model terms*/
class treatment plot genotype; 
model new_value = treatment / ddfm=kr outp=den2013_mixed_out2 residual; /*solution gives fixed effect, ddfm=kr uses the kenward rogers adjustment (everyone likes it), outp gives model estimates and outpm gives the mean model estimates, residual is residual*/
random genotype genotype*treatment; 
run;quit;

proc gplot data=dn2013_mixed_out2; 
by trait;
plot resid*pred=treatment; run;quit;
proc glm data=dn2013_mixed_out2;by trait;class treatment;
model resid =treatment;
run;

data den2014; set ril_data;
where environment = 'density_2014';
run;
data dry2013; set ril_data;
where environment = 'drought_2013';
run;
data dry2014; set ril_data;
where environment = 'drought_2014';
run;
data dry2015; set ril_data;
where environment = 'drought_2015';
run;
*mixed model to test GxE;
proc mixed data = den2013; class genotype treatment
