###########################################################################
### Beta Linear multiple Regression Analysis Metacognition - PID domain ### 
###########################################################################

library(betareg)

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total_filtered)

### preprocessing
d$age.norm <- (d$age - mean(d$age))/ sd(d$age)
d$gender <- ifelse(d$gender == "Male",1,0)
d$DomainNegativeAffect.norm <- (d$DomainNegativeAffect - mean(d$DomainNegativeAffect))/ sd(d$DomainNegativeAffect)
d$DomainDetachment.norm <- (d$DomainDetachment - mean(d$DomainDetachment))/ sd(d$DomainDetachment)
d$DomainAntagonism.norm <- (d$DomainAntagonism - mean(d$DomainAntagonism))/ sd(d$DomainAntagonism)
d$DomainDisinhibition.norm <- (d$DomainDisinhibition - mean(d$DomainDisinhibition))/ sd(d$DomainDisinhibition)
d$DomainPsychoticism.norm <- (d$DomainPsychoticism - mean(d$DomainPsychoticism))/ sd(d$DomainPsychoticism)
d$mc.norm <- (d$mc - 0.5) *2

# run the model
a=betareg(mc.norm ~ DomainNegativeAffect.norm+
       DomainDetachment.norm+
       DomainAntagonism.norm+
       DomainDisinhibition.norm+
       DomainPsychoticism.norm+
       gender +
       age.norm,
     data = d) 
summary(a)

save(a, file = "Data/Regression_Results/Metacognitive_sensitivity_Results/mc_PID_domain_Beta_linea_model.RData")
