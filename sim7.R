source("model_check_deps.R")

library(lme4)
library(MASS)
library(meta)
library(glmmTMB)
library(parallel)
library(rmarkdown)
library(knitr)
library(DT)
library(kableExtra)

source('model_config.R')
source('model_config_modification.R')
source('model_police_forces.R')
source('model_participants.R')
source('model_hospital_events.R')
source('model_simulation.R')
source('markdown_functions.R')

# I want to change the 

# set the hospital event rates from the actual data
# assume PDD has a 0% impact for group 1 and a 15% impact for group 2
config <- set_hospital_rates_from_real_data(
	configTemplate,
	0.0, # impact is a fraction of baseline, this is the value used for group 1
	0.15 # value for group 2
)

# set some options for our baseConfig
baseConfig <- config
baseConfig$sim$num_models_per_sim <- 1000
baseConfig$sim$save_models <- T
print("Base config rates:")
print_outcome_rates(baseConfig)

baseConfig$sim$formulae = list(
   # NAME             Modelfunction                    Formula                                              Family
   c("HospitalEventCount","glmmTMB","HospitalEventCount ~ UsesPDD + PersistentOffender + (1|PoliceForceID)","nbinom2")
)

# make a bunch of configs for differing police force variability
policeForceVariabilities <- c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)

# for each variability, do AllForces,First6Forces,First12Forces
configList <- lapply(policeForceVariabilities,function(pfv) {
   # add prescribed variability between police forces in outcome prevalance

   # make three config based on specified level of police force variability
   currentConfig <- modify_police_force_variability(baseConfig,pfv)

   # 1st is include all forces
   cA <- currentConfig
   cA$sim$name <- paste0("AllForces_PV",pfv)
   cA$sim$pf_filter <- NULL

   # 2nd is include only the first 6 forces
   cB <- currentConfig
   cB$sim$name <- paste0("First6Forces_PV",pfv)
   cB$sim$pf_filter <- paste0("PF",7:18)

   # 3rd is include only the first 12 forces
   cC <- currentConfig
   cC$sim$name <- paste0("First12Forces_PV",pfv)
   cC$sim$pf_filter <- paste0("PF",13:18)

   list(cA,cB,cC)
}) %>% unlist(.,recursive=F)
namesCartesian <- expand.grid(c("AllForces_PV","First6Forces_PV","First12Forces_PV"),policeForceVariabilities)
names(configList) <- paste0(namesCartesian[,1],namesCartesian[,2],"_",baseConfig$sim$num_models_per_sim)

# this is just for the markdown output
testConfig <- baseConfig
testConfig <- modify_police_force_variability(testConfig,0.05)
modelTestData <- create_random_dataset(testConfig)
modelTestModel <- glmer(ReoffendedWithinYear ~ UsesPDD + PersistentOffender + (1|PoliceForceID), data = modelTestData$pd, family = 'binomial')

# run the metasim
dryRun <- F
simOutputs <- meta_sim(configList,"PFVarCompHospAccurate",forceRerun=F,dryRun=dryRun)

# make a comparison table
if(!dryRun) {
   comparisonTable <- make_significance_comparison_table(simOutputs)
}

# render the Rmd
source('model_power_vs_police_force_variance.R')
render("sim7.Rmd",output_dir="outputs")
