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
# assume PDD has a 10% impact for group 1 and a 15% impact for group 2
config <- set_hospital_rates_from_real_data(
	configTemplate,
	0.2, # impact is a fraction of baseline, this is the value used for group 1
	0.30 # value for group 2
)

# set some options for our baseConfig
baseConfig <- config
baseConfig$sim$num_models_per_sim <- 1000
baseConfig$sim$save_models <- T
print("Base config rates:")
print_outcome_rates(baseConfig)


# double the effects
# rehab
baseConfig$effects$rehab$pdd_impact_min <- baseConfig$effects$rehab$pdd_impact_min*2
baseConfig$effects$rehab$pdd_impact_max <- baseConfig$effects$rehab$pdd_impact_min
# reoffending
baseConfig$effects$reoffending$pdd_impact_min <- baseConfig$effects$reoffending$pdd_impact_min*2
baseConfig$effects$reoffending$pdd_impact_max <- baseConfig$effects$reoffending$pdd_impact_min

# make a bunch of configs for differing police force variability
policeForceVariabilities <- c(0.1)

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
modelTestData <- create_random_dataset(baseConfig)
modelTestModel <- glmer(ReoffendedWithinYear ~ UsesPDD + PersistentOffender + (1|PoliceForceID), data = modelTestData$pd, family = 'binomial')

# run the metasim
dryRun <- F
simOutputs <- meta_sim(configList,"PFVarCompDispersionTest",forceRerun=F,dryRun=dryRun)

# make a comparison table
if(!dryRun) {
   comparisonTable <- make_significance_comparison_table(simOutputs)
}
