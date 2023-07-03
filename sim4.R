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

# set some options for our baseConfig
baseConfig <- configTemplate
baseConfig$sim$num_models_per_sim <- 1000
baseConfig$sim$save_models <- T

# impacts to test (of whatever baseline is)
impactsToTest <- c(0.01,0.05,0.075,0.1,0.2,0.25,0.3,0.4,0.5)

# for each variability, do AllForces,First6Forces,First12Forces
configList <- lapply(impactsToTest,function(impact) {
   simName <- paste0("ImpactVar_",impact)

   # set the hospital event rates from the actual data
   currentConfig <- set_hospital_rates_from_real_data(
      baseConfig,
      impact, # impact is a fraction of baseline, group 1
      impact*1.05 # group 2, let us assume G2 impact is 5% more than G1
   )

   # set the impact for rehab based on percentage of original baseline
   # note that baseline min and max are identical at this point and
   # if we make the min and max impact the same, then the impact will
   # be a constant
   currentConfig$effects$rehab$pdd_impact_min <- currentConfig$effects$rehab$baseline_min*impact
   currentConfig$effects$rehab$pdd_impact_max <- currentConfig$effects$rehab$pdd_impact_min

   # set impact for reoffending based on percentage of original baseline
   # note that baseline min and max are identical at this point and
   # if we make the min and max impact the same, then the impact will
   # be a constant
   currentConfig$effects$reoffending$pdd_impact_min <- currentConfig$effects$reoffending$baseline_min*impact
   currentConfig$effects$reoffending$pdd_impact_max <- currentConfig$effects$reoffending$pdd_impact_min

   # assume 5% police force variability of baseline rate
   currentConfig <- modify_police_force_variability(currentConfig,0.05)

   # 1st is include all forces
   currentConfig$sim$name <- simName
   # use all forces
   currentConfig$sim$pf_filter <- NULL
   
   print(impact)
   print_outcome_rates(currentConfig)

   return(currentConfig)
}) 
names(configList) <- lapply(impactsToTest,function(impact) paste0("ImpactVar_",impact))

# this is just for the markdown output
#testConfig <- baseConfig
#modelTestData <- create_random_dataset(baseConfig)
#modelTestModel <- glmer(ReoffendedWithinYear ~ UsesPDD + PersistentOffender + (1|PoliceForceID), data = modelTestData$pd, family = 'binomial')

# run the metasim
dryRun <- F
simOutputs <- meta_sim(configList,"ImpactsWithVar",forceRerun=F,dryRun=dryRun)

# make a comparison table
if(!dryRun) {
   comparisonTable <- make_significance_comparison_table(simOutputs)
}

# render the Rmd
render("sim4.Rmd",output_dir="outputs")
