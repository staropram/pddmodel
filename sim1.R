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

# run a basic simulation to compare power with 
# all police forces vs just the first 6

# setup the basic config from the configTemplate

# set the hospital event rates from the actual data
# assume PDD has a 10% impact for group 1 and a 15% impact for group 2
config <- set_hospital_rates_from_real_data(
	configTemplate,
	0.1, # impact is a fraction of baseline, this is the value used for group 1
	0.15 # value for group 2
)

# add 1% baseline variability between police forces
policeForceVariability <- 0.01
config <- modify_police_force_variability(config,policeForceVariability)

# set number of models per sim
config$sim$num_models_per_sim <- 1000

# this is just for the markdown output
testConfig <- config
modelTestData <- create_random_dataset(config)
modelTestModel <- glmer(ReoffendedWithinYear ~ UsesPDD + PersistentOffender + (1|PoliceForceID), data = modelTestData$pd, family = 'binomial')

# make two configs, one with all police forces
configAll <- config
configAll$sim$name <- "AllForces"
configAll$sim$pf_filter <- NULL
configAll$sim$save_models <- T

# and one with only the first 6
configF6 <- config
configF6$sim$name <- "First6Forces"
configF6$sim$pf_filter <- paste0("PF",7:18)
configF6$sim$save_models <- T

# make a list of these configs
configs <- list(configAll,configF6)

# run the metasim
simOutputs <- meta_sim(configs,"AllvsF6",forceRerun=F,lite=T)

# make a comparison table
comparisonTable <- make_significance_comparison_table(simOutputs)

# render the Rmd
render("sim1.Rmd",output_dir="outputs")
