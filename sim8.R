# the purpose of this simulation is to estimate the A&E admission rate 
# between all force participation and 6 forces only participation by making
# some assumptions about the relationship between hospital and A&E admissions

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
library(tidyverse)

source('model_config.R')
source('model_config_modification.R')
source('model_police_forces.R')
source('model_participants.R')
source('model_hospital_events.R')
source('model_simulation.R')
source('markdown_functions.R')

# run a basic simulation to compare power with 
# all police forces vs just the first 6


config <- configTemplate
config <- set_hospital_rates_from_real_data(
	configTemplate,
	0.25, # impact is a fraction of baseline, this is the value used for group 1
	0.25 # value for group 2
)
config$sim$num_models_per_sim <- 1000

# we want to assume that the A&E rate is 4x that of the hospitalisation rate and 
# that G1 and G2 share this rate approximately, and that impact is 25% of baseline
make_AandE_config <- function(cfg,baselineMultiplier,impactMultiplier) {
   currentG2Baseline <- cfg$effects$hospitalisation$group2$baseline_count_rate_min
   newG2Baseline <- currentG2Baseline*baselineMultiplier
   newG2Impact <- newG2Baseline*impactMultiplier
   cfg$effects$hospitalisation$group2$baseline_count_rate_min <- newG2Baseline
   cfg$effects$hospitalisation$group2$baseline_count_rate_max <- newG2Baseline
   cfg$effects$hospitalisation$group2$pdd_impact_min <- newG2Impact
   cfg$effects$hospitalisation$group2$pdd_impact_max <- newG2Impact

   cfg$effects$hospitalisation$group1$baseline_count_rate_min <- newG2Baseline
   cfg$effects$hospitalisation$group1$baseline_count_rate_max <- newG2Baseline
   cfg$effects$hospitalisation$group1$pdd_impact_min <- newG2Impact
   cfg$effects$hospitalisation$group1$pdd_impact_max <- newG2Impact
   cfg
}


print("new config")
print_outcome_rates(config)

# no variability between police forces
policeForceVariability <- 0.0
config <- modify_police_force_variability(config,policeForceVariability)


# this is just for the markdown output
testConfig <- config
modelTestData <- create_random_dataset(config)
modelTestModel <- glmer(ReoffendedWithinYear ~ UsesPDD + PersistentOffender + (1|PoliceForceID), data = modelTestData$pd, family = 'binomial')

# make a few diff configs for diff impacts and baselines
baselineMultipliers <- c(2,3,4)
impactMultipliers <- c(0.05,0.1,0.15,0.2,0.25)
configs <- unlist(lapply(baselineMultipliers,function(bm) {
   unlist(lapply(impactMultipliers,function(im) {
      modConfig <- make_AandE_config(config,bm,im)

      # make two configs, one with all police forces
      configAll <- modConfig
      configAll$sim$name <- paste0("AllForces_",bm,"_",im)
      configAll$sim$pf_filter <- NULL
      configAll$sim$save_models <- T

      # and one with only the first 6
      configF6 <- modConfig
      configF6$sim$name <- paste0("First6Forces_",bm,"_",im)
      configF6$sim$pf_filter <- paste0("PF",7:18)
      configF6$sim$save_models <- T
      list(configAll,configF6)
   }),recursive=F)
}),recursive=F)


# run the metasim
simOutputs <- meta_sim(configs,"AandE_AllvsF6",forceRerun=F,lite=T)

# make a comparison table
comparisonTable <- make_significance_comparison_table(simOutputs)


hospital <- comparisonTable %>% 
  slice(2) %>%
  pivot_longer(
    cols = -Outcome,  # Exclude the Outcome column from pivoting
    names_to = c("Experiment", "BaselineMultiplier", "ImpactMultiplier"),  # Define new column names
    names_pattern = "(.*)_([0-9]*)_([0-9]*.[0-9]*)"  # Define the pattern in the current column names
) %>% data.table()

hospital_diff <- hospital[,
	list(
		All=first(round(value*100,digits=1)),
		F6=last(round(value*100,digits=1)),
		Diff=round((first(value)-last(value))*100,digits=1)
	),
	by=list(BaselineMultiplier,ImpactMultiplier)
]

origG2Baseline <- configTemplate$effects$hospitalisation$group2$baseline_count_rate_min
hospital_diff[,Baseline:=as.numeric(BaselineMultiplier)*origG2Baseline]

# render the Rmd
render("sim8.Rmd",output_dir="outputs")
