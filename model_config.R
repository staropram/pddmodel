# Note we could use YAML here but why bother when we can use base R

# MODEL 1 CONFIGURATION
configTemplate <- list(
	# - - - - - SIMULATION OPTIONS - - - - - -
   sim = list(
      # output variable will use this name
      name = "simulation_name",
      # police force rows to EXCLUDE from simulation NULL to include all, i.e exclude none
      pf_filter = NULL,
      # should model list be saved to disk?
      save_models = T,
      # number of models per simulation
      num_models_per_sim = 10,
      # cores to use in sim
      num_cores = 7,
      # each of these formulae will be ran in a given simulation
      formulae = list(
         # NAME             Modelfunction                    Formula                                              Family
         c("ReoffendedWithinYear","glmer","ReoffendedWithinYear ~ UsesPDD + PersistentOffender + (1|PoliceForceID)","binomial"),
         c("EnteredDrugTreatment","glmer","EnteredDrugTreatment ~ UsesPDD + PersistentOffender + (1|PoliceForceID)","binomial"),
         c("HospitalEventCount","glmmTMB","HospitalEventCount ~ UsesPDD + PersistentOffender + (1|PoliceForceID)","nbinom2")
      )
   ),

	# - - - - - POLICE FORCE OPTIONS - - - - -
	police=list(
		# The first option is whether we want a simple fixed model of
		# participation or participation based on population estimates
		# Set to T to generate the data from population estimates.
		# Set to F to make "completely generative data"
		use_population_estimate = T,

		# options for completely generative data
		gendata_number_of_forces = 15,
		gendata_fraction_in_pdd  = 0.6,

		# shared options
		mean_pdd_participants_per_force = 500
	),

	# - - - - - PDD EFFECT OPTIONS - - - - -
	effects=list(
		# Variance can be introduced to either the baseline, impact, or both
		# You can choose to generate from a normal distribution or a uniform distribution
		# 1) In the case of uniform, min,max in the options below are the limits
		# 2) In the case of normal we assume min and max bound 99.99% of the mass of the distribution
		impact_distribution = "normal",

		# probability of going into "rehab"
		# we assume that rehab admission rates increase in the PDD group
		# so impact values given are min,max increase in admission rate
      rehab = list(
         baseline_min = 0.1,
         baseline_max = 0.1,
         pdd_impact_min = 0.05,
         pdd_impact_max = 0.05
      ),

		# probability of reoffending within a year
		# we assume that reoffending decreases in the PDD group
		# so impact values given are min,max decrease in reoffending rate
      reoffending = list(
         baseline_min = 0.25,
         baseline_max = 0.25,
         pdd_impact_min = 0.03,
         pdd_impact_max = 0.03
      ),

		# number of hospital events per person
		# we assume that hospital events decrease in the PDD group
		# so impact value given are min,max decrease in count rate
		# we have 2 groups to model with different baseline rates
      hospitalisation = list(
		   # GROUP 1 (low level offenders)
         group1 = list(
		      baseline_count_rate_min = 0.025,
		      baseline_count_rate_max = 0.025,
		      pdd_impact_min = 0.2,
		      pdd_impact_max = 0.2,
            dispersion = 3.27
         ),
		   # GROUP 2
         group2 = list(
		      baseline_count_rate_min = 0.123,
		      baseline_count_rate_max = 0.123,
		      pdd_impact_min = 0.2,
		      pdd_impact_max = 0.2,
            dispersion = 5.39
         ),
		   pdd_impact_dispersion = 5
      )
	)
)
