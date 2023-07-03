# This script contains the function for generating random police force data.
# see the model_1_config file for options loaded into the variable "config"

library(data.table)

# load the police force data containing PDD participation estimates
police_force_estimates <- read.csv('data/police_force_estimates.csv')

generate_impact_by_pdd_status_uniform <- function(usesPDD,baseline,minImpact,maxImpact,impactDirection,...) {
	ifelse(usesPDD,
		# pdd
		baseline + impactDirection*runif(length(usesPDD),minImpact, maxImpact),
		# no pdd
		baseline
	)
}

# get the SD of a normal distribution where 99.999% of the mass falls within minVal,maxVal
generate_sd_from_symmetric_interval <- function(minVal,maxVal,quantileWidth=0.9999) {
	# create a normal distribution parameterized by the min,max values
	# so that quantileWidth % of the mass falls within min,max

	# we assume that the values minVal,maxVal are found at the
	# boundaries defined by the quantileWidth, i.e for 95% we
	# expect minVal,maxVal at quantiles 2.5% and 97.5%

	# quantile at which you would expect to find minVal
	qMin <- (1-quantileWidth)/2

	# normal dist is symmetric so the mean is at mean(minVal,maxVal)
	meanImpact <- 0.5*(minVal+maxVal)

	# we want to figure out the sd and we know that we expect
	# meanImpact + sd*qnorm(qMin) == minVal
	# because we expect to find that particular value at qMin
	# so we can solve for sd and derive our sd
	sdImpact <- (minVal-meanImpact)/qnorm(qMin)
}

# RNG for normal dist with 99.999% of values between minVal,maxVal
# anything it generates outside these bounds are truncated to minVal,maxVal
random_bounded_normal <- function(N,minVal,maxVal) {
	# if the minimum and maximum probabilities are equal, then the output is constant
	if(minVal==maxVal) {
		vals <- rep(minVal,N)
	} else {
		sdNorm <- generate_sd_from_symmetric_interval(minVal,maxVal)
		muNorm <- 0.5*(minVal+maxVal)
		vals <- rnorm(N,muNorm,sdNorm)
		# at 99.999%, roughly 1 in 200,000 random points would fall out of the boundary
		# so we constrain to minBaseline,maxBaseline to ensure this cannot happen
		vals <- pmax(minVal,vals)
		vals <- pmin(maxVal,vals)
	}
	vals
}

# use a normal distribution centred on 0.5(minVal+maxVal) so that 99.999%
# of the mass falls between minVal and maxVal for each of baseline and impact params
generate_impact_by_pdd_status_normal <- function(usesPDD,minBaseline,maxBaseline,minImpact,maxImpact,impactDirection) {
	# number of police forces
	N=length(usesPDD)
	# generate the probs
	baselineProbs <- random_bounded_normal(N,minBaseline,maxBaseline)
	impactProbs <- random_bounded_normal(N,minImpact,maxImpact)

	ifelse(usesPDD,
		# pdd
		baselineProbs + impactDirection*impactProbs
		,
		# no pdd
		baselineProbs
	)
}

# Generate a random table of police forces according to the options in passed config
create_random_police_forces <- function(config) {
	# pfd = police force data
	pfd <- NULL

	# population estimate model
	if(config$police$use_population_estimate) {
		# start with the loaded population estimates
		pfd <- data.table(police_force_estimates)

		# rename column Treatment to UsesPDD
		names(pfd)[names(pfd)=="Treatment"] <- "UsesPDD"

		# there were a couple of "TBD" treatment status, randomly assign them
		pfd[pfd$UsesPDD=="TBD"]$UsesPDD <-
			c("Control","Intervention")[1+rbinom(sum(pfd$UsesPDD=="TBD"),1,0.5)]
		pfd$UsesPDD <- pfd$UsesPDD=="Intervention"
		# set their PeristentOffenderFraction too, 0.5 for controls, 0 for PDD
		pfd[is.na(PersistentOffenderFraction),PersistentOffenderFraction:=fifelse(UsesPDD,0,0.5)][]

		# scale the participant count according to relative population sizes
		mean_population <- mean(pfd$Population)
		pfd[,ParticipantCount:=round(Population/mean_population*config$police$mean_pdd_participants_per_force)][]
	} else {
		# generative police force data model
		num_pf <- config$police$gendata_number_of_forces
		num_pf_in_pdd <- round(num_pf*config$police$gendata_fraction_in_pdd)
		num_pf_in_control <- num_pf - num_pf_in_pdd
		pfd <- data.table(
			PoliceForceID=paste0("PF",1:num_pf),
			UsesPDD=sample(c(rep(T,num_pf_in_pdd),rep(F,num_pf_in_control))),
			ParticipantCount=config$police$mean_pdd_participants_per_force
		)
	}

	# introduce per-force variability with respect to outcomes
	# we can either assume uniform distribution or a normal distribution
	variabilityFunction <- ifelse(
		(config$effects$impact_distribution=="uniform"),
		generate_impact_by_pdd_status_uniform,
		generate_impact_by_pdd_status_normal
	)

	# Enters into drug treatment probability
   cfg <- config$effects$rehab
	pfd$RehabProb <- variabilityFunction(
		pfd$UsesPDD,
		cfg$baseline_min,
		cfg$baseline_max,
		cfg$pdd_impact_min,
		cfg$pdd_impact_max,
		1 # pdd increases treatment probability relative to baseline
	)

	# Probability of reoffending within one year
   cfg <- config$effects$reoffending
	pfd$ReoffendingProb <- variabilityFunction(
		pfd$UsesPDD,
		cfg$baseline_min,
		cfg$baseline_max,
		cfg$pdd_impact_min,
		cfg$pdd_impact_max,
		-1 # pdd decreases reoffending rate relative to baseline
	)

	# Hospitalisation event rate (per person) GROUP 1
   g1 <- config$effects$hospitalisation$group1
	pfd$HospitalisationRatePPGroup1 <- variabilityFunction(
		pfd$UsesPDD,
		g1$baseline_count_rate_min,
		g1$baseline_count_rate_max,
		g1$pdd_impact_min,
		g1$pdd_impact_max,
		-1 # pdd decreases the hospitalisation rate
	)

	# Hospitalisation event rate (per person) GROUP 2
   g2 <- config$effects$hospitalisation$group2
	pfd$HospitalisationRatePPGroup2 <- variabilityFunction(
		pfd$UsesPDD,
		g2$baseline_count_rate_min,
		g2$baseline_count_rate_max,
		g2$pdd_impact_min,
		g2$pdd_impact_max,
		-1 # pdd decreases the hospitalisation rate
	)

	pfd
}
