#-------------------------------------------------------------------------------
# create_random_participant_data: Generate simulated participant data for each
#                                 police force
#
# Parameters:
#   police (data.table): A data.table containing police force-specific info,
#                        including ParticipantCount, PoliceForceID, UsesPDD,
#                        RehabProb, and ReoffendingProb
#
# Returns:
#   pd (data.table): A data.table containing simulated participant data with
#                    columns for ParticipantID, PoliceForceID, UsesPDD,
#                    EnteredDrugTreatment, ReoffendedWithinYear,
#                    HospitalEventCount, OnboardingDate, and FollowupDate
#-------------------------------------------------------------------------------
create_random_participants <- function(config,police) {
   #print("create_random_participants")
   #print_outcome_rates(config)

	# Put all the participant data in one table. In reality this is 
	# probably one table provided by each force, we don't have to do that
	# here but I'll emulate that to make it easier to input force-specific
	# parameters at a later date
	pd <- rbindlist(lapply(1:nrow(police),function(i) {
		# number of participants and ID
		n <- police$ParticipantCount[i]
		id <- police$PoliceForceID[i]
		# number of persistent offenders
		nPO <- round(police$PersistentOffenderFraction[i]*n)

		d <- data.table(
			# identities
			ParticipantID=paste0(id,"_",1:n),
			PoliceForceID=rep(id,n),

			# pdd involvement
			UsesPDD=rep(police$UsesPDD[i],n),

			# offender type
			PersistentOffender=sample(c(rep(T,nPO),rep(F,n-nPO)),size=n),

			## OUTCOMES
			# a) entered drug treatment within a year
			EnteredDrugTreatment=rbinom(
				n,
				size=1,
				prob=police$RehabProb[i]
			),

			# b) reoffending rate within a year
			ReoffendedWithinYear=rbinom(
				n,
				size=1,
				prob=police$ReoffendingProb[i]
			)

		)

		# persistent offender hospitalisation mean count as a probability (size=1)
		group2HospRate <- police$HospitalisationRatePPGroup2[i]
		# mean hospitalisation count rate of everyone else as a probablity (size=1)
		group1HospRate <- police$HospitalisationRatePPGroup1[i]	

		# vector of hospitalisation probabilities for the participants
		hospRates <- rep(group1HospRate,n)
		hospRates[d$PersistentOffender] <- group2HospRate

      dispersions <- rep(config$effect$hospitalisation$group1$dispersion,n)
      dispersions[d$PersistentOffender] <- config$effect$hospitalisation$group2$dispersion

		# c) hospital event rate has to be done outside this loop as it depends
		# on the value of the offender group that the participant prior value
		# note, uses negative binomial
		d[,HospitalEventCount:=rnbinom(
			n,
			#size=config$effect$hospitalisation$pdd_impact_dispersion,
			size=dispersions,
			mu=hospRates
		)]
		d
	}))

	# The following are not dependent on force or can be calculated from existing vars

	# NOTE onboarding and follow-up are not used in the model as the outcomes
	# all assume some fixed time window "entered drug treatment within year"
	# "reoffended within year", "hospital events within year"
	# Onboarding date between Oct 21 and Sep 22, note POSIXct is in YY-MM-DD
	onboardingOpenDate <-as.POSIXct("21-10-01","UTC")
	onboardingCloseDate <- as.POSIXct("22-09-30","UTC")
	onboardingWindow <- difftime(onboardingCloseDate,onboardingOpenDate)
	pd$OnboardingDate <- onboardingOpenDate+ceiling(runif(nrow(pd))*onboardingWindow)

	# Follow-up time between 2 and 3 years, assume uniform followup dist
	pd[,FollowupDate:=OnboardingDate+as.difftime(730,units="days") 
		+ ceiling(runif(nrow(pd))*as.difftime(365,units="days"))][]

	pd
}

create_random_dataset <- function(config) {
	d <- list(pf=create_random_police_forces(config))
	d$pd <- create_random_participants(config,d$pf)
	d
}
