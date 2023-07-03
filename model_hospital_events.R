library(readxl)
xlfn <- "data/HUPIO_selected_admissions.xlsx"

# hospital admission rates
hospital_admission_rates <- data.table(read_excel(xlfn,
	sheet="Sheet1",
	na=c(""),
	col_types=c(
		rep("text",3),
		rep("numeric",3)
	)
))

hospital_admissions_by_offender_status <- hospital_admission_rates[,list(Rate=sum(Value)/1000),by=Exposure]

hospital_admission_rate_group1 <- hospital_admissions_by_offender_status[Exposure==F]$Rate
hospital_admission_rate_group2 <- hospital_admissions_by_offender_status[Exposure==T]$Rate

hospital_admission_dispersions <- hospital_admission_rates[Variable=="Qpdispersion",list(MeanQPD=mean(Value)),by="Exposure"]
hospital_admission_dispersion_group1 <- hospital_admission_dispersions[Exposure==F,MeanQPD]
hospital_admission_dispersion_group2 <- hospital_admission_dispersions[Exposure==T,MeanQPD]


#print(paste0("hospital_admission_rate_group1: ",hospital_admission_rate_group1))
#print(paste0("hospital_admission_rate_group2: ",hospital_admission_rate_group2))

set_hospital_rates_from_real_data <- function(config,impactFractionGroup1,impactFractionGroup2) {
   # Group 1
   cg1 <- config$effects$hospitalisation$group1
   cg1$dispersion <- hospital_admission_dispersion_group1

	cg1$baseline_count_rate_min <- hospital_admission_rate_group1
	cg1$baseline_count_rate_max <- hospital_admission_rate_group1

	if(!is.na(impactFractionGroup1)) {
		cg1$pdd_impact_min <- hospital_admission_rate_group1*impactFractionGroup1
		cg1$pdd_impact_max <- hospital_admission_rate_group1*impactFractionGroup1
	}

   # Group 2
   cg2 <- config$effects$hospitalisation$group2
   cg2$dispersion <- hospital_admission_dispersion_group2

	cg2$baseline_count_rate_min <- hospital_admission_rate_group2
	cg2$baseline_count_rate_max <- hospital_admission_rate_group2

	if(!is.na(impactFractionGroup2)) {
		cg2$pdd_impact_min <- hospital_admission_rate_group2*impactFractionGroup2
		cg2$pdd_impact_max <- hospital_admission_rate_group2*impactFractionGroup2
	}

   config$effects$hospitalisation$group1 <- cg1
   config$effects$hospitalisation$group2 <- cg2
	config
}

#cat(rep("\n",5))
#x <- set_hospital_rates_from_real_data(configTemplate,0.1,0.15)
#print_outcome_rates(x)
