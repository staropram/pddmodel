# functions to modify a config file programmatically
library(stringr)

# modify the min and max variability according to a fixed percentage of the baseline
modify_police_force_variability <- function(config,fractionVariability) {
   fx <- config$effects

   # Drug treatment
   # make two new variables, save them in config so can refer in reporting
   fx$rehab$baseline <- fx$rehab$baseline_min
   fx$rehab$variation <- fx$rehab$baseline*fractionVariability
   # vary the rehab rate
   fx$rehab$baseline_min <- fx$rehab$baseline - fx$rehab$variation 
   fx$rehab$baseline_max <- fx$rehab$baseline + fx$rehab$variation 
   # record the computed SD of our generated normal dist
   fx$rehab$baseline_sd <- generate_sd_from_symmetric_interval(fx$rehab$baseline_min,fx$rehab$baseline_max)

   # Reoffending
   # make two new variables, save them in config so can refer in reporting
   fx$reoffending$baseline <- fx$reoffending$baseline_min
   fx$reoffending$variation <- fx$reoffending$baseline*fractionVariability
   # add variation to output
   fx$reoffending$baseline_min <- fx$reoffending$baseline - fx$reoffending$variation
   fx$reoffending$baseline_max <- fx$reoffending$baseline + fx$reoffending$variation
   # record the computed SD of our generated normal dist
   fx$reoffending$baseline_sd <- generate_sd_from_symmetric_interval(fx$reoffending$baseline_min,fx$reoffending$baseline_max)

   # Hospitalisation

   # Group 1
   g1 <- fx$hospitalisation$group1
   # make two new variables, save them in config so can refer in reporting
   g1$baseline <- g1$baseline_count_rate_min
   g1$variation <- g1$baseline*fractionVariability
   g1$baseline_count_rate_min <- g1$baseline - g1$variation
   g1$baseline_count_rate_max <- g1$baseline + g1$variation
   # record the computed SD of our generated normal dist
   g1$baseline_sd <- generate_sd_from_symmetric_interval(g1$baseline_count_rate_min,g1$baseline_count_rate_max)

   # set the config
   fx$hospitalisation$group1 <- g1

   # Group 2
   g2 <- fx$hospitalisation$group2
   # make two new variables, save them in config so can refer in reporting
   g2$baseline <- g2$baseline_count_rate_min
   g2$variation <- g2$baseline*fractionVariability
   g2$baseline_count_rate_min <- g2$baseline - g2$variation
   g2$baseline_count_rate_max <- g2$baseline + g2$variation
   # record the computed SD of our generated normal dist
   g2$baseline_sd <- generate_sd_from_symmetric_interval(g2$baseline_count_rate_min,g2$baseline_count_rate_max)

   # set the config
   fx$hospitalisation$group2 <- g2

   # set the config
   config$effects <- fx

   config
}

print_outcome_rates <- function(cfg) {
   fx <- cfg$effects

   hosp <- fx$hospitalisation
   reof <- fx$reoffending
   rehb <- fx$rehab
   
   numsToCheck <- c(
      hosp$group1$baseline_count_rate_min,
      hosp$group1$baseline_count_rate_max,
      hosp$group1$pdd_impact_min,
      hosp$group1$pdd_impact_max,
      hosp$group2$baseline_count_rate_min,
      hosp$group2$baseline_count_rate_max,
      hosp$group2$pdd_impact_min,
      hosp$group2$pdd_impact_max,
      rehb$baseline_min,
      rehb$baseline_max,
      rehb$pdd_impact_min,
      rehb$pdd_impact_max,
      reof$baseline_min,
      reof$baseline_max,
      reof$pdd_impact_min,
      reof$pdd_impact_max
   )
   min_dp <- min_decimal_places(numsToCheck)

   cat("Hospital: \n")
   sprintString <- paste0("   G1: (%.",min_dp,"f,%.",min_dp,"f) - (%.",min_dp,"f,%.",min_dp,"f)\n")
   cat(sprintf(sprintString,
      hosp$group1$baseline_count_rate_min,
      hosp$group1$baseline_count_rate_max,
      hosp$group1$pdd_impact_min,
      hosp$group1$pdd_impact_max
   ))
   sprintString <- paste0("   G2: (%.",min_dp,"f,%.",min_dp,"f) - (%.",min_dp,"f,%.",min_dp,"f)\n")
   cat(sprintf(sprintString,
      hosp$group2$baseline_count_rate_min,
      hosp$group2$baseline_count_rate_max,
      hosp$group2$pdd_impact_min,
      hosp$group2$pdd_impact_max
   ))
   cat("Rehab: \n")
   sprintString <- paste0("   (%.",min_dp,"f,%.",min_dp,"f) + (%.",min_dp,"f,%.",min_dp,"f)\n")
   cat(sprintf(sprintString,
      rehb$baseline_min,
      rehb$baseline_max,
      rehb$pdd_impact_min,
      rehb$pdd_impact_max
   ))
   cat("Reoffending: \n")
   sprintString <- paste0("   (%.",min_dp,"f,%.",min_dp,"f) - (%.",min_dp,"f,%.",min_dp,"f)\n")
   cat(sprintf(sprintString,
      reof$baseline_min,
      reof$baseline_max,
      reof$pdd_impact_min,
      reof$pdd_impact_max
   ))

}

min_decimal_places <- function(vec,minDP=2) {
   vecString <- format(vec, scientific = FALSE, trim = TRUE)
   nums <- sapply(vecString,function(x) {
      decPart <- strsplit(x,"\\.")[[1]][2]
      nonZeroPos <- stringr::str_locate(decPart,"[1-9]")[1]
   })
   max(minDP,max(nums+(minDP-1),na.rm=T))
}

#print(min_decimal_places(c(10.2,0.0005,0.000533,3)))


#cat(rep("\n",5))
#print_outcome_rates(config)
#y <- modify_police_force_variability(config,0.1)
#cat(rep("\n",2))
#print_outcome_rates(y)
