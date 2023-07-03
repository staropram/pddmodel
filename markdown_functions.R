make_significance_pie <- function(d,threshold,N) {
	dNotNA <- d[!is.na(d)]
	# calculate proportion exceeding threshold
	prop_above_threshold <- sum(dNotNA>threshold) / length(dNotNA)
	prop_below_threshold <- 1 - prop_above_threshold

	# define color palette
	colors <- c("#E69F00", "#56B4E9")

	# create pie chart with numeric labels
	pie(c(prop_above_threshold, prop_below_threshold),
		 labels = sprintf("%.1f%%", c(prop_above_threshold, prop_below_threshold)*100),
		 main = paste("Proportion of significant models at 0.05 across",N,"executions"),
		 col = colors)

	# add legend
	legend("topright", c("Not significant", "Significant"),
			 fill = colors, border = NA)
}

# XXX generalise this to whatever the outcomes are
make_significance_table_from_sim_output <- function(simOutput) {
   sig <- simOutput$significance
   reoffendingSigs <- sig$ReoffendedWithinYear[!is.na(sig$ReoffendedWithinYear)]
   reoffendingSigProp <- sum(reoffendingSigs<0.05)/length(reoffendingSigs)
   enteredDrugSigs <- sig$EnteredDrugTreatment[!is.na(sig$EnteredDrugTreatment)]
   enteredDrugSigProp <- sum(enteredDrugSigs<0.05)/length(enteredDrugSigs)
   hospitalSigs <- sig$HospitalEventCount[!is.na(sig$HospitalEventCount)]
   hospitalSigProp <- sum(hospitalSigs<0.05)/length(hospitalSigs)
   d <- data.table(
      "Outcome"=c("ReoffendedWithinYear","EnteredDrugTreatment","HospitalEventCount"),
      "Value"=c(reoffendingSigProp,enteredDrugSigProp,hospitalSigProp)
   )
   names(d) <- c("Outcome",simOutput$config$sim$name)
   d
}

# compares the statistical power of a bunch of different simulation 
# configuration outputs
make_significance_comparison_table <- function(simOutputs) {
   tabs <- lapply(names(simOutputs),function(simName) {
      tab <- make_significance_table_from_sim_output(simOutputs[[simName]])
   })
   merged <- Reduce(function(a,b) {merge(a,b,by="Outcome")},tabs)
   names(merged) <- c("Outcome",names(simOutputs))
   merged
}

# makes a pretty comparison table from existing comparison
# table from 
make_pretty_comparison_table <- function(ctab) {
   comparisonMelted <- melt(ctab, id.vars = "Outcome", variable.name = "Model", value.name = "Value",variable.factor=F)
   comparisonMelted <- dcast(comparisonMelted, Model ~ Outcome, value.var = "Value")
   names(comparisonMelted) <- c("Model","Rehab","Hospital","Reoffend")
   comparisonSeparated <- lapply(c("AllForces","First6","First12"),function(selection) {
      d <- comparisonMelted[str_detect(comparisonMelted$Model,selection)]
      d[,BetweenForceVariance:=gsub(".*_PV(.*)_50","\\1",Model)]
      d[,Model:=NULL]
      setkey(d,"BetweenForceVariance")
      setcolorder(d,c("BetweenForceVariance","Rehab","Hospital","Reoffend"))
      names(d) <- c("BetweenForceVariance",paste(selection,c("Rehab","Hospital","Reoffend"),sep="_"))
      d
   })

   outputTable <- Reduce(function(a,b) { merge(a,b,on="BetweenForceVariance") },comparisonSeparated)
   # order this so we group by force selection all,first12,first6
   setcolorder(outputTable,c(1,2,8,5,3,9,6,4,10,7))
   names(outputTable) <- c("BetweenForceVariance",rep(c("All","First12","First6"),3))
}

# make a comparison table but using the SDs rather than the "BetweenForceVariance"
make_pretty_comparison_table_sds <- function(ctab) {
   melt1 <- melt(ctab, id.vars = "Outcome", variable.name = "Model", value.name = "Value",variable.factor=F)
   melt1[,BetweenForceVariance:=gsub(".*_PV(.*)_1000","\\1",Model)]
   melt2 <- dcast(melt1, Model ~ Outcome, value.var = "Value")
   names(melt2) <- c("Model","Rehab","Hospital","Reoffend")
   # add in the "variability" as SDs
   melt2[,RehabSD:=lapply(configList[Model],function(cfg) {cfg$effects$rehab$baseline_sd})]
   melt2[,HospitalSDG1:=lapply(configList[Model],function(cfg) {cfg$effects$hospitalisation$group1$baseline_sd})]
   melt2[,HospitalSDG2:=lapply(configList[Model],function(cfg) {cfg$effects$hospitalisation$group2$baseline_sd})]
   melt2[,ReoffendSD:=lapply(configList[Model],function(cfg) {cfg$effects$reoffending$baseline_sd})]

   comparisonSeparated <- lapply(c("AllForces","First6","First12"),function(selection) {
      d <- melt2[str_detect(melt2$Model,selection)]
      d[,BetweenForceVariance:=gsub(".*_PV(.*)_1000","\\1",Model)]
      d[,Model:=NULL]
      setkey(d,"BetweenForceVariance")
      specialNames <- c("RehabSD","Rehab","HospitalSDG1","HospitalSDG2","Hospital","ReoffendSD","Reoffend")
      setcolorder(d,c("BetweenForceVariance",specialNames))
      names(d) <- c("BetweenForceVariance",paste(selection,specialNames,sep="_"))
      d
   })
   browser()

   outputTable <- Reduce(function(a,b) { merge(a,b,on="BetweenForceVariance") },comparisonSeparated)
   # order this so we group by force selection all,first12,first6
   setcolorder(outputTable,c(1,2,8,5,3,9,6,4,10,7))
   names(outputTable) <- c("BetweenForceVariance",rep(c("All","First12","First6"),3))
}
#make_pretty_comparison_table_sds(comparisonTable)
