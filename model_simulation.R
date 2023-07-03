#formulae <- data.table(rbind(
#	c("ReoffendedWithinYear","glmer",ReoffendedWithinYear ~ UsesPDD + PersistentOffender + (1|PoliceForceID),"binomial"),
#	c("EnteredDrugTreatment","glmer",EnteredDrugTreatment ~ UsesPDD + PersistentOffender + (1|PoliceForceID),"binomial"),
#	c("HospitalEventCount","glmmTMB",HospitalEventCount ~ UsesPDD + PersistentOffender + (1|PoliceForceID),"nbinom2")
#))
#names(formulae) <- c("Name","ModelFunction","Formula","Family")

# for fast serialisation of R objects to/from disk
library(qs)
# creates N models according to the specified config and other parameters
# lite version, computes and returns the significance vector
run_simulation_lite <- function(config,N,modelFunctionName,formula,family,policeForceFilter) {
	sigs <- mclapply(1:N,function(i) {
		dR <- create_random_dataset(config)
		dR$pd <- dR$pd[!PoliceForceID %in% policeForceFilter]
		modelFunction <- get(modelFunctionName)
		m <- modelFunction(formula, data=dR$pd, family=family)
      # get the model summary
		mSummary <- summary(m)
      rm(m)
      gc()
      # extract the significance
		if(mSummary$family!="nbinom2") {
			return(mSummary$coefficients["UsesPDDTRUE","Pr(>|z|)"])
		} else {
			return(mSummary$coefficients$cond["UsesPDDTRUE","Pr(>|z|)"])
		}
	},mc.cores=config$sim$num_cores)
	sigs
}

# lite version of simulation that just returns significance rather than
# all the models
run_simulations_lite <- function(config) {
   # get variables we need from config
   formulae <- data.table(do.call(rbind,config$sim$formulae))
   # give the formulae table names
   names(formulae) <- c("Name","ModelFunction","Formula","Family")
   formulae$Formula <- lapply(formulae$Formula,as.formula)
   # set some other local params
   N <- config$sim$num_models_per_sim
   policeForceFilter <- config$sim$pf_filter

	# make random models with default config and look at the prs
	sigs <- lapply(1:nrow(formulae),function(i) {
		startTime <- proc.time()
		ms <- run_simulation_lite(
			config,
			N,
			formulae$ModelFunction[[i]],
			formulae$Formula[[i]],
			formulae$Family[[i]],
			policeForceFilter
		)
		endTime <- proc.time()
		elapsedTime <- endTime-startTime
		print(paste("SIMULATION TIME: ",elapsedTime[[3]]))
		ms
	})
	names(sigs) <- formulae$Name

   list(significance=sigs,config=config)
}

# creates N models according to the specified config and other parameters
run_simulation <- function(config,N,modelFunctionName,formula,family,policeForceFilter) {
	models <- mclapply(1:N,function(i) {
		dR <- create_random_dataset(config)
		dR$pd <- dR$pd[!PoliceForceID %in% policeForceFilter]
		modelFunction <- get(modelFunctionName)
		m <- modelFunction(formula, data=dR$pd, family=family)
      # run the garbage collector
      gc()
		m
	},mc.cores=config$sim$num_cores)
	models
	list(models=models,significance=significance,config=config)
}

# creates N models for each outcome variable according to parameters
run_simulations <- function(config) {
   # get variables we need from config
   formulae <- data.table(do.call(rbind,config$sim$formulae))
   # give the formulae table names
   names(formulae) <- c("Name","ModelFunction","Formula","Family")
   formulae$Formula <- lapply(formulae$Formula,as.formula)
   # set some other local params
   N <- config$sim$num_models_per_sim
   policeForceFilter <- config$sim$pf_filter

	# make random models with default config and look at the prs
	models <- lapply(1:nrow(formulae),function(i) {
		startTime <- proc.time()
		ms <- run_simulation(
			config,
			N,
			formulae$ModelFunction[[i]],
			formulae$Formula[[i]],
			formulae$Family[[i]],
			policeForceFilter
		)
		endTime <- proc.time()
      gc()
		elapsedTime <- endTime-startTime
		print(paste("SIMULATION TIME: ",elapsedTime[[3]]))
		ms
	})
	names(models) <- formulae$Name

	# calculate significance
	significance <- lapply(1:nrow(formulae),function(i) {
		currentModels <- models[formulae$Name[[i]]]
		ms <- mclapply(currentModels[[1]],function(m) {
			mSummary <- summary(m)
			if(mSummary$family!="nbinom2") {
				return(mSummary$coefficients["UsesPDDTRUE","Pr(>|z|)"])
			} else {
				return(mSummary$coefficients$cond["UsesPDDTRUE","Pr(>|z|)"])
			}
		},mc.cores=config$sim$num_cores)
		unlist(ms)
	})
	names(significance) <- formulae$Name

	list(models=models,significance=significance,config=config)
}

if(!exists("global_sim_register")) {
   global_sim_register <- list()
}

# Recursive function to rename elements that contain "ppd"
rename_elements <- function(x) {
   # If x is a list, apply this function recursively
   if(is.list(x)) {
      names(x) <- ifelse(grepl("ppd", names(x)), gsub("ppd", "pdd", names(x)), names(x))
      x <- lapply(x, rename_elements)
   }
   return(x)
}

# runs an N-model simulation for all outcomes, for each of the configs passed to it
# uses metaname as a file prefix for saving models
meta_sim <- function(configList,metaname,forceRerun=F,dryRun=F,lite=T) {
   cat("Running sim ",metaname,"\n")
   # loop through configList
   simOutputs <- lapply(configList,function(cfg) {
      # keep a global record of simulations with unique names
      simName <- paste0(metaname,"_lite_",cfg$sim$name,"_",cfg$sim$num_models_per_sim)
      outDir <- paste0("simdata/",metaname)
      fn <- paste0(outDir,"/",simName,".qs")
      cat("Running config",simName,"\n")
      if(dryRun) {
         return(NULL)
      }

      # if not forcing a rerun, load the simulation
      # from disk if necessary
      if(!forceRerun) {
         # load sim from disk if not in register
         if(!simName %in% names(global_sim_register)) {
            if(file.exists(fn)) {
               cat('Loading "',fn,'" back in, this may take a long time\n')
               simOutput <- qread(fn)
               if(str_detect(names(simOutput$config$effects$reoffending)[3],"ppd")) {
                  #browser()
                  cat("renaming elements for",fn,"\n")
                  simOutput$config <- rename_elements(simOutput$config)
                  cat("saving again with renamed for",fn,"\n")
                  qsave(simOutput,file=fn)
               }
               global_sim_register[[simName]] <- simOutput
               return(simOutput)
            }
            # could not load sim from disk, do nothing
            # as will go onto simulation
         } else {
            # sim name already in register, return it
            return(global_sim_register[[simName]])
         }
      }

      # run the simulation
      if(lite) {
         simOutput <- run_simulations_lite(cfg)
      } else {
         simOutput <- run_simulations(cfg)
      }
      # add it to the register (NOTE: global assignment)
      global_sim_register[[simName]] <<- simOutput

      # save model if asked to
      if(cfg$sim$save_models) {
         # check output directory exists and create if necessary
         if(!dir.exists(outDir)) {
            dir.create(outDir)
         }
         cat("Saving model \"",simName,"\", this may take a long time\n")
         qsave(simOutput,file=fn)
      }

      simOutput
   })

   names(simOutputs) <- lapply(configList,function(cfg) {paste0(cfg$sim$name,"_",cfg$sim$num_models_per_sim)})
   simOutputs
}
