source('model_check_deps.R')
source("model_police_forces.R")
plot_norm_from_symmetric <- function(minVal,maxVal) {
   # Set mean and standard deviation
   mean_value <- 0.5*(minVal+maxVal)
   sd_value <- generate_sd_from_symmetric_interval(minVal,maxVal)

   # Create a function for the normal distribution PDF with the specified mean and sd
   normal_pdf <- function(x) {
        dnorm(x, mean = mean_value, sd = sd_value)
   }

   # Plot the normal distribution
   curve(normal_pdf, from = mean_value - 4 * sd_value, to = mean_value + 4 * sd_value,
               ylab = "Density", xlab = "Baseline Rehab Probability", main = "Normal Distribution for Between force Rehab probability",
                     col = "blue", lwd = 2)

   # Add 1 SD lines
   abline(v = mean_value + sd_value, col = "red", lty = 2, lwd = 1.5)
   abline(v = mean_value - sd_value, col = "red", lty = 2, lwd = 1.5)

   # Add 2 SD lines
   abline(v = mean_value + 2 * sd_value, col = "green", lty = 3, lwd = 1.5)
   abline(v = mean_value - 2 * sd_value, col = "green", lty = 3, lwd = 1.5)

   # Calculate the x-values for the 0.0001 and 0.9999 percentiles
   x_0001 <- qnorm(0.0001, mean_value, sd_value)
   x_9999 <- qnorm(0.9999, mean_value, sd_value)

   # Add vertical lines for the 0.0001 and 0.9999 percentiles
   abline(v = x_0001, col = "orange", lty = 2, lwd = 1.5)
   abline(v = x_9999, col = "orange", lty = 2, lwd = 1.5)

   legend("topright", # Position of the legend on the plot
          legend = c("PDF", "1 SD", "2 SD", "0.01% percentile", "99.99% percentile"), # Labels for each line
          col = c("blue", "red", "green", "orange", "orange"), # Colors for each label
          lty = c(1, 2, 3, 2, 2), # Line types for each label
          lwd = c(2, 1.5, 1.5, 1.5, 1.5), # Line widths for each label
          bty = "n", # No box around the legend
          cex = 0.8) # Font size


   # Add a grid to the plot
   grid()
}

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

plot_norm_from_symmetric(1,19)
#pfVars <- c(0.01,0.1)

# assume 2% var of baseline
pfVar <- 0.3

# assume baseline of 10%
baseline <- 0.1

variability <- baseline*pfVar

baseline_min <- baseline-variability
baseline_max <- baseline+variability


# what is the sd
baseline_sd <- generate_sd_from_symmetric_interval(baseline_min,baseline_max)


baseConfig<- set_hospital_rates_from_real_data(
   configTemplate,
   0.1, # impact is a fraction of baseline, this is the value used for group 1
   0.15 # value for group 2
)


baseConfig <- modify_police_force_variability(baseConfig,1.6)
print_outcome_rates(baseConfig)

fx <- baseConfig$effects

cat(sprintf("Rehab: %.2f%%, SD: %.2f%%, ~Min: %.2f%%, ~Max: %.2f%%\n",
	100*fx$rehab$baseline,
	100*fx$rehab$baseline_sd,
	100*fx$rehab$baseline_min,
	100*fx$rehab$baseline_max
))
cat(sprintf("Reoffend: %.2f%%, SD: %.2f%%, ~Min: %.2f%%, ~Max: %.2f%%\n",
	100*fx$reoffending$baseline,
	100*fx$reoffending$baseline_sd,
	100*fx$reoffending$baseline_min,
	100*fx$reoffending$baseline_max
))

# test model
modelTestData <- create_random_dataset(baseConfig)
#mReoffend <- glmer(EnteredDrugTreatment ~ UsesPDD + PersistentOffender + (1|PoliceForceID), data = modelTestData$pd, family = 'binomial')
modelTestModel <- glmer(ReoffendedWithinYear ~ UsesPDD + PersistentOffender + (1|PoliceForceID), data = modelTestData$pd, family = 'binomial')
print(summary(modelTestModel))
         #c("ReoffendedWithinYear","glmer","ReoffendedWithinYear ~ UsesPDD + PersistentOffender + (1|PoliceForceID)","binomial"),
         #c("EnteredDrugTreatment","glmer","EnteredDrugTreatment ~ UsesPDD + PersistentOffender + (1|PoliceForceID)","binomial"),
         #c("HospitalEventCount","glmmTMB","HospitalEventCount ~ UsesPDD + PersistentOffender + (1|PoliceForceID)","nbinom2"
