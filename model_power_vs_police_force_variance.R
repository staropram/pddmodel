library(qs)
library(data.table)
library(stringr)
source("markdown_functions.R")
#ctab <- qread("ctabletest.qs")
#configList <- qread("configList.qs")
ctab <- make_significance_comparison_table(simOutputs)

# make a comparison table but using the SDs rather than the "BetweenForceVariance"
make_pf_sd_vs_power <- function(ctab) {
   melt1 <- melt(ctab, id.vars = "Outcome", variable.name = "Model", value.name = "Value",variable.factor=F,value.factor=F)
   melt1[,BetweenForceVariance:=gsub(".*_PV(.*)_1000","\\1",Model)]
   melt2 <- dcast(melt1, Model ~ Outcome, value.var = "Value")
   names(melt2) <- c("Model","Rehab","Hospital","Reoffend")
   # add in the "variability" as SDs
   melt2[,RehabSD:=unlist(lapply(configList[Model],function(cfg) {cfg$effects$rehab$baseline_sd}))]
   melt2[,HospitalSDG1:=unlist(lapply(configList[Model],function(cfg) {cfg$effects$hospitalisation$group1$baseline_sd}))]
   melt2[,HospitalSDG2:=unlist(lapply(configList[Model],function(cfg) {cfg$effects$hospitalisation$group2$baseline_sd}))]
   melt2[,ReoffendSD:=unlist(lapply(configList[Model],function(cfg) {cfg$effects$reoffending$baseline_sd}))]

   forceSelection <- c("AllForces","First6","First12")

   comparisonSeparated <- lapply(forceSelection,function(selection) {
      d <- melt2[str_detect(melt2$Model,selection)]
      d[,BetweenForceVariance:=gsub(".*_PV(.*)_1000","\\1",Model)]
      d[,Model:=NULL]
      setkey(d,"BetweenForceVariance")
      specialNames <- c("RehabSD","Rehab","HospitalSDG1","HospitalSDG2","Hospital","ReoffendSD","Reoffend")
      setcolorder(d,c("BetweenForceVariance",specialNames))
      names(d) <- c("BetweenForceVariance",specialNames)
      d
   })
   names(comparisonSeparated) <- forceSelection
   comparisonSeparated 
}
pfsd_vs_power <- make_pf_sd_vs_power(ctab)

# make some cool plots

plot_reoffending_power_vs_pf_sd <- function() {
   par(mar = c(5.1, 4.1, 6.1, 2.1))  # Default margins are c(5.1, 4.1, 4.1, 2.1)

   x <- pfsd_vs_power$AllForces$ReoffendSD*100
   y_AF <- pfsd_vs_power$AllForces$Reoffend
   y_F12 <- pfsd_vs_power$First12$Reoffend
   y_F6 <- pfsd_vs_power$First6$Reoffend

   ylimits <- c(
      min(c(y_AF,y_F12,y_F6)),
      max(c(y_AF,y_F12,y_F6))
   )

   plot(
      x,
      y_AF,
      xlab="Between-force baseline Reoffending SD (Percentage points)",
      ylab="% of 1000 simulations significant",
      type="b",
      pch=2
   )

   title("Impact of PDD on Reoffending Rate (Success of 1000 trials)\n as variance between police forces increases",line=3)
   title(paste0(
      "Baseline reoffending: ",
      baseConfig$effect$reoffending$baseline_min*100
      ,"%"
   ),line=1.9,col.main="darkred")
   title(paste0(
      "Impact: -",
      baseConfig$effects$reoffending$pdd_impact_min*100
      ,"%"
   ),line=0.8,col.main="darkgreen")

   lines(
      x,
      y_F12,
      type="b",
      pch=1,
      col="darkorange"
   )

   lines(
      x,
      y_F6,
      type="b",
      pch=3,
      col="darkred"
   )

   legend("topright",
      c("All forces","First 12 forces","First 6 forces"),
      pch=c(2,1,3),
      col=c("black","darkorange","darkred"),
      lty=1
   )
}


plot_rehab_power_vs_pf_sd <- function() {
   par(mar = c(5.1, 4.1, 6.1, 2.1))  # Default margins are c(5.1, 4.1, 4.1, 2.1)

   x <- pfsd_vs_power$AllForces$RehabSD*100
   y_AF <- pfsd_vs_power$AllForces$Rehab
   y_F12 <- pfsd_vs_power$First12$Rehab
   y_F6 <- pfsd_vs_power$First6$Rehab

   ylimits <- c(
      min(c(y_AF,y_F12,y_F6)),
      max(c(y_AF,y_F12,y_F6))
   )

   plot(
      x,
      y_AF,
      xlab="Between-force baseline Drug Treatment SD (Percentage points)",
      ylab="% of 1000 simulations significant",
      type="b",
      pch=2
   )
   
   title("Impact of PDD on Drug Treatment entry (Success of 1000 trials)\n as variance between police forces increases",line=3)
   title(paste0(
      "Baseline entry into treatment: ",
      baseConfig$effects$rehab$baseline_min*100,
      "%"
   ),line=1.9,col.main="darkred")
   title(paste0(
      "Impact: +",
      baseConfig$effects$rehab$pdd_impact_min*100,
      "%"
   ),line=0.8,col.main="darkgreen")

   lines(
      x,
      y_F12,
      type="b",
      pch=1,
      col="darkorange"
   )

   lines(
      x,
      y_F6,
      type="b",
      pch=3,
      col="darkred"
   )

   legend("topright",
      c("All forces","First 12 forces","First 6 forces"),
      pch=c(2,1,3),
      col=c("black","darkorange","darkred"),
      lty=1
   )
}

plot_hosp_power_vs_pf_sd <- function() {
   par(mar = c(5.1, 4.1, 6.1, 2.1))  # Default margins are c(5.1, 4.1, 4.1, 2.1)
   x1 <- pfsd_vs_power$AllForces$HospitalSDG1
   x2 <- pfsd_vs_power$AllForces$HospitalSDG2
   y_AF <- pfsd_vs_power$AllForces$Hospital
   y_F12 <- pfsd_vs_power$First12$Hospital
   y_F6 <- pfsd_vs_power$First6$Hospital

   ylimits <- c(
      min(c(y_AF,y_F12,y_F6)),
      max(c(y_AF,y_F12,y_F6))
   )

   plot(
      x1,
      y_AF,
      xlab="Between-force baseline Hospital SD",
      ylab="% of 1000 simulations significant",
      type="b",
      pch=2,
      ylim=ylimits
   )


   title(paste0(
      "Impact of PDD on Hospital entry (Success of 1000 trials)\n",
      "as variance between police forces increases. GROUP 1"
   ),line=3)
   title(paste0(
      "Baseline hospitalisation rate: ",
      format(baseConfig$effects$hospitalisation$group1$baseline_count_rate_min,digits=3)
   ),line=1.9,col.main="darkred")
   title(paste0(
      "Impact: -",
      format(baseConfig$effects$hospitalisation$group1$pdd_impact_min,digits=3)
   ),line=0.8,col.main="darkgreen")

   lines(
      x1,
      y_F12,
      type="b",
      pch=1,
      col="darkorange"
   )

   lines(
      x1,
      y_F6,
      type="b",
      pch=3,
      col="darkred"
   )

   usr_coords <- par("usr")

   # Base R usually places tick marks at pretty intervals
   x_ticks <- pretty(x2)

   # Print the tick positions
   print(x_ticks)

   par(new = TRUE)
   plot(x1, y_AF, xlab = "", ylab = "", axes = FALSE)
   # Add a Second X Axis
   axis(side = 3, at = x_ticks, labels = x_ticks)

   legend("topright",
      c("All forces","First 12 forces","First 6 forces"),
      pch=c(2,1,3),
      col=c("black","darkorange","darkred"),
      lty=1
   )
}

plot_hosp_power_vs_pf_sd_g1 <- function() {
   par(mar = c(5.1, 4.1, 6.1, 2.1))  # Default margins are c(5.1, 4.1, 4.1, 2.1)
   x <- pfsd_vs_power$AllForces$HospitalSDG1
   y_AF <- pfsd_vs_power$AllForces$Hospital
   y_F12 <- pfsd_vs_power$First12$Hospital
   y_F6 <- pfsd_vs_power$First6$Hospital

   ylimits <- c(
      min(c(y_AF,y_F12,y_F6)),
      max(c(y_AF,y_F12,y_F6))
   )

   plot(
      x,
      y_AF,
      xlab="Between-force baseline Hospital SD GROUP 1",
      ylab="% of 1000 simulations significant",
      type="b",
      pch=2,
      ylim=ylimits
   )

   title(paste0(
      "Impact of PDD on Hospital entry (Success of 1000 trials)\n",
      "as variance between police forces increases. GROUP 1"
   ),line=3)
   title(paste0(
      "Baseline hospitalisation rate: ",
      format(baseConfig$effects$hospitalisation$group1$baseline_count_rate_min,digits=3)
   ),line=1.9,col.main="darkred")
   title(paste0(
      "Impact: -",
      format(baseConfig$effects$hospitalisation$group1$pdd_impact_min,digits=3)
   ),line=0.8,col.main="darkgreen")

   lines(
      x,
      y_F12,
      type="b",
      pch=1,
      col="darkorange"
   )

   lines(
      x,
      y_F6,
      type="b",
      pch=3,
      col="darkred"
   )

   legend("topright",
      c("All forces","First 12 forces","First 6 forces"),
      pch=c(2,1,3),
      col=c("black","darkorange","darkred"),
      lty=1
   )
}

plot_hosp_power_vs_pf_sd_g2 <- function() {
   par(mar = c(5.1, 4.1, 6.1, 2.1))  # Default margins are c(5.1, 4.1, 4.1, 2.1)
   x <- pfsd_vs_power$AllForces$HospitalSDG2
   y_AF <- pfsd_vs_power$AllForces$Hospital
   y_F12 <- pfsd_vs_power$First12$Hospital
   y_F6 <- pfsd_vs_power$First6$Hospital

   ylimits <- c(
      min(c(y_AF,y_F12,y_F6)),
      max(c(y_AF,y_F12,y_F6))
   )

   plot(
      x,
      y_AF,
      xlab="Between-force baseline Hospital SD GROUP 2",
      ylab="% of 1000 simulations significant",
      type="b",
      pch=2,
      ylim=ylimits
   )

   title(paste0(
      "Impact of PDD on Hospital entry (Success of 1000 trials)\n",
      "as variance between police forces increases. GROUP 2"
   ),line=3)
   title(paste0(
      "Baseline hospitalisation rate: ",
      format(baseConfig$effects$hospitalisation$group2$baseline_count_rate_min,digits=3)
   ),line=1.9,col.main="darkred")
   title(paste0(
      "Impact: -",
      format(baseConfig$effects$hospitalisation$group2$pdd_impact_min,digits=3)
   ),line=0.8,col.main="darkgreen")

   lines(
      x,
      y_F12,
      type="b",
      pch=1,
      col="darkorange"
   )

   lines(
      x,
      y_F6,
      type="b",
      pch=3,
      col="darkred"
   )

   legend("topright",
      c("All forces","First 12 forces","First 6 forces"),
      pch=c(2,1,3),
      col=c("black","darkorange","darkred"),
      lty=1
   )
}

#graphics.off()
#plot_reoffending_power_vs_pf_sd()
#plot_rehab_power_vs_pf_sd()
#plot_hosp_power_vs_pf_sd_g1()
#plot_hosp_power_vs_pf_sd_g2()

ftest <- function() {
   x <- pfsd_vs_power$AllForces$ReoffendSD*100
   y <- pfsd_vs_power$AllForces$Reoffend
   log_y <- log(y)

   d <- data.table(x,y)

   m1 <- lm(log_y ~ x,d)

   a0 <- coef(m1)["(Intercept)"]
   a1 <- coef(m1)["x"]
   exponential_fit <- function(x) {
        exp(a0) * exp(a1 * x)
   }

   x_range <- seq(min(x), max(x), length.out = 100)
   y_pred <- exponential_fit(x_range)
   plot(x, y, main = "Exponential Fit Using Least Squares", xlab = "X", ylab = "Y", pch = 19)
   lines(x_range, y_pred, col = "red", lwd = 2)
}

