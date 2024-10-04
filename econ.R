
#libraries
library(haven)
library(estimatr)
library(stats)
library(rdrobust)
library(rddensity)
library(rdd)
library(dplyr)
library(ggplot2)

#dataset
lmb=lmb

# remove NA from dataset
lmb[is.na(lmb) | lmb=="Inf"] = NA

#convert relevant variables from char to numeric
lmb2 <- transform(lmb, V173 = as.numeric(V173), V174 = as.numeric(V174), V175 = as.numeric(V175), V24=as.numeric(V24), V22=as.numeric(V22)) 

#create subsample of data where lagdemvoteshare is between 52 and 48 percent
lmb_sub=subset(lmb2, V24 >=0.48 & V24 <=0.52)

#My R import for some reason put each header in terms of V#, so this code is to reassign the Vs to the variables.
#V173=democrat, V174=lagdemocrat, V175=score
democrat=lmb_sub$V173
lagdemocrat=lmb_sub$V174
score=lmb_sub$V175

#results from close races
score_dem=lm(score~democrat)
score_lag=lm(score~lagdemocrat)
dem_lagdem=lm(democrat~lagdemocrat)
summary(score_dem)
summary(score_lag)
summary(dem_lagdem)

#create full sample variables
dem_full=lmb2$V173
lag_full=lmb2$V174
score_full=lmb2$V175

#regressions
full_scoredem=lm(score_full~dem_full)
full_scorelag=lm(score_full~lag_full)
full_demlag=lm(dem_full~lag_full)
summary(full_scoredem)
summary(full_scorelag)
summary(full_demlag)

#center demvoteshare around 0
demvoteshare_c = lmb2$V22 - 0.5

#regressions
lm1=lm(score_full~dem_full+demvoteshare_c)
lm2=lm(score_full~lag_full+demvoteshare_c)
lm3=lm(dem_full~lag_full+demvoteshare_c)
summary(lm1)
summary(lm2)
summary(lm3)

#create left and right side of cutoff
lmb48=subset(lmb_sub, V24 >=0.48 & V24 <0.505)
lmb52=subset(lmb_sub, V24 >=0.505 & V24 <=0.52)
#define variables for each side of the cutoff
lagdvs48=lmb48$V24
lagdvs52=lmb52$V24
demvoteshare_c48 = lmb48$V22 - 0.5
demvoteshare_c52 = lmb52$V22 - 0.5
score48=lmb48$V175
score52=lmb52$V175
dem48=lmb48$V173
dem52=lmb52$V173
lagdem48=lmb48$V174
lagdem52=lmb52$V174



#create regressions for each side of the cutoff
lm1_cut48=lm(score48~dem48+lagdvs48+demvoteshare_c48)
lm2_cut48=lm(score48~lagdem48+lagdvs48+demvoteshare_c48)
lm3_cut48=lm(dem48~lagdem48+lagdvs48+demvoteshare_c48)
lm1_cut52=lm(score52~dem52+lagdvs52+demvoteshare_c52)
lm2_cut52=lm(score52~lagdem52+lagdvs52+demvoteshare_c52)
lm3_cut52=lm(dem52~lagdem52+lagdvs52+demvoteshare_c52)
summary(lm1_cut48)
summary(lm2_cut48)
summary(lm3_cut48)
summary(lm1_cut52)
summary(lm2_cut52)
summary(lm3_cut52)

#create squared centered demvoteshare and then regressions
demvoteshare_c48_2=demvoteshare_c48^2
demvoteshare_c52_2=demvoteshare_c52^2
lm1_cut48_2=lm(score48~dem48+lagdvs48+demvoteshare_c48+demvoteshare_c48_2)
lm2_cut48_2=lm(score48~lagdem48+lagdvs48+demvoteshare_c48+demvoteshare_c48_2)
lm3_cut48_2=lm(dem48~lagdem48+lagdvs48+demvoteshare_c48+demvoteshare_c48_2)
lm1_cut52_2=lm(score52~dem52+lagdvs52+demvoteshare_c52+demvoteshare_c52_2)
lm2_cut52_2=lm(score52~lagdem52+lagdvs52+demvoteshare_c52+demvoteshare_c52_2)
lm3_cut52_2=lm(dem52~lagdem52+lagdvs52+demvoteshare_c52+demvoteshare_c52_2)
summary(lm1_cut48_2)
summary(lm2_cut48_2)
summary(lm3_cut48_2)
summary(lm1_cut52_2)
summary(lm2_cut52_2)
summary(lm3_cut52_2)

#create subsample within 5 percent of centered demvoteshare cutoff
lmb_center=transform(lmb2, V22=V22-0.5)
lmb_cut=subset(lmb_center, V22 >=-0.05 & V22 <=0.05)

#recreating variables from new subsample
dem_cut=lmb_cut$V173
lagdem_cut=lmb_cut$V174
score_cut=lmb_cut$V175
demvoteshare_cc = lmb_cut$V22
demvoteshare_cc2=demvoteshare_cc^2

#regressions
lm1_qsix=lm(score_cut~dem_cut*demvoteshare_cc+dem_cut*demvoteshare_cc2)
lm2_qsix=lm(score_cut~lagdem_cut*demvoteshare_cc+lagdem_cut*demvoteshare_cc2)
lm3_qsix=lm(dem_cut~lagdem_cut*demvoteshare_cc+lagdem_cut*demvoteshare_cc2)
summary(lm1_qsix)
summary(lm2_qsix)
summary(lm3_qsix)

#creating plots
lm1_plot= ggplot(lmb_cut, aes(lagdem_cut, score_cut)) +
  geom_point(aes(x = lagdem_cut, y = score_cut), data = lmb_cut) +
  stat_smooth(aes(lagdem_cut, score_cut), method = "loess") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)

lm2_plot= ggplot(lmb_cut, aes(dem_cut, score_cut)) +
  geom_point(aes(x = dem_cut, y = score_cut), data = lmb_cut) +
  stat_smooth(aes(dem_cut, score_cut), method = "loess") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)


lm3_plot= ggplot(lmb_cut, aes(dem_cut, lagdem_cut)) +
  geom_point(aes(x = lagdem_cut, y = dem_cut), data = lmb_cut) +
  stat_smooth(aes(lagdem_cut, dem_cut), method = "loess") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)



beta_X <- coef(lm)["lagdemocrat"]

# Check the statistical significance
p_value <- summary(lm)$coefficients["lagdemocrat", "Pr(>|t|)"]

# Interpret the results
if (p_value < 0.05) {
  if (beta_X > 0) {
    cat("There is a statistically significant positive effect of lagdemocrat on score.")
  } else {
    cat("There is a statistically significant negative effect of lagdemocrat on score.")
  }
} else {
  cat("The effect of lagdemocrat on score is not statistically significant.")
}