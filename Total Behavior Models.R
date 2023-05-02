library(faraway)
library(effects)
library(MASS)
library(car)
library(leaps)
library(tidyverse)
library(mi)
library(alr4)
library(dplyr)

# Read in data
crime_data <- readRDS("/Users/edgil/Documents/Math Master/Math 6330 Statistical Consulting/cleaned_crime_data.rds")
#crime_data0 <- readRDS("/Users/edgil/Documents/Math Master/Math 6330 Statistical Consulting/cleaned_crime_data0.rds")
View(crime_data)

# Total Behaviors (Updated Q4.10 & Q4.9) Relating to Changes in Behavior in Safety
f_safety = total_behaviors ~ Q2.1 + Q2.4 + Q2.5 + Q2.6 + Q2.7 + Q2.8 + Q3.1 +
  Q4.1_1 + Q4.1_2 + Q4.1_3 + Q4.1_4 + Q4.3_casefile + Q4.3_crimejunkie +
  Q4.3_criminal + Q4.3_dateline + Q4.3_generationwhy + Q4.3_missing + Q4.3_morbid +
  Q4.3_myfavmurder + Q4.3_obsessed + Q4.3_truecrimeob + Q4.3_vanished + Q4.5 +
  Q4.6 + Q4.7 + Q4.8 + avg_Q5.1 + Q6.1 + avg_Q7.1 + avg_Q7.2 + avg_Q8.1 + avg_Q8.2 +
  Q8.3 + avg_Q9.1 + Q9.2 + avg_Q9.4 + Q9.5 + avg_Q10.1 + prop11.3 + threat11.3 +
  prop11.4 + threat11.4 + Q11.5 + Q11.8 + Q11.10 + Q11.11 + victim_life + Q67 +
  Q13.1 + Q13.2 + Q13.3 + Q13.4 + Q13.5

#lmod_safety = lm(f_safety, data = crime_data, na.action = na.omit)
lmod_safety = lm(f_safety, data = crime_data, na.action = na.exclude)
summary(lmod_safety)

# Will look for outliers # ====================================================

h1 <- hatvalues(lmod_safety)
indexes <- row.names(crime_data)
halfnorm(h1, nlab = 4, labs = indexes, ylab = "leverage")

infIndexPlot(lmod_safety, vars = "hat")

outlierTest(lmod_safety)

d1 = cooks.distance(lmod_safety)
halfnorm(d1, nlab = 6, labs = indexes)
infIndexPlot(lmod_safety, vars = "Cook", id = list(n = 6))

influencePlot(lmod_safety)

# Point 542 was shown to be an outlier via Cook's distance and the Bonferroni value,
# so this point was taken out, and both models were compared

lmod_safety_o = lm(f_safety,
                   data = crime_data,
                   subset = (indexes != "542"))
compareCoefs(lmod_safety, lmod_safety_o)

# It appears that this point does hold some strong influence as the coefficients
# changed, so this was left out when running the model

h1 <- hatvalues(lmod_safety_o)
halfnorm(h1, nlab = 4, labs = indexes, ylab = "leverage")

infIndexPlot(lmod_safety_o, vars = "hat")

outlierTest(lmod_safety_o)

d1 = cooks.distance(lmod_safety_o)
halfnorm(d1, nlab = 6, labs = indexes)
infIndexPlot(lmod_safety_o, vars = "Cook", id = list(n = 6))

influencePlot(lmod_safety_o)

# Point 153 was shown to be an outlier via Cook's distance and the Bonferroni value,
# so this point was taken out, and both models were compared

lmod_safety_ou = lm(f_safety,
                   data = crime_data,
                   subset = (indexes != c("542","153")))
compareCoefs(lmod_safety_o, lmod_safety_ou)

# The differences between these coefficients are pretty minimal, so this point
# will not be taken out, and the VIF can now be looked at

# Run to eliminate collinearity
vif(lmod_safety_o)

# Q4.7 has a VIF of 809.18 and is much higher than other values, so this will be removed
lmod_safety_ou <- update(lmod_safety_o, .~. - Q4.7, data = crime_data)
vif(lmod_safety_ou)

# Because Q11.10 and prop11.3 have about the same VIF (524.76 and 520.26 respectively),
# prop11.3 was removed as this was seen as less of a distinct variable that
# would be interesting (victim of property damage/theft in life)

lmod_safety_ou <- update(lmod_safety_ou, .~. - prop11.3, data = crime_data)
vif(lmod_safety_ou)

# Q11.10 is still very high (VIF = 498.14), so this will still be removed

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q11.10, data = crime_data)
vif(lmod_safety_ou)

# Q2.1 is very high (VIF = 297.03), so this will be removed

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q2.1, data = crime_data)
vif(lmod_safety_ou)

# Next is Q4.1_2 (125.00)

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q4.1_2, data = crime_data)
vif(lmod_safety_ou)

# Next is Q4.1_3 (72.76)

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q4.1_3, data = crime_data)
vif(lmod_safety_ou)

# Because Q11.8 and threat11.3 have about the same VIF (84.11 and 80.29 respectively),
# threat11.3 was removed as this was seen as less of a distinct variable that
# would be interesting (victim of abuse/threat in life)

lmod_safety_ou <- update(lmod_safety_ou, .~. - threat11.3, data = crime_data)
vif(lmod_safety_ou)

# Next is Q13.3 (63.65)

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q13.3, data = crime_data)
vif(lmod_safety_ou)

# Next is Q11.8 (40.90)

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q11.8, data = crime_data)
vif(lmod_safety_ou)

# Because both Q4.5 and Q13.5 are high (37.00 and 36.13 respectively) and are
# equally interesting, either will be pulled, and the results will be compared
# as far as next highest VIF

lmod_safety_ou <- update(lmod_safety_ou, .~. + Q4.5 - Q13.5, data = crime_data)
vif(lmod_safety_ou)

# Both will be removed

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q4.5, data = crime_data)
vif(lmod_safety_ou)

# Next is Q13.2 (20.98)

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q13.2, data = crime_data)
vif(lmod_safety_ou)

# Next is prop11.4 (18.67)

lmod_safety_ou <- update(lmod_safety_ou, .~. - prop11.4, data = crime_data)
vif(lmod_safety_ou)

# Next is Q2.8 (11.85)

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q2.8, data = crime_data)
vif(lmod_safety_ou)

# Next is Q4.6 (10.24)

lmod_safety_ou <- update(lmod_safety_ou, .~. - Q4.6, data = crime_data)
vif(lmod_safety_ou)

# Now that all GVIFs are less than 10, will use step function to finalize model

# Need to reiterate the model since the step function will recreate models
# assuming the NAs are back (I believe)
lmod_safety2<-lm(total_behaviors~., data=lmod_safety_ou$model)

lmod_safety_f <- step(lmod_safety2, direction = "both")

# Run again to assess collinearity
vif(lmod_safety_f)

# All GVIF are less than 5, so this is done
summary(lmod_safety_f)

# Trying with exhaustive search, but ended up taking too long
# b_safety <- regsubsets(f_safety, data = crime_data, really.big = TRUE)

# Now will attempt to add applicable interactive terms

lmod_safety_fi <- update(lmod_safety_f, .~. +
                          avg_Q7.1*Q4.8 + avg_Q7.1*avg_Q10.1 + avg_Q7.1*Q11.11 +
                          avg_Q7.1*victim_life + avg_Q7.1*Q67 + Q11.11*Q4.8 +
                          Q11.11*avg_Q10.1 + Q11.11*victim_life + Q11.11*Q67 +
                          victim_life*Q4.8 + victim_life*avg_Q10.1 +
                          victim_life*Q67, data = crime_data)

# Checking for significance with step again
lmod_safety_fiu <- step(lmod_safety_fi, direction = "both")

summary(lmod_safety_fiu)
nobs(lmod_safety_fiu)

# Useful Info:

se_safety <- sqrt(diag(vcov(lmod_safety_fiu)))
# table of estimates with 95% CI
(tab_safety <- cbind(Coeff = coefficients(lmod_safety_fiu), 
                    LL = coefficients(lmod_safety_fiu) - 1.96 * se_safety, 
                    UL = coefficients(lmod_safety_fiu) + 1.96 * se_safety,
                    P = summary(lmod_safety_fiu)$coefficients[,4]))

write.csv(tab_safety, "RegCoeffs_totalBehav.csv")

# fitted values vs regressors
residualPlot(lmod_safety_fiu, quadratic = FALSE)

residualPlots(lmod_safety_fiu, fitted = FALSE, tests = FALSE, quadratic = FALSE)

# plot sqrt absolute residuals vs fitted values
plot(lmod_safety_fiu, which = 3)

# assess normality of errors using shapiro-wilk test for data
shapiro.test(residuals(lmod_safety_fiu))
#Reject null hypothesis (moderate evidence for alternative hypothesis)

qqPlot(residuals(lmod_safety_fiu))

# better q-q plot of studentized residuals with confidence bands
qqPlot(lmod_safety_fiu)

# q-q plot of savings residuals
plot(lmod_safety_fiu, which = 2)

# effects plot of one-way model

# Worry About Personal Safety vs Total Safety Behaviors Taken
plot(Effect("Q4.8", lmod_safety_fiu), main = "Worry about Personal Safety Effect Plot",
     xlab = "Worry", ylab = "Total Safety Behaviors")

# Is Total Media influencing worry and just crime podcasts influencing worry

# Victim of Assault in Lifetime vs Total Safety Behaviors Taken
plot(Effect("victim_life", lmod_safety_fiu), main = "Victim of Assault Effect Plot",
     xlab = "No <- Victim -> Yes", ylab = "Total Safety Behaviors")

# Living in the U.S. vs Total Safety Behaviors Taken
plot(Effect("Q67", lmod_safety_fiu), main = "US Resident Effect Plot",
     xlab = "US Resident", ylab = "Total Safety Behaviors")
