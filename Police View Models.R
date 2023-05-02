library(faraway)
library(effects)
library(MASS)
library(car)
library(leaps)
library(tidyverse)
library(alr4)
library(dplyr)

# Read in data
crime_data <- readRDS("/Users/edgil/Documents/Math Master/Math 6330 Statistical Consulting/cleaned_crime_data.rds")
#crime_data0 <- readRDS("/Users/edgil/Documents/Math Master/Math 6330 Statistical Consulting/cleaned_crime_data0.rds")
View(crime_data)

# Views of Police (Q8.1 Sums)
f_police = avg_Q8.1 ~ Q2.1 + Q2.4 + Q2.5 + Q2.6 + Q2.7 + Q2.8 + Q3.1 +
  Q4.1_1 + Q4.1_2 + Q4.1_3 + Q4.1_4 + Q4.3_casefile + Q4.3_crimejunkie +
  Q4.3_criminal + Q4.3_dateline + Q4.3_generationwhy + Q4.3_missing + Q4.3_morbid +
  Q4.3_myfavmurder + Q4.3_obsessed + Q4.3_truecrimeob + Q4.3_vanished + Q4.5 +
  Q4.6 + Q4.7 + Q4.8 + Q4.9_1 + Q4.9_2 + Q4.9_3 + Q4.9_4 + Q4.9_5 + Q4.9_6 +
  Q4.9_7 + Q4.9_8 + Q4.10_1 + Q4.10_2 + Q4.10_3 + Q4.10_4 + Q4.10_5 + Q4.10_6 +
  Q4.10_7 + Q4.10_8 + avg_Q5.1 + Q6.1 + avg_Q7.1 + avg_Q7.2 +
  Q8.3 + avg_Q9.1 + Q9.2 + avg_Q9.4 + Q9.5 + avg_Q10.1 + prop11.3 + threat11.3 +
  prop11.4 + threat11.4 + Q11.5 + Q11.8 + Q11.10 + Q11.11 + victim_life + Q67 + Q13.1 +
  Q13.2 + Q13.3 + Q13.4 + Q13.5

lmod_police = lm(f_police, data = crime_data, na.action = na.exclude)
summary(lmod_police)

# Will look for outliers # ====================================================

h3 <- hatvalues(lmod_police)
indexes <- row.names(crime_data)
halfnorm(h3, nlab = 4, labs = indexes, ylab = "leverage")

infIndexPlot(lmod_police, vars = "hat")

outlierTest(lmod_police)

d3 = cooks.distance(lmod_police)
halfnorm(d3, nlab = 6, labs = indexes)
infIndexPlot(lmod_police, vars = "Cook", id = list(n = 6))

influencePlot(lmod_police)

# Point 546 was shown to be an outlier via Cook's distance and the Bonferroni value,
# so this point was taken out, and both models were compared

lmod_police_o = lm(f_police,
                   data = crime_data,
                   subset = (indexes != "546"))
compareCoefs(lmod_police, lmod_police_o)

# It appears that this point does hold some strong influence as the coefficients
# changed, so this was left out when running the model

h3 <- hatvalues(lmod_police_o)
halfnorm(h3, nlab = 4, labs = indexes, ylab = "leverage")

infIndexPlot(lmod_police_o, vars = "hat")

outlierTest(lmod_police_o)

d3 = cooks.distance(lmod_police_o)
halfnorm(d3, nlab = 6, labs = indexes)
infIndexPlot(lmod_police_o, vars = "Cook", id = list(n = 6))

influencePlot(lmod_police_o)

# Nothing clearly sticking out as an influential point here, so no more terms removed

# Run to eliminate collinearity
vif(lmod_police_o)

# Q2.1 has a VIF of 1,510.66 and is higher than other values, so this will be removed
lmod_police_ou <- update(lmod_police_o, .~. - Q2.1, data = crime_data)
vif(lmod_police_ou)

# Q4.7 is next with GVIF = 1034.68

lmod_police_ou <- update(lmod_police_ou, .~. - Q4.7, data = crime_data)
vif(lmod_police_ou)

# Q11.10 is high (VIF = 725.46), so this will be removed

lmod_police_ou <- update(lmod_police_ou, .~. - Q11.10, data = crime_data)
vif(lmod_police_ou)

# prop11.3 is very high (VIF = 637.92), so this will be removed

lmod_police_ou <- update(lmod_police_ou, .~. - prop11.3, data = crime_data)
vif(lmod_police_ou)

# Next is Q13.3 (333.61)

lmod_police_ou <- update(lmod_police_ou, .~. - Q13.3, data = crime_data)
vif(lmod_police_ou)

# Next is Q13.2 (163.46)

lmod_police_ou <- update(lmod_police_ou, .~. - Q13.2, data = crime_data)
vif(lmod_police_ou)

# Next is Q11.8 (122.44) or Q4.1_3 (122.17). Both will be removed to test

lmod_police_ou <- update(lmod_police_ou, .~. - Q11.8 + Q4.1_3, data = crime_data)
vif(lmod_police_ou)

# Removing Q11.8 ended up with lower overall GVIFs, so this was chosen

# Next is Q4.1_3 (115.08)

lmod_police_ou <- update(lmod_police_ou, .~. - Q4.1_3, data = crime_data)
vif(lmod_police_ou)

# Next is Q4.1_2 (69.76)

lmod_police_ou <- update(lmod_police_ou, .~. - Q4.1_2, data = crime_data)
vif(lmod_police_ou)

# Next is threat11.3 (49.15)

lmod_police_ou <- update(lmod_police_ou, .~. - threat11.3, data = crime_data)
vif(lmod_police_ou)

# Next is Q4.5 (47.84)

lmod_police_ou <- update(lmod_police_ou, .~. - Q4.5, data = crime_data)
vif(lmod_police_ou)

# Next is Q13.4 (34.77)

lmod_police_ou <- update(lmod_police_ou, .~. - Q13.4, data = crime_data)
vif(lmod_police_ou)

# Next is prop11.4 (20.12)

lmod_police_ou <- update(lmod_police_ou, .~. - prop11.4, data = crime_data)
vif(lmod_police_ou)

# Next is Q2.8 (17.87)

lmod_police_ou <- update(lmod_police_ou, .~. - Q2.8, data = crime_data)
vif(lmod_police_ou)

# Because Q4.8 and Q4.6 are high (15.95 and 15.55 respectively),
# Q4.6 was removed as this was seen as less of an interesting variable
# (More aware of surroundings since crime podcasts)

lmod_police_ou <- update(lmod_police_ou, .~. - Q4.6, data = crime_data)
vif(lmod_police_ou)

# Now that all GVIFs are less than 10, will use step function to finalize model

# Need to reiterate the model since the step function will recreate models
# assuming the NAs are back (I believe)
lmod_police2 <- lm(avg_Q8.1~., data=lmod_police_ou$model)

lmod_police_f <- step(lmod_police2, direction = "both")

# Run again to assess collinearity
vif(lmod_police_f)

# All GVIF are less than 5, so this is done
summary(lmod_police_f)

# Now will attempt to add applicable interactive terms

lmod_police_fi <- update(lmod_police_f, .~. +
                           Q4.9_8*Q4.10_1 + Q4.9_8*avg_Q9.1 + Q4.9_8*avg_Q9.4 +
                           Q4.9_8*avg_Q10.1 + Q4.9_8*Q13.5 + Q4.10_1*avg_Q9.1 +
                           Q4.10_1*avg_Q9.4 + Q4.10_1*avg_Q10.1 + Q4.10_1*Q13.5 +
                           Q13.5*avg_Q9.1 + Q13.5*avg_Q9.4 + Q13.5*avg_Q10.1,
                         data = crime_data)

summary(lmod_police_fi)

# Need to remove some terms due to NAs

lmod_police_fi <- update(lmod_police_fi, .~. - Q4.9_8*Q4.10_1 - Q4.9_8*Q13.5 -
                           avg_Q10.1*Q13.5, data = crime_data)

summary(lmod_police_fi)

# Checking for significance with step again
lmod_police_fiu <- step(lmod_police_fi, direction = "both")
# Fear of Crime dependent: Victimization, media consumption (1,2,4)

summary(lmod_police_fiu)
nobs(lmod_police_fiu) #229

# Useful Info:

se_police <- sqrt(diag(vcov(lmod_police_fiu)))
# table of estimates with 95% CI
(tab_police <- cbind(Coeff = coefficients(lmod_police_fiu), 
                     LL = coefficients(lmod_police_fiu) - 1.96 * se_police, 
                     UL = coefficients(lmod_police_fiu) + 1.96 * se_police,
                     P = summary(lmod_police_fiu)$coefficients[,4]))

write.csv(tab_police, "RegCoeffs_police.csv")

# fitted values vs regressors
residualPlot(lmod_police_fiu, quadratic = FALSE)

residualPlots(lmod_police_fiu, fitted = FALSE, tests = FALSE, quadratic = FALSE)

# plot sqrt absolute residuals vs fitted values
plot(lmod_police_fiu, which = 3)

# assess normality of errors using shapiro-wilk test for data
shapiro.test(residuals(lmod_police_fiu))
#Reject null hypothesis (moderate evidence for alternative hypothesis)

qqPlot(residuals(lmod_police_fiu))

# better q-q plot of studentized residuals with confidence bands
qqPlot(lmod_police_fiu)

# q-q plot of savings residuals
plot(lmod_police_fiu, which = 2)

# effects plot of one-way model

# View of Prosecutors vs View of Police
plot(Effect("avg_Q9.4", lmod_police_fiu), main = "Prosecutor View Effect Plot",
     xlab = "View of Prosecutors", ylab = "View of Police")

# Living in US vs View of Police
plot(Effect("Q67", lmod_police_fiu), main = "Live in U.S. Effect Plot",
     xlab = "Live in US", ylab = "View of Police")

# View of Crime/Punishment*Carrying a Handgun vs View of Police
plot(Effect(c("Q4.9_8","avg_Q10.1"), lmod_police_fiu), main = "Crime View-Carrying Handgun Interaction Effect Plot",
     xlab = "Crime View-Handgun", ylab = "View of Police")

# View of Prosecutors*Political View vs View of Police
plot(Effect(c("avg_Q9.4", "Q13.5"), lmod_police_fiu), main = "Prosecutor*Political Views Effect Plot",
     xlab = "Prosecutor Views", ylab = "View of Police")
