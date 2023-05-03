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

# Views of Police (Q8.2 Sums)
f_police2 = avg_Q8.2 ~ Q2.1 + Q2.4 + Q2.5 + Q2.6 + Q2.7 + Q2.8 + Q3.1 +
  Q4.1_1 + Q4.1_2 + Q4.1_3 + Q4.1_4 + Q4.3_casefile + Q4.3_crimejunkie +
  Q4.3_criminal + Q4.3_dateline + Q4.3_generationwhy + Q4.3_missing + Q4.3_morbid +
  Q4.3_myfavmurder + Q4.3_obsessed + Q4.3_truecrimeob + Q4.3_vanished + Q4.5 +
  Q4.6 + Q4.7 + Q4.8 + Q4.9_1 + Q4.9_2 + Q4.9_3 + Q4.9_4 + Q4.9_5 + Q4.9_6 +
  Q4.9_7 + Q4.9_8 + Q4.10_1 + Q4.10_2 + Q4.10_3 + Q4.10_4 + Q4.10_5 + Q4.10_6 +
  Q4.10_7 + Q4.10_8 + avg_Q5.1 + Q6.1 + avg_Q7.1 + avg_Q7.2  +
  Q8.3 + avg_Q9.1 + Q9.2 + avg_Q9.4 + Q9.5 + avg_Q10.1 + prop11.3 + threat11.3 +
  prop11.4 + threat11.4 + Q11.5 + Q11.8 + Q11.10 + Q11.11 + victim_life + Q67 + Q13.1 +
  Q13.2 + Q13.3 + Q13.4 + Q13.5

lmod_police2 = lm(f_police2, data = crime_data, na.action = na.exclude)
summary(lmod_police2)

# Will look for outliers # ====================================================

h4 <- hatvalues(lmod_police2)
indexes <- row.names(crime_data)
halfnorm(h4, nlab = 4, labs = indexes, ylab = "leverage")

infIndexPlot(lmod_police2, vars = "hat")

outlierTest(lmod_police2)

d4 = cooks.distance(lmod_police2)
halfnorm(d4, nlab = 6, labs = indexes)
infIndexPlot(lmod_police2, vars = "Cook", id = list(n = 6))

influencePlot(lmod_police2)

# Point 270 was shown to be an outlier via Cook's distance and the Bonferroni value,
# so this point was taken out, and both models were compared

lmod_police2_o = lm(f_police2,
                   data = crime_data,
                   subset = (indexes != "270"))
compareCoefs(lmod_police2, lmod_police2_o)

# It appears that this point does hold some strong influence as the coefficients
# changed, so this was left out when running the model

h4 <- hatvalues(lmod_police2_o)
halfnorm(h4, nlab = 4, labs = indexes, ylab = "leverage")

infIndexPlot(lmod_police2_o, vars = "hat")

outlierTest(lmod_police2_o)

d4 = cooks.distance(lmod_police2_o)
halfnorm(d4, nlab = 6, labs = indexes)
infIndexPlot(lmod_police2_o, vars = "Cook", id = list(n = 6))

influencePlot(lmod_police_o)

# Nothing clearly sticking out as an influential point here, so no more terms removed

# Run to eliminate collinearity
vif(lmod_police2_o)

# Q4.7 has a VIF of 1,378.81 and is higher than other values, so this will be removed
lmod_police2_ou <- update(lmod_police2_o, .~. - Q4.7, data = crime_data)
vif(lmod_police2_ou)

# Q2.1 is next with GVIF = 863.45

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q2.1, data = crime_data)
vif(lmod_police2_ou)

# Q11.10 is high (VIF = 725.40), so this will be removed

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q11.10, data = crime_data)
vif(lmod_police2_ou)

# prop11.3 is very high (VIF = 641.16), so this will be removed

lmod_police2_ou <- update(lmod_police2_ou, .~. - prop11.3, data = crime_data)
vif(lmod_police2_ou)

# Next is Q13.3 (335.86)

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q13.3, data = crime_data)
vif(lmod_police2_ou)

# Next is Q13.2 (162.24)

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q13.2, data = crime_data)
vif(lmod_police2_ou)

# Next is Q11.8 (123.91)

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q11.8, data = crime_data)
vif(lmod_police2_ou)

# Next is Q4.1_3 (103.18)

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q4.1_3, data = crime_data)
vif(lmod_police2_ou)

# Next is Q4.1_2 (69.25)

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q4.1_2, data = crime_data)
vif(lmod_police2_ou)

# Next is threat11.3 (49.81)

lmod_police2_ou <- update(lmod_police2_ou, .~. - threat11.3, data = crime_data)
vif(lmod_police2_ou)

# Next is Q4.5 (46.58)

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q4.5, data = crime_data)
vif(lmod_police2_ou)

# Next is Q13.4 (35.86)

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q13.4, data = crime_data)
vif(lmod_police2_ou)

# Next is prop11.4 (19.77)

lmod_police2_ou <- update(lmod_police2_ou, .~. - prop11.4, data = crime_data)
vif(lmod_police2_ou)

# Next is Q2.8 (18.60)

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q2.8, data = crime_data)
vif(lmod_police2_ou)

# Because Q4.8 and Q4.6 are high (16.20 and 15.55 respectively),
# Q4.6 was removed as this was seen as less of an interesting variable
# (More aware of surroundings since crime podcasts)

lmod_police2_ou <- update(lmod_police2_ou, .~. - Q4.6, data = crime_data)
vif(lmod_police2_ou)

# Now that all GVIFs are less than 10, will use step function to finalize model

# Need to reiterate the model since the step function will recreate models
# assuming the NAs are back (I believe)
lmod_police2_2 <- lm(avg_Q8.2~., data=lmod_police2_ou$model)

lmod_police2_f <- step(lmod_police2_2, direction = "both")

# Run again to assess collinearity
vif(lmod_police2_f)

# All GVIF are less than 5, so this is done
summary(lmod_police2_f)

# Now will attempt to add applicable interactive terms

lmod_police2_fi <- update(lmod_police2_f, .~. +
                           Q2.4*Q4.9_6 + Q2.4*Q4.9_7 + Q2.4*Q4.9_8 + Q2.4*Q4.10_7 +
                            Q2.4*Q67 + Q2.4*Q13.5 + Q4.9_8*Q4.9_6 + Q4.9_8*Q4.9_7 +
                            Q4.9_8*Q4.10_7 + Q4.9_8*Q67 + Q4.9_8*Q13.5 +
                            Q4.10_7*Q4.9_6 + Q4.10_7*Q4.9_7 + Q4.10_7*Q67 +
                            Q4.10_7*Q13.5 + Q67*Q4.9_6 + Q67*Q4.9_7 + Q67*Q13.5 +
                            Q13.5*Q4.9_6 + Q13.5*Q4.9_7, data = crime_data)

summary(lmod_police2_fi)

# Need to remove some terms due to NAs

lmod_police2_fi <- update(lmod_police2_fi, .~. - Q2.4*Q4.9_6 - Q2.4*Q4.9_7 -
                            Q2.4*Q4.9_8 - Q2.4*Q4.10_7 - Q2.4*Q67 - Q2.4*Q13.5 -
                            Q4.9_8*Q4.9_6 - Q4.9_8*Q67 - Q4.9_8*Q13.5 - Q4.10_7*Q4.9_6 -
                            Q4.10_7*Q4.9_7 - Q4.10_7*Q67 - Q4.10_7*Q13.5 -
                            Q67*Q4.9_7 - Q13.5*Q4.9_6 - Q13.5*Q4.9_7, data = crime_data)

summary(lmod_police2_fi)

# Checking for significance with step again
lmod_police2_fiu <- step(lmod_police2_fi, direction = "both")
# Fear of Crime dependent: Victimization, media consumption (1,2,4)

summary(lmod_police2_fiu)
nobs(lmod_police2_fiu) #236

# Useful Info:

se_police2 <- sqrt(diag(vcov(lmod_police2_fiu)))
# table of estimates with 95% CI
(tab_police2 <- cbind(Coeff = coefficients(lmod_police2_fiu), 
                     LL = coefficients(lmod_police2_fiu) - 1.96 * se_police2, 
                     UL = coefficients(lmod_police2_fiu) + 1.96 * se_police2,
                     P = summary(lmod_police2_fiu)$coefficients[,4]))

write.csv(tab_police2, "RegCoeffs_police2.csv")

# fitted values vs regressors
residualPlot(lmod_police2_fiu, quadratic = FALSE)

residualPlots(lmod_police2_fiu, fitted = FALSE, tests = FALSE, quadratic = FALSE)

# plot sqrt absolute residuals vs fitted values
plot(lmod_police2_fiu, which = 3)

# assess normality of errors using shapiro-wilk test for data
shapiro.test(residuals(lmod_police2_fiu))
#Reject null hypothesis (moderate evidence for alternative hypothesis)

qqPlot(residuals(lmod_police2_fiu))

# better q-q plot of studentized residuals with confidence bands
qqPlot(lmod_police2_fiu)

# q-q plot of savings residuals
  plot(lmod_police2_fiu, which = 2)

# effects plot of one-way model

# Consumption of News vs View of Local Police
plot(Effect("Q4.1_1", lmod_police2_fiu), main = "Consumption of News Effect Plot",
     xlab = "Time Watching the News", ylab = "View of Local Police")

# View of Prosecutors vs Local View of Police
plot(Effect("avg_Q9.4", lmod_police2_fiu), main = "View of Prosecutors Effect Plot",
     xlab = "View of Prosecutors", ylab = "View of Local Police")

# US Resident*Social Political Stance vs View of Local Police
plot(Effect(c("Q67","Q13.5"), lmod_police2_fiu), main = "US Resident-Political Interaction Effect Plot",
     xlab = "Lives in US, Social Political Views", ylab = "View of Local Police")


# Forest Plot
plot_model(lmod_police2_fiu, title = "Local Police View Forest Plot")
