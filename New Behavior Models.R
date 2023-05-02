library(faraway)
library(effects)
library(MASS)
library(car)
library(leaps)
library(tidyverse)
library(alr4)
library(dplyr)
library(sjPlot)
library(ggplot2)

# Read in data
crime_data <- readRDS("/Users/edgil/Documents/Math Master/Math 6330 Statistical Consulting/cleaned_crime_data.rds")
#crime_data0 <- readRDS("/Users/edgil/Documents/Math Master/Math 6330 Statistical Consulting/cleaned_crime_data0.rds")
View(crime_data)

summary(crime_data$Q9.5)

plot(crime_data$new_behaviors)

# Changed Behaviors after Podcasts (Q4.10 Conditional on Q4.9)
f_safety2 = new_behaviors ~ Q2.1 + Q2.4 + Q2.5 + Q2.6 + Q2.7 + Q2.8 + Q3.1 +
  Q4.1_1 + Q4.1_2 + Q4.1_3 + Q4.1_4 + Q4.3_casefile + Q4.3_crimejunkie +
  Q4.3_criminal + Q4.3_dateline + Q4.3_generationwhy + Q4.3_missing + Q4.3_morbid +
  Q4.3_myfavmurder + Q4.3_obsessed + Q4.3_truecrimeob + Q4.3_vanished + Q4.5 +
  Q4.6 + Q4.7 + Q4.8 + avg_Q5.1 + Q6.1 + avg_Q7.1 + avg_Q7.2 + avg_Q8.1 + avg_Q8.2 +
  Q8.3 + avg_Q9.1 + Q9.2 + avg_Q9.4 + Q9.5 + avg_Q10.1 + prop11.3 + threat11.3 +
  prop11.4 + threat11.4 + Q11.5 + Q11.8 + Q11.10 + Q11.11 + victim_life + Q67 +
  Q13.1 + Q13.2 + Q13.3 + Q13.4 + Q13.5

lmod_change = lm(f_safety2, data = crime_data, na.action = na.exclude)
summary(lmod_change)

# Will look for outliers # ====================================================

h2 <- hatvalues(lmod_change)
indexes <- row.names(crime_data)
halfnorm(h2, nlab = 4, labs = indexes, ylab = "leverage")

infIndexPlot(lmod_change, vars = "hat")

outlierTest(lmod_change)

d2 = cooks.distance(lmod_change)
halfnorm(d2, nlab = 6, labs = indexes)
infIndexPlot(lmod_change, vars = "Cook", id = list(n = 6))

influencePlot(lmod_change)

# Point 243 was shown to be an outlier via Cook's distance and the Bonferroni value,
# so this point was taken out, and both models were compared

lmod_change_o = lm(f_safety2,
                   data = crime_data,
                   subset = (indexes != "243"))
compareCoefs(lmod_change, lmod_safety_o)

# It appears that this point does hold some strong influence as the coefficients
# changed, so this was left out when running the model

h2 <- hatvalues(lmod_change_o)
halfnorm(h2, nlab = 4, labs = indexes, ylab = "leverage")

infIndexPlot(lmod_change_o, vars = "hat")

outlierTest(lmod_change_o)

d2 = cooks.distance(lmod_change_o)
halfnorm(d2, nlab = 6, labs = indexes)
infIndexPlot(lmod_change_o, vars = "Cook", id = list(n = 6))

influencePlot(lmod_safety_o)

# Nothing clearly sticking out as an influential point here, so no more terms removed

# Run to eliminate collinearity
vif(lmod_change_o)

# Q4.7 has a VIF of 767.51 and is much higher than other values, so this will be removed
lmod_change_ou <- update(lmod_change_o, .~. - Q4.7, data = crime_data)
vif(lmod_change_ou)

# prop11.3 is next with GVIF = 510.30

lmod_change_ou <- update(lmod_change_ou, .~. - prop11.3, data = crime_data)
vif(lmod_change_ou)

# Q11.10 is high (VIF = 508.34), so this will be removed

lmod_change_ou <- update(lmod_change_ou, .~. - Q11.10, data = crime_data)
vif(lmod_change_ou)

# Q2.1 is very high (VIF = 314.22), so this will be removed

lmod_change_ou <- update(lmod_change_ou, .~. - Q2.1, data = crime_data)
vif(lmod_change_ou)

# Next is Q4.1_2 (129.46)

lmod_change_ou <- update(lmod_change_ou, .~. - Q4.1_2, data = crime_data)
vif(lmod_change_ou)

# Next is Q11.8 (90.63)

lmod_change_ou <- update(lmod_change_ou, .~. - Q11.8, data = crime_data)
vif(lmod_change_ou)

# Next is Q4.1_3

lmod_change_ou <- update(lmod_change_ou, .~. - Q4.1_3, data = crime_data)
vif(lmod_change_ou)

# Next is Q13.3 (60.94)

lmod_change_ou <- update(lmod_change_ou, .~. - Q13.3, data = crime_data)
vif(lmod_change_ou)

# Because threat11.3, Q4.5, and Q13.5 are high (38.42, 38.14 and 38.09 respectively)
# and are equally interesting, either will be pulled, and the results will be compared
# as far as next highest VIF

lmod_change_ou <- update(lmod_change_ou, .~. + Q13.5 - threat11.3, data = crime_data)
vif(lmod_change_ou)

# threat11.3 will be removed as this is correlated with the other two
# Because both Q4.5 and Q13.5 are high (37.09 and 36.04 respectively) and are
# equally interesting, either will be pulled, and the results will be compared
# as far as next highest VIF

lmod_change_ou <- update(lmod_change_ou, .~. + Q4.5 - Q13.5, data = crime_data)
vif(lmod_change_ou)

# Both will be removed

lmod_change_ou <- update(lmod_change_ou, .~. - Q4.5, data = crime_data)
vif(lmod_change_ou)

# Because prop11.4 and Q13.2 have about the same VIF (19.58 and 19.48 respectively),
#  and are equally interesting, either will be pulled, and the results will be compared
# as far as next highest VIF

lmod_change_ou <- update(lmod_change_ou, .~. - Q13.2 - prop11.4, data = crime_data)
vif(lmod_change_ou)

# Both are removed
# Next is Q2.8 (11.96)

lmod_change_ou <- update(lmod_change_ou, .~. - Q2.8, data = crime_data)
vif(lmod_change_ou)

# Next is Q4.6 (10.23)

lmod_change_ou <- update(lmod_change_ou, .~. - Q4.6, data = crime_data)
vif(lmod_change_ou)

# Now that all GVIFs are less than 10, will use step function to finalize model

# Need to reiterate the model since the step function will recreate models
# assuming the NAs are back (I believe)
lmod_change2<-lm(new_behaviors~., data=lmod_change_ou$model)

lmod_change_f <- step(lmod_change2, direction = "both")

# Run again to assess collinearity
vif(lmod_change_f)

# All GVIF are less than 5, so this is done
summary(lmod_change_f)

# Now will attempt to add applicable interactive terms

lmod_change_fi <- update(lmod_change_f, .~. +
                           Q4.1_4*Q4.8 + Q4.1_4*avg_Q5.1 + Q4.1_4*Q8.3 + Q4.1_4*Q13.1 +
                           Q4.1_4*Q13.4 + avg_Q5.1*Q4.8 + avg_Q5.1*Q8.3 +
                           avg_Q5.1*Q13.1 + avg_Q5.1*Q13.4 + Q8.3*Q4.8 + Q8.3*Q13.1 +
                           Q8.3*Q13.4, data = crime_data)

# Checking for significance with step again
lmod_change_fiu <- step(lmod_change_fi, direction = "both")

summary(lmod_change_fiu)
nobs(lmod_change_fiu)

# Useful Info:

se_change <- sqrt(diag(vcov(lmod_change_fiu)))
# table of estimates with 95% CI
(tab_change <- cbind(Coeff = coefficients(lmod_change_fiu), 
                     LL = coefficients(lmod_change_fiu) - 1.96 * se_change, 
                     UL = coefficients(lmod_change_fiu) + 1.96 * se_change,
                     P = summary(lmod_change_fiu)$coefficients[,4]))

write.csv(tab_change, "RegCoeffs_newBehav.csv")

# fitted values vs regressors
residualPlot(lmod_change_fiu, quadratic = FALSE)

residualPlots(lmod_change_fiu, fitted = FALSE, tests = FALSE, quadratic = FALSE)

# plot sqrt absolute residuals vs fitted values
plot(lmod_change_fiu, which = 3)

# assess normality of errors using shapiro-wilk test for data
shapiro.test(residuals(lmod_change_fiu))
#Reject null hypothesis (moderate evidence for alternative hypothesis)

qqPlot(residuals(lmod_change_fiu))

# better q-q plot of studentized residuals with confidence bands
qqPlot(lmod_change_fiu)

# q-q plot of savings residuals
plot(lmod_change_fiu, which = 2)

# effects plot of one-way model

# Attempts of trying to single out one term from categorical variables
# plot_model(lmod_change_fiu, type = "pred", terms = "Q2.6 [Single, never married]")
# 


# plot(Effect("Q2.6", lmod_change_fiu, terms = "Single, never married"),
#      main = "Single, Never Married Effect Plot", xlab = "Single, Never Married",
#      ylab = "New Safety Behaviors")

# plot marginal effects
#plot_model(lmod_change_fiu, type = "eff", terms = "Q2.6 [3]")

# Time on Social Media vs New Safety Behaviors Taken
plot(Effect("Q3.1", lmod_change_fiu), main = "Social Media Effect Plot",
     xlab = "Time on Social Media", ylab = "New Safety Behaviors")

# Economic Political View vs New Safety Behaviors Taken
plot(Effect("Q13.4", lmod_change_fiu), main = "Economic Political Effect Plot",
     xlab = "Stance on Economic Politics", ylab = "New Safety Behaviors")
