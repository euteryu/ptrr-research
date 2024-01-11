# RM3_2023-2024_Week9_LMM.R
# Created on: sometime in 2021; revised in October 2023
# Author: Amy Packer, revised by Andrew Bell
# Version 1.1 - fixed construction of LMM (Nov 27, 2023)
# Purpose: Script accompanying lectures for RM3 - Week 9 (2023-2024)
# This script is a solution to the 2023-2024 Worksheet Problem, first designed by Mark Crook-Rumsey in 2021.

# Notes: In addition to lecture materials, these links were helpful: 
# https://lme4.r-forge.r-project.org/slides/2009-07-07-Rennes/3LongitudinalD.pdf
# https://attilagk.github.io/monoallelic-brain-notebook/2017-03-01-learning-lme4-package.html
# https://glennwilliams.me/r4psych/mixed-effects-models.html

# Workshop Activities/Objectives
# For this session:
# 1.	Plot the data to help you decide which model will best explain the effect of sleep deprivation on reaction time.
# 2.	Create a fixed-effect model to explain the sleep data (this will be refresher of what you have already done in other workshops).
# 3.	Create a random-effects model of sleep data.
# 4.	Create a mixed effects mode of the sleep data to explain reaction time due to sleep deprivation. 
# 5.	Explain yours results. 

###########################################################################

## CLEAR ALL PLOTS
#dev.off(dev.list()["RStudioGD"])

###########################################################################

# Clean workspace 
rm(list=ls())

install.packages("Matrix")
library(Matrix)

# load standard libraries
packages = c("ggplot2", "dplyr", "ggpubr", "psych", "tidyverse", "car", "Matrix", "lme4", "MuMIn")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## INSTALL MISSING PACKAGES
#instpack <- packages %in% installed.packages()[,"Package"]
#if (any(instpack == FALSE)) {
#  install.packages(packages[!instpack])
#}

oo <- options(repos = "https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


###########################################################################

## Change Directory to Data
setwd("C:/Users/minse/Desktop/Psychology/Year2/5PASNRM3/Week9")
getwd()

## Load data
sleepstudy <- data.frame(sleepstudy) # sleepstudy data set is available within lme4


## 1. Inspect and plot data
str(sleepstudy) # 180 obs, 3 vars (Reaction Time, Days, Subject)
dplyr::glimpse(sleepstudy)

# Tabulate data (are the data balanced?)
xtabs( ~ Days + Subject, data = sleepstudy) # all participants have 10 days worth of data

## 2. Plot
p.scatter <- ggplot(sleepstudy, aes(x=Days, y=Reaction)) + 
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(colour= "blue", alpha = .7) +
  theme_minimal() +
  xlab("Sleep deprived days") +
  ylab("Reaction time") +
  labs(title="Reaction time after sleep deprivation") +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 16)) 
p.scatter


# Scatter plot, by subject
p.scatter.subject <- p.scatter + facet_wrap(sleepstudy$Subject, nrow=6, ncol=3)
p.scatter.subject

# Andy's (nice rainbow) version, per subject
ggplot(sleepstudy, aes(x=Days, y=Reaction, color=Subject)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(sleepstudy$Subject, nrow=6, ncol=3) +
  xlab("Sleep deprived days") +
  ylab("Reaction time") +
  labs(title="Reaction time after sleep deprivation") +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 20))

cat("\nSubjects appear to differ in terms of the average reaction time across all days; they also seem to differ in terms of how reaction time depends on the day. But subjects also share certain characteristics: for most subjects reaction time increases with days, and taken all days together, the average reaction time is somewhere around 300ms. We want to model both the shared characteristics as well as the heterogeneity among subjects. - AP")

## 3.	Create a fixed-effect model to explain the sleep data.
# Simple ANOVA (using Type III SS)
sleep.anova_test <- rstatix::anova_test(data = sleepstudy, dv = Reaction, wid = Subject, within = Days)
rstatix::get_anova_table(sleep.anova_test)
summary(sleep.anova_test)


## 4.	Create a random-effects model of sleep data.






library(lme4)
# Build random-intercept model
randomIntercept.model = lmer(Reaction ~ Days + (1|Subject), data=sleepstudy)
randomIntercept.null = lmer(Reaction ~ 1 + (1|Subject), data=sleepstudy)
summary(randomIntercept.model)
anova(randomIntercept.model, randomIntercept.null)

# Examine model fit  
sleepstudy$fitted.mod <- fitted(randomIntercept.model)

# Plot the residuals
ggplot(data = sleepstudy, mapping = aes(y = resid(randomIntercept.model), x = Days)) + 
  geom_hline(yintercept = 0) + geom_point() + geom_line() + facet_wrap(. ~ Subject)

cat("\nThere is some structure left in the residuals. Let’s keep modelling to see if we can improve this - AP.")
# N.B.Andy here: structure in the residuals means there is an external force driving the pattern. For example, subjects 309, 310, 330, 335 all show evidence of a negative downward slope. We should be able to model these DIFFERENCES IN SLOPE across participants... by including a RANDOM SLOPE in our model.

## 5.	Create a mixed effects model of the sleep data to explain reaction time due to sleep deprivation. 
# Create more complex model with random slope model for effect of days
randomSlope.model=lmer(Reaction ~  Days + (0 + Days | Subject), data=sleepstudy) # model with random slope
summary(randomSlope.model)

randomBoth.model=lmer(Reaction ~  Days + (1|Subject) + (0 + Days | Subject), data=sleepstudy) # model with both random slope + intercept
summary(randomBoth.model)

# Optional: Check the model fit for this more complex model. Does the model fit the data better than the random intercept model? 
sleepstudy$fitted.mod <- fitted(randomSlope.model)
ggplot(data = sleepstudy, mapping = aes(y = resid(randomSlope.model), x = Days)) + 
  geom_hline(yintercept = 0) + geom_point() + geom_line() + facet_wrap(. ~ Subject)

# Compare models using ANOVA
anova(randomIntercept.model, randomSlope.model, test="Chisq") # compare random slope vs. random intercept models

# Notes:
# AIC: Lower AIC values indicate a better-fit model, and a model with a delta-AIC (the difference between the two AIC values being compared) of more than -2 is considered significantly better than the model it is being compared to.
# BIC: As complexity of the model increases, bic value increases and as likelihood increases, bic decreases. So, lower is better. 

# Plot fitted lines
df.fitted <- cbind(sleepstudy, data.frame(yhat.randomSlope.model = predict(randomSlope.model), yhat.randomIntercept.model = predict(randomIntercept.model)))
p.fitted <- lattice::xyplot(Reaction + yhat.randomSlope.model + yhat.randomIntercept.model ~ Days | Subject, data = df.fitted, type = "l", ylab = "Reaction", layout = c(6, 3),
                            auto.key = list(text = c("observed", "predicted by randomSlope.model", "predicted by randomIntercept.model"), points = FALSE, lines = TRUE))
p.fitted

## 6.	Explain yours results. 
cat("A Chi-Square difference test revealed a significant difference between the models, (ꭓ2(2)=42.139, <.001). The AIC and BIC are smaller in the model with the random slope, suggesting better model fit.
    \nWe can reject the null hypothesis. The data provide support for the alternative hypothesis that the dependence of reaction time on days sleep deprived varies across subjects.")









# The following is just here to show how you might use nlme to generate the different models
library(nlme)
modelintercept <- nlme::lme(Reaction ~ Days, random = ~1|Subject, data = sleepstudy) # random intercept
summary(modelintercept)

modelboth <- nlme::lme(Reaction ~ Days, random = ~Days|Subject, data = sleepstudy) # random slope and intercept
summary(modelboth)

modelslope <- nlme::lme(Reaction ~ Days, random = ~0+Days|Subject, data = sleepstudy) # random slope
summary(modelslope)