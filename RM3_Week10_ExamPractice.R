# Name:       RM3_Week10_ExamPractice.R
# Created on: 27/11/23
# Author:     Minseok Ryu
# Version:    1.0
# Purpose:    Practice Exam Attempt #1

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

# load standard libraries
packages = c("ggplot2", "dplyr", "ggpubr", "psych", "tidyverse", "car", "lme4", "MuMIn")
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

# lme4 depends on 'Matrix' pkg which recently changed
# Perhaps this is causing issues ?
oo <- options(repos = "https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


###########################################################################

## Change Directory to Data
setwd("C:/Users/k22014863/OneDrive - King's College London/Year2/5PASNRM3/Week10")
getwd()

## Load data
sesamestudycsv = read.csv("RM3_2023-2024_Week10_SampleExamData_Sesame.csv", header=TRUE)
sesamestudy <- data.frame(sesamestudycsv)

## 1. Inspect and plot data
str(sesamestudy) 
dplyr::glimpse(sesamestudy)

# Tabulate data (are the data balanced?)
xtabs( ~ view_freq + peabody, data = sesamestudy) # all participants have 10 days worth of data

## 2. Plot
p.scatter <- ggplot(sesamestudy, aes(x=view_freq, y=numb_improv)) + 
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(colour= "blue", alpha = .7) +
  theme_minimal() +
  xlab("View frequency of Sesame Street per week") +
  ylab("Improvement in cognitive dev score") +
  labs(title="Cognitive dev score after viewing Sesame Street") +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 16)) 
p.scatter


# Scatter plot, by child subject
p.scatter.subject <- p.scatter + facet_wrap(sesamestudy$Subject, nrow=6, ncol=3)
p.scatter.subject

ggplot(sesamestudy, aes(x=view_freq, y=numb_improv, color=X)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(sesamestudy$X, nrow=6, ncol=3) +
  xlab("View frequency of Sesame Street per week") +
  ylab("Improvement in cognitive dev score") +
  labs(title="Cognitive dev score after viewing Sesame Street") +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 20))

cat("\nSubjects appear to differ in terms of the cognitive improvements across all viewing freq.")

## 3.	Create a fixed-effect model to explain the sleep data.
# Simple ANOVA (using Type III SS)
sesame.anova_test <- rstatix::anova_test(data = sesamestudy, dv = numb_improv, wid = X, within = view_freq)
rstatix::get_anova_table(sesame.anova_test)
summary(sesame.anova_test)


## 4.	Create a random-effects model of sleep data.
library(lme4)
# Build random-intercept model
randomIntercept.model = lmer(numb_improv ~ view_freq + (1|X), data=sesamestudy)
randomIntercept.null = lmer(numb_improv ~ 1 + (1|X), data=sesamestudy)
summary(randomIntercept.model)
anova(randomIntercept.model, randomIntercept.null)

# Examine model fit  
sesamestudy$fitted.mod <- fitted(randomIntercept.model)

# Plot the residuals
ggplot(data = sesamestudy, mapping = aes(y = resid(randomIntercept.model), x = Days)) + 
  geom_hline(yintercept = 0) + geom_point() + geom_line() + facet_wrap(. ~ x)


## 5.	Create a mixed effects model of the sleep data to explain reaction time due to sleep deprivation. 
# Create more complex model with random slope model for effect of days
randomSlope.model=lmer(numb_improv ~  view_freq + (0 + view_freq | X), data=sesamestudy) # model with random slope
summary(randomSlope.model)

randomBoth.model=lmer(numb_improv ~  view_freq + (1|X) + (0 + view_freq | X), data=sesamestudy) # model with both random slope + intercept
summary(randomBoth.model)

# Check the model fit for this more complex model. Does the model fit the data better than the random intercept model? 
sesamestudy$fitted.mod <- fitted(randomSlope.model)
ggplot(data = sesamestudy, mapping = aes(y = resid(randomSlope.model), x = view_freq)) + 
  geom_hline(yintercept = 0) + geom_point() + geom_line() + facet_wrap(. ~ X)

# Compare models using ANOVA
anova(randomIntercept.model, randomSlope.model, test="Chisq") # compare random slope vs. random intercept models

# Notes:
# AIC: Lower AIC values indicate a better-fit model, and a model with a delta-AIC (the difference between the two AIC values being compared) of more than -2 is considered significantly better than the model it is being compared to.
# BIC: As complexity of the model increases, bic value increases and as likelihood increases, bic decreases. So, lower is better. 

# Plot fitted lines
df.fitted <- cbind(sesamestudy, data.frame(yhat.randomSlope.model = predict(randomSlope.model), yhat.randomIntercept.model = predict(randomIntercept.model)))
p.fitted <- lattice::xyplot(Reaction + yhat.randomSlope.model + yhat.randomIntercept.model ~ Days | Subject, data = df.fitted, type = "l", ylab = "Improvement in cog dev score", layout = c(6, 3),
                            auto.key = list(text = c("observed", "predicted by randomSlope.model", "predicted by randomIntercept.model"), points = FALSE, lines = TRUE))
p.fitted
