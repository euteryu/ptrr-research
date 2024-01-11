# RM3_2023-2024_Week5_ExamPractice.R
# Created on: October 2023
# Author: Andrew Bell
# Version 1.1, updated Nov 7, 2023
# Purpose: Script accompanying lectures for RM3 - Week 5 (2023-2024)
# This script provides a solution for the Week 5 worksheet

## Load packages
packages = c("ggplot2", "multcomp", "psych", "dplyr", "car", "psycho","afex", "lsmeans", "BayesFactor")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## Change Directory to Data
setwd("~/OneDrive - King's College London/KCL_RM3_CourseData/")
getwd()

## 0. State research question, hypotheses, and analysis plan
## this is a 2x2 repeated-measures design as all participants experience all conditions
## There should be 3 sets of hypotheses (two main effects, one interaction term)


## 1. Load data 
bugs <- read.csv("rm3_2023-2024_week5_bugs.csv", header=TRUE)

## 2. View data (and fix variable types)
head(bugs, 20)
glimpse(bugs)
bugs$gender <- as.factor(bugs$gender)
bugs$subjectID <- as.factor(bugs$subjectID) 
bugs$frightening <- as.factor(ordered(bugs$frightening, levels = c("low", "high")))
bugs$disgusting <- as.factor(ordered(bugs$disgusting, levels = c("low", "high")))
bugs$hostility <- as.numeric(bugs$hostility)
glimpse(bugs)

## 2. Generate quick box plot of complete data set
ggplot2::ggplot(bugs, aes(x = frightening, y = hostility, fill = disgusting)) +
  geom_bar(position='dodge', stat='summary', fun='mean')+
  ggtitle("Effect of level of disgust and fright towards an insect on participant's hostility") +
  xlab("Level of Fright") + ylab("Hostility Rating (0-10)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## 3. Descriptives
(descriptives.f <- describeBy(hostility ~ frightening, data = bugs, digits = 2))
(descriptives.d <- describeBy(hostility ~ disgusting, data = bugs, digits = 2))
(grandMean <- mean(bugs$hostility, na.rm = TRUE))
(grandSD <- sd(bugs$hostility, na.rm = TRUE))

## 4. Run ANOVA
buganova <- afex::aov_ez(
  id = "subjectID",
  dv = "hostility",
  within = c("frightening", "disgusting"),
  data = bugs,
  detailed = TRUE,
  return_aov = TRUE
)

## Students have been experiencing difficulty with rstatix::anova_test()  Here are some approaches that work/don't work

# Works:
(rstaov <- rstatix::anova_test(bugs, hostility ~ frightening * disgusting, wid = subjectID))

# Doesn't work:
#rstaov <- rstatix::anova_test(data = bugs, hostility ~ frightening * disgusting + Error(subjectID/frightening * disgusting))
#rstaov <- anova_test(data = bugs, dv = "hostility", wid = "subjectID", within = c("frightening", "disgusting"))
#rstaov <- rstatix::anova_test(bugs, dv = "hostility", wid = "subjectID",  within = c("frightening", "disgusting"))

xtabs(~ frightening + disgusting, data = bugs)

# check assumptions
hist(buganova$lm$residuals)
qqnorm(buganova$lm$residuals)
shapiro.test(buganova$lm$residuals)

# Inspect ANOVA
summary(buganova)

# Calculate effect size
library(effectsize)
effectsize::eta_squared(buganova)

## 5. Planned Contrasts
# Contrasts
library(lsmeans)
(cell_means <- lsmeans(buganova, specs = c("frightening", "disgusting")))

# contrasts
lowF_lowD_vs_lowF_highD <- c(1, 0, -1, 0) # just as an example 

summary(lsmeans::contrast(  
  cell_means,  
  list(    
    lowF_lowD_vs_lowF_highD = lowF_lowD_vs_lowF_highD
  )
))

# 6. Bayes
library(BayesFactor)
bugs.bayes.anova <- anovaBF(hostility ~ frightening * disgusting, whichRandom = "subjectID", bugs)
summary(bugs.bayes.anova)

# The following code provides a more accessible comparision between the models.
# It divides the BEST model (only main effects) by the NEXT best model (full model)
bugs.bayes.anova[3]/bugs.bayes.anova[4]

# Writeup...

