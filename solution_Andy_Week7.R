# RM3_2023-2024_Week7_ExamPractice.R
# Created on: October 2023, updated Nov 2023
# Author: Andrew Bell
# Version 1.0
# Purpose: Script accompanying lectures for RM3 - Week 7 (2023-2024)
# This script provides a solution for the Week 7 worksheet

## Load packages
packages = c("ggplot2", "multcomp", "psych", "dplyr", "car", "psycho","afex", "lsmeans", "BayesFactor")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE) }
  }
)

## Change Directory to Data
setwd("~/OneDrive - King's College London/KCL_RM3_CourseData/")
getwd()

#### STUDENTS: The following code is NOT part of the solution. This is the code that I used to generate the fake dataset.
#### I'm leaving it in for your interest.

### New Fake Data for Mixed ANOVA
## 00. Create fake data set
rm(list = ls()) # clear up everything  USE WITH CAUTION!
set.seed(42) # set seed to ease debugging (42 = secret of life, universe, and everything)
EM1 <- cbind(seq(1, 20), abs(rnorm(20, mean = 24, sd = 5)), "Energy", "M1")
EV1 <- cbind(seq(1, 20), rnorm(20, mean = 50, sd = 14), "Energy", "Vertex")
CM1 <- cbind(seq(21, 40), rnorm(20, mean = 28, sd = 8), "Classic", "M1")
CV1 <- cbind(seq(21, 40), rnorm(20, mean = 52, sd = 11), "Classic", "Vertex")
WM1 <- cbind(seq(41, 60), rnorm(20, mean = 77, sd = 6), "Water", "M1")
WV1 <- cbind(seq(41, 60), rnorm(20, mean = 65, sd = 10), "Water", "Vertex")

coke.data <- data.frame(rbind(EM1, EV1, CM1, CV1, WM1, WV1))
names(coke.data) <- c("ParticipantID", "MEP", "Drink", "Site")

write.csv(coke.data,'RM3_2023-2024_Week7_SampleExamData_CokeEnergy.csv')
rm(list = ls()) # clear up everything  USE WITH CAUTION!

#### OK - that's it. The rest is the solution.






## 0. State research question, hypotheses, and analysis plan
## this is a 2x3 mixed design
## There should be 3 sets of hypotheses (two main effects, one interaction term)

## 1. Load data, inspect, fix, and inspect again
coke.data <- read.csv('RM3_2023-2024_Week7_SampleExamData_CokeEnergy.csv')
dplyr::glimpse(coke.data)

coke.data$MEP <- as.numeric(coke.data$MEP)
coke.data$ParticipantID <- as.numeric(coke.data$ParticipantID)
coke.data$Drink <- as.factor(coke.data$Drink)
coke.data$Site <- as.factor(coke.data$Site)
dplyr::glimpse(coke.data)

## 2. Plot data
library(ggplot2)
ggplot(coke.data, aes(x=Drink, y=MEP, fill=Site)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Effect of Drink on MEP Threshold as a function of stimulation site") +
  xlab("Drink Consumed") + ylab("MEP Threshold (mV)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## 3. run ANOVA
coke.anova <- afex::aov_ez(
  id = "ParticipantID",
  dv = "MEP",
  between = "Drink",
  within = "Site",
  data = coke.data,
  detailed = TRUE,
  return_aov = TRUE
)

# check assumptions
hist(coke.anova$lm$residuals)
qqnorm(coke.anova$lm$residuals)
shapiro.test(coke.anova$lm$residuals)
car::leveneTest(MEP ~ Drink * Site, data = coke.data, center = "mean")

xtabs(~Drink + Site, data = coke.data)
## 4. Planned Contrasts
# Contrasts
library(lsmeans)
(cell_means <- lsmeans(coke.anova, specs = c("Drink", "Site")))

# Set up contrast vectors
sugar_vs_water_m1 <- c(-1, -1, 2, 0, 0, 0) # compare water vs. sugary drinks (for M1 only)
classic_vs_energy_m1 <- c(1, -1, 0, 0, 0, 0) # classic vs. energy (for M1 only)
sugar_vs_water_vtx <- c(0, 0, 0, -1, -1, 2) # compare water vs. sugary drinks (for vtx only)
classic_vs_energy_vtx <- c(0, 0, 0, 1, -1, 0) # classic vs. energy (for vtx only)

# Run Contrasts 
summary(contrast(cell_means, list(sugar_vs_water_m1, classic_vs_energy_m1, sugar_vs_water_vtx, classic_vs_energy_vtx)))

## 5. Writeup...