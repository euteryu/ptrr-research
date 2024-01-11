# RM3_2023-2024_Week3_OneWayANOVA_contrasts.R
# Created on: sometime August 2023
# Author: Andrew Bell
# Version 1.0
# Purpose: Script accompanying lectures for RM3 - Week 3 (2023-2024)
# This script creates a fake dataset inspired by Zhan et al., 2023 Sci Adv; a study on subdivisions of VWFA in bilingual individuals.

## Load packages
packages = c("ggplot2", "multcomp", "psych", "dplyr", "car")
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
setwd("C:/Users/minse/Desktop/Psychology/Year2/5PASNRM3/Week3")
getwd()

## 0. Create simulated dataset
# 0.1 Start with individual vectors created using "rnorm" (randomised normal variable)
rm(list=c("english", "french", "chinese", "arabic", "hebrew")) # clear existing variables

set.seed(42) # set seed to ease debugging (42 = secret of life, universe, and everything)
english <- rnorm(16, 4.0, 1) #
french <-  rnorm(16, 4.0, 1) #
chinese <- rnorm(16, 2, 1)   #
arabic <-  rnorm(16, 5.5, 1) #
hebrew <-  rnorm(16, 0.2, 1) #

# 0.2 Combine into a single dataframe
languageDF <- data.frame(rbind(
  cbind(english, "english"),
  cbind(french, "french"),
  cbind(chinese, "chinese"),
  cbind(arabic, "arabic"),
  cbind(hebrew, "hebrew")
))
names(languageDF) <- list("percentSignalChange", "language")
save(languageDF, file="rm3_2023-2024_week3_vwfaDataset.RData") # save your work!
write.csv(languageDF, "rm3_2023-2024_week3_vwfaDataset.csv", row.names=FALSE)

# 0.3 Remove unnecessary variables (no longer necessary and take up memory)
rm(list=c("english", "french", "chinese", "arabic", "hebrew")) # clear existing variables

# 0.4 - CODING CHALLENGE - try doing the above in a single command!

## 1. Generate hypothesis
# What is your hypothesis?

## 2. Planned Contrasts
# Confirm labels and order
levels(languageDF$language)

# What are your planned contrasts?
# e.g., 1. spoken languages vs. non-spoken languages? 2. Arabic vs. English
# Q. What other options are there?

# Contrast contrasts
# 1: Spoken vs. Not-Spoken
# 2: Arabic vs. English
contrast1 <- c(3, -2, 3, -2, -2) # Spoken vs. Not-Spoken
contrast2 <- c(1, 0, -1, 0, 0)  # Arabic vs. English
(contrasts(languageDF$language) <- cbind(contrast1, contrast2))

## 3 & 4. New Script and Load Data
rm(list = ls()) # clear up everything  USE WITH CAUTION!
languageDF <- read.csv("rm3_2023-2024_week3_vwfaDataset.csv", header=TRUE)

## 5. View data (and fix variable types)
languageDF$percentSignalChange <- as.numeric(languageDF$percentSignalChange)
languageDF$language <- as.factor(languageDF$language)
glimpse(languageDF)

## 6. Descriptive Statistics
psych::describeBy(languageDF$percentSignalChange, group = languageDF$language, digits = 2, data = languageDF)

(marginalMeans <- tapply(languageDF$percentSignalChange, languageDF$language, mean))
(individualMeans <- aggregate(percentSignalChange ~ language, data = languageDF, mean))

## 7. Generate quick box plot of complete data set
boxplot(percentSignalChange ~ language, data = languageDF,
        main = "Activity in VWFA patch in response to words of a different language", 
        xlab = "Language", ylab = "BOLD Response (% Signal Change)",         
        cex.main=1.5, cex.lab=1.25, cex.axis=1)

## 8. Run ANOVAs and check assumptions
# 8.1 ANOVA (aov)
language_aov <- aov(percentSignalChange ~ language, languageDF)

# check assumptions
hist( x = language_aov$residuals )
qqnorm( y = language_aov$residuals )
shapiro.test( x = language_aov$residuals )
car::leveneTest(percentSignalChange ~ language, data=languageDF)

## 11. Pairwise/Tukeys (the Brute Force method)
pairwise.t.test(languageDF$percentSignalChange, languageDF$language, p.adjust.method = "none")
pairwise.t.test(languageDF$percentSignalChange, languageDF$language, p.adjust.method = "bonferroni")
pairwise.t.test(languageDF$percentSignalChange, languageDF$language, p.adjust.method = "holm")

postHocs <- glht(language.anova.contrasts, linfct = mcp(language = "Tukey"))
summary(postHocs)

# 12. Run ANOVA with planned contrasts
language.anova.contrasts <- aov(percentSignalChange ~ language, languageDF)
summary.lm(language.anova.contrasts)

# 13. Effect Size
lsr::etaSquared(language.anova.contrasts)


# Coding Challenge