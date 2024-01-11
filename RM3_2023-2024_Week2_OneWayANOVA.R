# RM3_2023-2024_Week2_OneWayANOVA.R
# Created on: sometime September 2023
# Author: Andrew Bell
# Version 1.0
# Purpose: Script accompanying lectures for RM3 - Week 2 (2023-2024)
# This script creates a fake dataset based on a BSc N&P student research project supervised by T.Trotter. The study examined the effect of response compatibility on the automatic imitation effect (see worksheet for details)

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
setwd("C:/Users/k22014863/OneDrive - King's College London/Year2/5PASNRM3")
getwd()

## 0. Create dataset (using SOA of 300 ms)
# NOTE TO STUDENTS: You don't need to worry about any of Step 0. This is how I have created the dataset that is available on KEATS. I am including this code so you can see how I did it.

# 0.1 Start with individual vectors created using "rnorm" (randomised normal variable)
set.seed(42) # set seed to ease debugging (42 = secret of life, universe, and everything)
congruent <- rnorm(40, 969.8894194, 94.76406461) # congruent ~N(969.8894194, 94.76406461^2)
incongruent<-rnorm(40, 999.9460667, 97.63992126) # incongruent ~N(969.8894194, 94.76406461^2)
baseline <-  rnorm(40, 989.1498716, 96.07554452) # baseline ~N(969.8894194, 94.76406461^2)

# 0.2 Combine into a single dataframe
imitationDF <- data.frame(rbind(
  cbind(congruent, "congruent", 1),
  cbind(incongruent, "incongruent", -1),
  cbind(baseline, "baseline", 0)
))

# 0.3 Clean up and save as rdata and csv
names(imitationDF) <- list("reactionTime", "condition", "condition_num")
save(imitationDF, file="rm3_2023-2024_week2_imitationDataset.RData") # save your work!
write.csv(imitationDF, "rm3_2023-2024_week2_imitationDataset.csv", row.names=FALSE)

# 0.4 Remove unnecessary variables (no longer necessary and take up memory)
rm(list = ls()) # clear up everything  USE WITH CAUTION!

# 0.5 - SECRET CODING CHALLENGE - try doing all of the above in a single command!

## 1. Load and View data (and fix)
imitationDF <- read.csv("rm3_2023-2024_week2_imitationDataset.csv", header = TRUE)
glimpse(imitationDF)

# What do you notice about the variable type for reactionTime?  Is it correct? (Hint: Maybe not.)
imitationDF$reactionTime <- as.numeric(imitationDF$reactionTime)
imitationDF$condition_num <- as.numeric(imitationDF$condition_num)
imitationDF$condition <- as.factor(imitationDF$condition)
glimpse(imitationDF)

## 2. Descriptives
describeBy(reactionTime ~ condition, data = imitationDF, digits = 2)
tapply(imitationDF$reactionTime, imitationDF$condition, mean)
aggregate(reactionTime ~ condition, data = imitationDF, mean)
aggregate(reactionTime ~ condition, data = imitationDF, sd)

## 3. Generate quick box plot of complete data set
# 3.1 Simple Boxplot
boxplot(reactionTime ~ condition, data = imitationDF,
        main = "Response Compatibility on Automatic Imitation Effect", xlab = "Condition", ylab = "Reaction Time (ms)", 
        cex.main=1.5, cex.lab=1.25, cex.axis=1)

## 4., 5., 6. Run ANOVAs
# 4.1 ANOVA (aov)
imitation_aov <- aov(reactionTime ~ condition, imitationDF)
summary(imitation_aov)

# check assumptions
hist( x = imitation_aov$residuals )
qqnorm( y = imitation_aov$residuals )
shapiro.test( x = imitation_aov$residuals )
car::leveneTest(reactionTime ~ condition, data=imitationDF)

# 4.2 ANOVA (lm)
imitation_lm <- lm(reactionTime ~ condition, imitationDF)
print(imitation_lm)
summary(imitation_lm)

hist( x = imitation_lm$residuals )
qqnorm( y = imitation_lm$residuals )
shapiro.test( x = imitation_lm$residuals )
car::leveneTest(imitation_lm)

# for some of the following commands, you need a subjectID column (for reasons that will become clearer
# later in the course.)
# Assumptions can be tested in the same way as above so they are not included in the code below.

imitationDF <- cbind(subjectID = 1:nrow(imitationDF), imitationDF)

# 4.3 rstatix::anova_test()
library(rstatix)
imitation.anova_test <- anova_test(
  data = imitationDF,
  dv = reactionTime,
  wid = subjectID, 
  between = condition
)
get_anova_table(imitation.anova_test)

# 4.4 afex::aov_ez()
library(afex)
imitation.afex.anova <- aov_ez(
  id = "subjectID",
  dv = "reactionTime",
  data = imitationDF, 
  between = "condition",
  detailed = TRUE, 
  return_aov = TRUE)
summary(imitation.afex.anova)

# 4.5 ez::ezANOVA()
library(ez)
imitation.ez.anova <- ezANOVA(
  data=imitationDF, 
  dv = reactionTime,
  wid = subjectID, 
  between = condition
  )
imitation.ez.anova # Note the lack of "summary" call here

## 7. Pairwise
pairwise.t.test(imitationDF$reactionTime, imitationDF$condition, p.adjust.method = "none")
pairwise.t.test(imitationDF$reactionTime, imitationDF$condition, p.adjust.method = "bonferroni")
pairwise.t.test(imitationDF$reactionTime, imitationDF$condition, p.adjust.method = "holm")

postHocs <- glht(imitation_aov, linfct = mcp(condition = "Tukey"))
summary(postHocs)
postHocs.lm <- glht(imitation_lm, linfct = mcp(condition = "Tukey"))
summary(postHocs.lm)

# 8. See lecture notes for how to report the results of an ANOVA.

## Coding/Knowledge Challenge
imitation_aov <- aov(reactionTime ~ condition, imitationDF)
summary(imitation_aov)

imitation_conditionNum_aov <- aov(reactionTime ~ condition_num, imitationDF)
summary(imitation_conditionNum_aov)

# Using the condition_num field instead of condition gives different results. 
# This is because condition_num is being treated as a CONTINUOUS variable whereas 
# condition is being treated as a CATEGORICAL variable. ANOVAs require the independent 
# variables to be categorical. Therefore, using condition_num will result in a WRONG answer. 
# The lesson here is that, when conducting an ANOVA, make sure your IVs are either text-based OR 
# you've forced them to be treated as factors (using the as.factor()) command.