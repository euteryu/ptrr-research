# RM3_2023-2024_Week4_FactorialANOVA.R
# Created on: sometime August 2023
# Author: Andrew Bell
# Version 1.0
# Purpose: Script accompanying lectures for RM3 - Week 4 (2023-2024)
# This script creates a fake dataset loosely inspired by Barrett et al., 2018; a study on the effects of psilocybin vs. dextromethorphan on cognition

## Load packages
packages = c("ggplot2", "multcomp", "psych", "dplyr", "car", "psycho","afex", "lsmeans")
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

## 0. Create fake data set (using SOA of 300 ms)
rm(list = ls()) # clear up everything  USE WITH CAUTION!
set.seed(42) # set seed to ease debugging (42 = secret of life, universe, and everything)

# Simulating a task with 50 trials with 20 targets and 30 distractors
genData <- function(h, f, n) {
  temp_h <- t(round(rnorm(n, h, 2)))
  temp_h <- apply(matrix(temp_h), 2, function(x) ifelse(x > 20, 20, x))
  temp_h <- apply(matrix(temp_h), c(1,2), function(x) ifelse(x < 0, 0, x))
  temp_f <- t(round(rnorm(n, f, 2)))
  temp_f <- apply(matrix(temp_f), 2, function(x) ifelse(x > 30, 30, x))
  temp_f <- apply(matrix(temp_f), c(1,2), function(x) ifelse(x < 0, 0, x))
  temp_m <- 20 - temp_h
  temp_r <- 30 - temp_f
  tempstat <- psycho::dprime(n_hit = temp_h, 
                             n_fa = temp_f, 
                             n_miss = temp_m, 
                             n_cr = temp_r)
  return(
    cbind(temp_h, temp_f, temp_m, temp_r, tempstat$dprime)
  )
}

# Construct individual vectors for each group
(ctrlPlacebo <- genData(19, 2, 20))
(ctrlPsilo <-   genData(17, 7, 20))
(ctrlDextro <-  genData(17, 5, 20))
(ctrlFluox <-   genData(19, 2, 20))
(subPlacebo <-  genData(17, 3, 20))
(subPsilo <-    genData(17, 8, 20))
(subDextro <-   genData(17, 5, 20))
(subFluox <-    genData(18, 1, 20))
(clinPlacebo <- genData(15, 5, 20))
(clinPsilo <-   genData(13, 12, 20))
(clinDextro <-  genData(15, 6, 20))
(clinFluox <-   genData(12, 1, 20))

# Combine vectors into dataframe
cogdrugDF <- data.frame(rbind(
  cbind("ctrl", "placebo", ctrlPlacebo),
  cbind("ctrl", "psilocybin", ctrlPsilo),
  cbind("ctrl", "dextrom", ctrlDextro),
  cbind("ctrl", "fluoxetine", ctrlFluox),
  cbind("subclin", "placebo", subPlacebo),
  cbind("subclin", "psilocybin", subPsilo),
  cbind("subclin", "dextrom", subDextro),
  cbind("subclin", "fluoxetine", subFluox),
  cbind("clinD", "placebo", clinPlacebo),
  cbind("clinD", "psilocybin", clinPsilo),
  cbind("clinD", "dextrom", clinDextro),
  cbind("clinD", "fluoxetine", clinFluox)
))

# Clean dataframe
names(cogdrugDF) <- list("clinStatus", "drug", "hits", "fa", "misses", "corr_rejections", "dprime")
cogdrugDF <- cbind(subjectID = 1:nrow(cogdrugDF), cogdrugDF)
cogdrugDF$dprime <- as.numeric(cogdrugDF$dprime)

cogdrugDF$clinStatus <- ordered(cogdrugDF$clinStatus, levels = c("ctrl", "subclin", "clinD"))
cogdrugDF$drug <- ordered(cogdrugDF$drug, levels = c("placebo", "fluoxetine", "psilocybin", "dextrom"))

save(cogdrugDF, file="rm3_2023-2024_week4_cognitiveDrugDataset_dprime.RData") # save your work!
write.csv(cogdrugDF, file="rm3_2023-2024_week4_cognitiveDrugDataset_dprime.csv", row.names=FALSE)

cogdrugDF <- subset(cogdrugDF, select = -dprime)
write.csv(cogdrugDF, file="rm3_2023-2024_week4_cognitiveDrugDataset.csv", row.names=FALSE)

# Remove unnecessary variables (no longer necessary and take up memory)
rm(list = ls()) # clear up everything  USE WITH CAUTION!

## 1. Load data 
cogdrugDF <- read.csv("rm3_2023-2024_week4_cognitiveDrugDataset.csv", header=TRUE)

## 2. View data (and fix variable types)
cogdrugDF$clinStatus <- as.factor(cogdrugDF$clinStatus)
glimpse(cogdrugDF)

## 3. Calculate dprime

tempstat <- psycho::dprime(n_hit = cogdrugDF$hits, 
                           n_fa = cogdrugDF$fa, 
                           n_miss = cogdrugDF$misses, 
                           n_cr = cogdrugDF$corr_rejections)
cogdrugDF$dprime <- as.numeric(tempstat$dprime)

## 3. Generate quick box plot of complete data set
cogdrugDF$clinStatus <- ordered(cogdrugDF$clinStatus, levels = c("ctrl", "subclin", "clinD"))
cogdrugDF$drug <- ordered(cogdrugDF$drug, levels = c("placebo", "fluoxetine", "psilocybin", "dextrom"))
ggplot2::ggplot(cogdrugDF, aes(x = clinStatus , y = dprime, fill = drug)) +
  geom_bar(position='dodge', stat='summary', fun='mean')+
  ggtitle("Effect of drug use on cognitive status as a function of clinical status") +
  xlab("Clinical Status") + ylab("d-prime") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## 4. Run ANOVA
cogdruganova <- afex::aov_ez(
  id = "subjectID",
  dv = "dprime",
  between = c("clinStatus", "drug"),
  data = cogdrugDF,
  detailed = TRUE,
  return_aov = TRUE
)

# check assumptions
hist(cogdruganova$lm$residuals)
qqnorm(cogdruganova$lm$residuals)
shapiro.test(cogdruganova$lm$residuals)
car::leveneTest(dprime ~ clinStatus * drug, data = cogdrugDF, center = "mean")

# Inspect ANOVA
summary(cogdruganova)

# Calculate effect size
library(effectsize)
effectsize::eta_squared(cogdruganova)

## 5. Planned Contrasts
# Contrasts
library(lsmeans)
(cell_means <- lsmeans(cogdruganova, specs = c("clinStatus", "drug")))

# contrasts
placebo_vs_drugs <-     c(3,  3,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1)
ctrl_vs_depressed <-    c(2, -1, -1,  2, -1, -1,  2, -1, -1,  2, -1, -1)
flux_vs_psychedelics <- c(0,  0,  0,  2,  2,  2, -1, -1, -1, -1, -1, -1)

summary(lsmeans::contrast(  
  cell_means,  
  list(    
    placebo_vs_drugs = placebo_vs_drugs,    
    ctrl_vs_depressed = ctrl_vs_depressed,    
    flux_vs_psychedelics = flux_vs_psychedelics 
  )
))

## 6. Pairwise (the Brute Force method)
pairwise.t.test(cogdrugDF$dprime, cogdrugDF$clinStatus, p.adjust.method = "none")
pairwise.t.test(cogdrugDF$dprime, cogdrugDF$clinStatus, p.adjust.method = "bonferroni")
pairwise.t.test(cogdrugDF$dprime, cogdrugDF$clinStatus, p.adjust.method = "holm")