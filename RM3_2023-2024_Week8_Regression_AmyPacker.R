# Script name: RM3_2023-2024_Week8_Regression_AmyPacker.R
# Created on: Oct 2021
# Author: Amy Packer
# Version 1.0
# Purpose: Script for RM3 Week 8 In Person Workshop
# Notes:  Created using Andy Bell's lecture script, as reference

#### GENERAL NOTES #### 
# Students can pick from three choices for this session:
#   1.	Create your own model to predict property prices using the London Housing Dataset. The lecture model explained 40% of the variance (adjusted R2 = .4).  Can you do better? 
#   2.	We know from the lecture material that the lecture model violated several of the assumptions of multiple regression. We also discussed several possible ways to address these violations. Can you find a solution to one or more of the violations?
#   3.	The Decade of Dance Dataset contains a number of variables about songs across the decade. Can you generate a model that will predict the popularity of a given song (with an adjusted R-squared above 50%)?
# Overfitting can be an issue if include too many predictors. No. of predictors affects the adjusted R.

## Load Packages
packages = c("ggplot2", "dplyr", "afex", "ez", "rstatix", "psych", 
             "ggpubr", "gvlma", "car", "MASS", "multcomp", "reshape2", "ps",
             "corrplot")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# 1. Be clear on hypotheses ----------------------------------------------#
# 2. Start new script ----------------------------------------------------#
# 3. Research Q and hypotheses -------------------------------------------#
# 4. Load data (and clean workspace & load packages) ---------------------#

# Clean workspace
rm(list=ls()) 

# Set working directory
setwd("C:/Users/minse/Desktop/Psychology/Year2/5PASNRM3/Week8")

# Load data
londonHousing <- read.csv('london.csv', header = TRUE)
# could also use rio::import() - rio::import('london.csv') 
# Note, the package::function() syntax allows you to access a function without loading package in library.

# Clean names 
names(londonHousing)
londonHousing <- janitor::clean_names(londonHousing) # make sure assign back to data frame
names(londonHousing)

## Subset data ----------------------------------------------------------#
myvars <- c("Flat / Apartment", "House", "New development")

#  Subset data - per lectures/base R:
londonHousing_cheap.lecture <- subset(londonHousing, price <= 10000000 & no_of_bedrooms < 6 & no_of_bedrooms > 1)
londonHousing_cheap_clean.lecture <- subset(londonHousing_cheap.lecture, house_type %in% myvars)
londonHousing_cheap_clean.lecture$house_type <- factor(londonHousing_cheap_clean.lecture$house_type)
londonHousing_cheap_clean.lecture$price_k <- londonHousing_cheap_clean.lecture$price / 1000
londonHousing_cheap_clean.lecture$logprice <- log(londonHousing_cheap_clean.lecture$price)

# Subset data - using tidyverse pipeline:
londonHousing_cheap_clean <- londonHousing %>%
  dplyr::filter(price <= 10000000 & no_of_bedrooms < 6 & no_of_bedrooms > 1) %>%
  dplyr::filter(house_type %in% myvars) %>%
  dplyr::mutate(price_k = price / 1000) %>% # transform the price to be in thousands of Pounds
  dplyr::mutate(logprice = log(price)) %>% 
  dplyr::mutate(house_type = factor(house_type)) %>% 
  dplyr::select(!c(property_name, city_county)) # drop the variables that we are not in our model

## Initial assumption checks ------------------------#
# Note there are 5 assumptions to check - there are 2 we can check prior to running the model. 
# Also, useful to check the distribution of the model... 

# a. Inspect distribution.
# Using hist()
hist(londonHousing_cheap_clean$price_k)

# Boring histogram plot
ggplot(data = londonHousing_cheap_clean, aes(x = price_k)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Housing prices in London",
       x = "price (k£)",
       y = "Count") + 
  facet_wrap(~ house_type) # creates panels for each house type

## less boring histogram plot with price plotted as log
ggplot(data = londonHousing_cheap_clean, aes(logprice)) +
  geom_histogram() +
  labs(title = "Housing prices in London",
       x = "log(price (£))",
       y = "Count") + 
  facet_wrap(~ house_type)
# N.B. could use log-transformed data

# another plot because why not
interaction.plot(trace.factor = londonHousing_cheap_clean$house_type,
                 x.factor = londonHousing_cheap_clean$no_of_bedrooms,
                 response = londonHousing_cheap_clean$price_k,
                 fun = mean,
                 xlab = "Number of Bedrooms",
                 ylab = "price (£k)",
                 trace.label = "House Type",
                 type="b",
                 col=c("blue","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

# b. Assumption: linearity of of relationship between predictors and outcome.
# irrelevant to dummy coded variables: Dummy encoded variables are linear by design
library(ggpubr)
ggscatter(data = londonHousing_cheap_clean, x = "area_in_sq_ft", y = "price_k", facet.by = c("no_of_bedrooms", "house_type"))+
  geom_smooth(method = lm)

p1 <- ggscatter(data = londonHousing_cheap_clean, x = "area_in_sq_ft", y = "price_k") + geom_smooth(method = lm)
p2 <- ggscatter(data = londonHousing_cheap_clean, x = "no_of_bedrooms", y = "price_k") + geom_smooth(method = lm)
p3 <- ggscatter(data = londonHousing_cheap_clean, x = "house_type", y = "price_k") + geom_smooth(method = lm)
ggarrange(p1, p2, p3, ncol = 3)

# c. Assumption of absence of multicollinearity 
# Note: this assumption can only be partly checked (VIF can only be ontained by runnning model).
library(corrplot)
miniLondon <- subset(londonHousing_cheap_clean, select = c("house_type", "no_of_bedrooms", "area_in_sq_ft") )
miniLondon$house_type <- as.numeric(miniLondon$house_type)
cormat <- round(cor(miniLondon), 2)
corrplot.mixed(cormat, order = "AOE", tl.pos = "lt", tl.col = "black", tl.cex = 1.2)

## Inspect missingness --------------------------------------------------#
# Note: pairwise deletion is the default in lm()
# pairwise deletion = excluding fully each observation with even one missing value on one of the variables of interest.
sum(stats::complete.cases(londonHousing_cheap_clean))

summary(londonHousing_cheap_clean) 
skimr::skim(londonHousing_cheap_clean)
table(is.na(londonHousing_cheap_clean$house_type))
table(is.na(londonHousing_cheap_clean$no_of_bedrooms)) 
table(is.na(londonHousing_cheap_clean$area_in_sq_ft)) 

## Descriptives for predictors and outcomes ----------------------------#
# Good news: skimr::skim() above would have just given use these!
# skimr::skim(londonHousing_cheap_clean) 
# summary(londonHousing_cheap_clean) 

# Explore and Summarise data
xtabs(~ house_type + no_of_bedrooms, londonHousing_cheap_clean)

aggregate( price ~ house_type + no_of_bedrooms, 
           londonHousing_cheap_clean, mean )

## Regression model ---------------------------------------------------#
# Generate regression model
myLondon_model <- lm(price_k ~ house_type + area_in_sq_ft + no_of_bedrooms, 
                     data = londonHousing_cheap_clean)
summary.lm(myLondon_model)

# for fun, compare coefficients when price NOT divided by 1000
myLondon_model_full <- lm(price ~ house_type + area_in_sq_ft + no_of_bedrooms, 
                          data = londonHousing_cheap_clean)
summary.lm(myLondon_model_full)

myLondon_model$coefficients
myLondon_model_full$coefficients

# 10. Finish assumption checks -------------------------------------------#

# Here is a breakdown of the model fit plots
plot(myLondon_model, which = 1) # Residuals vs. Fitted - normality, want dots equally 
# distibuted above and below the horizontal line - want red line straight and horizontal.
plot(myLondon_model, which = 2) # QQ Plot - normality
plot(myLondon_model, which = 3) # Scale-Location - homogeneity, again want a 
# nice straight horizontal line
plot(myLondon_model, which = 4) # Cook's Distance
plot(myLondon_model, which = 5) # Leverage
plot(myLondon_model, which = 6) # Cook's Dist

# OR do the below:
layout(matrix(c(1,2,3,4),2,2)) # Specify a 2x2 layout
plot(myLondon_model)
layout(1) # Restore previous default layout


# a. normality of residuals
# see the Residuals vs. Fitted & QQ Plot

# Also, Shapiro-Wilk
shapiro.test(myLondon_model$residuals)

# b. homogeniety of variance
# see the Scale-Location plot.

# ALso, Levene's test:
leveneTest(myLondon_model)
# Error in leveneTest.formula(f, data = m, ...) : 
#Levene's test is not appropriate with quantitative explanatory variables.

# Levene’s Test will not work with numeric continuous variables. 
# Therefore, we need to use a different test: the non-constant variance score test using ncvTest()
# It works in the same way: a significant p-value means that the assumption has been violated

library(car)
ncvTest(myLondon_model) # another way of testing variances

# c. absence of multicollinearity calculating the VIF
## Generate figure that shows correlation matrix between predictor variables
# Create smaller version of dataset to make calculating correlation matrix easier
miniLondon <- subset(londonHousing_cheap_clean, select = c("house_type",
                                                           "no_of_bedrooms", "area_in_sq_ft"))
# Convert House.Type to NUMERIC (can’t calculate correlation matrix with categoricals)
miniLondon$house_type <- as.numeric(miniLondon$house_type)
# Calculate correlation matrix and plot
cormat <- round(cor(miniLondon), 2)
corrplot.mixed(cormat, order = "AOE", tl.pos = "lt", tl.col = "black", tl.cex = 1.2)

# The variance inflation factor is a measure of how much the variance has increased due to collinearity.
# Values above 5 are considered problematic
car::vif(myLondon_model)


# 11. Address unmet assumption checks --------------------------------------#

#Violated assumptions, what are our options?  Fortunately several:
# Clean/refine our dataset
# Limit our dataset further (e.g., price < £2.5M)
# Remove outliers (can use leverage and cook’s distance to identify culprits)

# 12. Write up results  --------------------------------------#

# Could put results in a table...
# Field (2013): The APA seem in favour of reporting, as a bare minimum, the standardized betas, their significance
# value and some general statistics about the model (such as the R2). If you do decide
# to do a table then the beta values and their standard errors are also very useful. Personally
# I’d like to see the constant as well because then readers of your work can construct the full
# regression model if they need to.
