# Script name: RM3_2023-2024_Week8_Regression_Lorenzo.R
# Created on: Nov 2023
# Author: Lorenzo
# Purpose: Script for RM3 Week 8 In Person Workshop
# Notes:  based on previous examples

## Load Packages
packages = c("ggplot2", "dplyr", "afex", "ez", "rstatix", "psych", 
             "ggpubr", "gvlma", "car", "MASS", "multcomp", "reshape2", "ps",
             "corrplot", "janitor", "skimr", "rsq", "performance")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Set working directory
setwd("C:/Users/minse/Desktop/Psychology/Year2/5PASNRM3/Week8")

#DATASET CLEANING####
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


## INITIAL ASSUMPTION CHECK ####
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


ggplot(data = londonHousing_cheap_clean, aes(x = price_k, fill= no_of_bedrooms)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Housing prices in London",
       x = "price (k£)",
       y = "Count") + 
  facet_wrap(~ house_type)

ggplot(data = londonHousing_cheap_clean, aes(x = price_k, fill= no_of_bedrooms)) +
  geom_density() +
  labs(title = "Housing prices in London",
       x = "price (k£)",
       y = "Count") + 
  facet_wrap(~ house_type)


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

#DESCRIPTIVE STATISTICS####
#Descriptives for predictors and outcomes ----------------------------#
# Good news: skimr::skim() above would have just given use these!
# skimr::skim(londonHousing_cheap_clean) 
# summary(londonHousing_cheap_clean) 

# Explore and Summarise data
xtabs(~ house_type + no_of_bedrooms, londonHousing_cheap_clean)

# let's use aggregate to get as many information on the variables demographics depending on the group
#first, let's create a function containing all the information you would like from the data.
#We will use this as the function argument in aggregate
custom_descriptives <- function(x) {
  c(summary(x),  #this will give us mean, min-max, and quartiles
    IQR = IQR(x), 
    median = median(x),
    range = range(x),
    ci_low = round((mean(x) - qt(0.975, df = length(x) - 1) * sd(x) / sqrt(length(x))),2),
    ci_high = round((mean(x) + qt(0.975, df = length(x) - 1) * sd(x) / sqrt(length(x))),2))
}

demographics_by_group <- aggregate( price ~ house_type + no_of_bedrooms, 
                                    londonHousing_cheap_clean,
                                    FUN=custom_descriptives)
#NB: you can add and remove as many demographic information as you want or need.

#LECTURE'S REGRESSION ####
#function to get Mean Squared Error
mse <- function(modelObject) 
  mean(modelObject$residuals^2)

lecture_full <- lm(price_k~house_type+no_of_bedrooms+ area_in_sq_ft, londonHousing_cheap_clean)
summary.lm(lecture_full)

# Another possible way to analyse this would be to transform the number of bedrooms in the house into a factor.
#While this is not usually done with ordinal values, in this context, 
#the numbers could easily be replaced by labels such as "single-bedroom flat" 
#for no_of_bedrooms = 1 and so on so forth.

londonHousing_cheap_clean$no_of_bedrooms <- factor(londonHousing_cheap_clean$no_of_bedrooms )
lecture_full_2fact <- lm(price_k~house_type+no_of_bedrooms+ area_in_sq_ft, londonHousing_cheap_clean)
summary.lm(lecture_full_2fact)


###Extra content for curiosity----
#Now, if we wanted to test whether the mean prices by amount of bedrooms vary 
#following a specific pattern, we could use an a priori polylinear contrast.
#Polylinear contrasts assign values to each level of the IV so that the results 
#provide evidence for different mean patterns, like linear, quadratic or cubic relations.

contrasts(londonHousing_cheap_clean$no_of_bedrooms) # let's check the order of 
#the factors (we want them 
#in crescent order in this case)

contrasts(londonHousing_cheap_clean$no_of_bedrooms) <- contr.poly(4)
lecture_full_lin <- lm(price_k~house_type+no_of_bedrooms+ area_in_sq_ft, londonHousing_cheap_clean)
summary.lm(lecture_full_lin)
#NB: patterns of mean can be visualised here https://i.stack.imgur.com/83mDh.png
#Let's examine whether the model created is the best fit for the data, or whether 
#it simpler models worked better
lecture_1_lin <- lm(price_k~house_type, londonHousing_cheap_clean)
summary.lm(lecture_1_lin)
lecture_2_lin <- lm(price_k~house_type+no_of_bedrooms, londonHousing_cheap_clean)
summary.lm(lecture_2_lin)
anova(lecture_1_lin, lecture_2_lin, lecture_full_lin)#a significant result 
#in this model comparison 
#suggests the model to be 
#a better fit for the data.
##Step-wise model####
#Another way to determine which model is the best is to use the function "step" or "stepAIC" 
#and use your maximal model. the direction indicates how the stepwise comparison is going to run.
step_lm <- step(lecture_full_lin, direction = "both", trace = 2)
step_lm_backward <- stepAIC(lecture_full_lin, direction = "backward", trace = FALSE)
step_lm_forward <- stepAIC(lecture_full_lin, direction = "forward", trace = FALSE)


#Let's look at the MSE
mse(lecture_full)


##a.) NORMALITY####
# Here is a breakdown of the model fit plots
#visual inspection
plot(lecture_full, which = 1) # Residuals vs. Fitted - normality, want dots equally 
# distibuted above and below the horizontal line - want red line straight and horizontal.
plot(lecture_full, which = 2) # QQ Plot - normality
#statistical test 
shapiro.test(lecture_full$residuals)# Shapiro-Wilk

##b.) HOMOGENEITY####
plot(lecture_full, which = 3) # Scale-Location - homogeneity, again want a 
# nice straight horizontal line
car::ncvTest(lecture_full)

###All assumptions####
performance::check_model(lecture_full) 

##Outliers####
plot(lecture_full, which = 4) # Cook's Distance
plot(lecture_full, which = 5) # Leverage
plot(lecture_full, which = 6) # Cook's Dist

##Can you do better?####

#let's see how this changes with the log-transformation
lecture_full_log <- lm(logprice~house_type+no_of_bedrooms+ area_in_sq_ft, londonHousing_cheap_clean)

summary.lm(lecture_full_log)
mse(lecture_full_log)


anova(lecture_full_log,lecture_full_log)
car::vif(lecture_full_log)

####Additional exploration####
#as Andy mentioned, it might be the case that the location of the house heavily impacts the prices of the house
#while this is not a remedy for the assumptions, it would be interesting to check how this changes the total fit of the model
#let's look at the number of NAs in the dataset per location
londonHousing_cheap_clean$location[londonHousing_cheap_clean$location==""]<- NA
table(is.na(londonHousing_cheap_clean$location))
londonHousing_cheap_clean$postal_code[londonHousing_cheap_clean$postal_code==""]<- NA
table(is.na(londonHousing_cheap_clean$postal_code))

#as location has quite a lot of missing values, we will use the postal code.
#However, since the first part of the postal code is the only one convening the meaningful information, we need to split the string into two.

londonHousing_cheap_clean$area<-factor(sub(" .*", " ", x=londonHousing_cheap_clean$postal_code))


lecture_full_new <- lm(price_k~house_type+no_of_bedrooms+ area_in_sq_ft+area, londonHousing_cheap_clean)
summary.lm(lecture_full_new)
lecture_full_log2 <- lm(logprice~house_type+no_of_bedrooms+ area_in_sq_ft+area, londonHousing_cheap_clean)
summary.lm(lecture_full_log2)
mse(lecture_full_new)
mse(lecture_full_log2)

#bear in mind this is only a demonstration as the variable we created has too many levels.
#it might be worth thinking about coding contrasts or regrouping the variables in a more concise fashion (e.g.: inner vs outer, central vs peripheral, east vs west, south vs north)


























#effect sizes####
rsq::rsq.partial(lecture_full)
rsq::rsq.partial(lecture_full_new)