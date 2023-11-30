# Script name:  PTRR_Research_Report.R
# Created on:   05/04/2023  
# Author:       Minseok Ryu
# Version:      1.0
# Purpose:      Data Analysis
# Notes:        See Week 9 Worksheet

### A1.1 set-up
# initialise libraries
library(tidyverse)
library(knitr)
library(pwr)
library(BayesFactor)

# 2022: 52 participants;  2023: 38 participants
Encode_22 <- read.csv("Coglab29LevelsOfProcessingEncodingData2022.csv")
Encode_23 <- read.csv("Coglab29LevelsOfProcessingEncodingData2023.csv")
Recall_22 <- read.csv("Coglab29LevelsOfProcessingRecallData2022.csv")
Recall_23 <- read.csv("Coglab29LevelsOfProcessingRecallData2023.csv")

Encode_22_23 <- dplyr::bind_rows(Encode_22, Encode_23)
Recall_22_23 <- dplyr::bind_rows(Recall_22, Recall_23)

# View(Encode_22_23)
# View(Recall_22_23)

##################
calculate_z_score <- function(data_heading) {
  z_score = abs(data_heading - mean(data_heading)) / sd(data_heading)
  return(z_score)
}

Recall_22_23$Letter_Zscore = calculate_z_score(Recall_22_23$Letter)
Recall_22_23$Rhyme_Zscore = calculate_z_score(Recall_22_23$Rhyme)
Recall_22_23$Semantic_Zscore = calculate_z_score(Recall_22_23$Semantic)

# create a new dataframe that contains only those rows 
# that have a |z-score| of below 3
Recall_22_23.new <- subset(Recall_22_23, Recall_22_23$Letter_Zscore < 3)
Recall_22_23.new <- subset(Recall_22_23, Recall_22_23$Rhyme_Zscore < 3)
Recall_22_23.new <- subset(Recall_22_23, Recall_22_23$Semantic_Zscore < 3)

# check the new dataset
# UPDATE: seems S10 from 2023 cohort is only outlier!
dim(Recall_22_23.new)

# View(Recall_22_23.new)

############################
# Shapiro-Wilk Test of normality for one variable (univariate)
# Threshold: 0.05
shapiro.test(Recall_22_23.new$Letter) # => 0.1931 > 0.05
shapiro.test(Recall_22_23.new$Rhyme) # => 0.06163 > 0.05
shapiro.test(Recall_22_23.new$Semantic) # => 1.052e-05 < 0.05

`%+%` = function(x,y) return(paste0(x,y))
cat("\nFrom the console output, the p-values for Letter and Rhyme > 0.05 " %+%
    "implying that " %+%
    "the distribution of the data are not significantly different from " %+%
    "normal distribution. In other words, we can assume the normality.")


############################
## paired t-test
# Assumption 1: Are the two samples paired? 
#   Yes, since the data have been collected from same sample of participants
#   tested on the two criterions of interest.
# Assumption 2: Is this a large sample?
#   Yes. n = 89 > 30

n_Recall_22_23.new = dim(Recall_22_23.new)[1]

## t-test: Semantic VS Rhyme
t_Recall_22_23.new.Semantic_Rhyme <- stats::t.test(
  Recall_22_23.new$Semantic,
  Recall_22_23.new$Rhyme,
  paired = TRUE,
  alternative = "greater"
)
t_Recall_22_23.new.Semantic_Rhyme
# => t = 12.733
# => df = 88
# => p-value = 2.2e-16
# => 95% conf.int = [0.1763323, Inf]
# => estimate; mean of the differences = 0.202809

cat("\nA t-test of performance differences in memory recall between Semantic " %+%
      "and Rhyme, in a sample of ",
    n_Recall_22_23.new," students, revealed: t = ",
    format(round(t_Recall_22_23.new.Semantic_Rhyme$statistic, 2), n_Recall_22_23.new = 2),
    ", df = ", format(round(t_Recall_22_23.new.Semantic_Rhyme$parameter, 2), n_Recall_22_23.new = 2),
    ", p = ", format(round(t_Recall_22_23.new.Semantic_Rhyme$p.value, 3), n_Recall_22_23.new = 3),
    sep = "")


## t-test: Rhyme VS Letter
t_Recall_22_23.new.Rhyme_Letter <- stats::t.test(
  Recall_22_23.new$Rhyme,
  Recall_22_23.new$Letter,
  paired = TRUE,
  alternative = "greater"
)
t_Recall_22_23.new.Rhyme_Letter
# => t = 10.343
# => df = 88
# => p-value = 2.2e-16
# => 95% conf.int = [0.1532397, Inf]
# => estimate; mean of the differences = 0.1825843

cat("\nA t-test of performance differences in memory recall between Rhyme " %+%
    "and Letter, in a sample of ",
    n_Recall_22_23.new," students, revealed: t = ",
    format(round(t_Recall_22_23.new.Rhyme_Letter$statistic, 2), n_Recall_22_23.new = 2),
    ", df = ", format(round(t_Recall_22_23.new.Rhyme_Letter$parameter, 2), n_Recall_22_23.new = 2),
    ", p = ", format(round(t_Recall_22_23.new.Rhyme_Letter$p.value, 3), n_Recall_22_23.new = 3),
    sep = "")


############################
## Effect Size
# Cohen's d: characterises the effect size by relating the mean difference
# to variability, similar to a signal-to-noise ratio.
#  A large Cohen's d indicates the mean difference (effect size = signal) is
# large compared to the variability (noise).
cohens_d <- function(samp_1, samp_2) {             
  # First compute the mean difference
  mean_diff <- mean(samp_1) - mean(samp_2)
  # Then compute the pooled standard deviation
  pool_sd <- sqrt((stats::sd(samp_1)^2 + stats::sd(samp_2)^2) / 2)
  # Now you can compute Cohen's d
  d <- mean_diff / pool_sd
  # Show the result as output: without this, you will only be able to see the
  # result if you save it to a variable and then call that variable
  return(d)
}


d_Recall_22_23.new.Semantic_Rhyme <- cohens_d(
  Recall_22_23.new$Semantic,
  Recall_22_23.new$Rhyme
)
d_Recall_22_23.new.Semantic_Rhyme # => 1.399449 > 0.8 => Large effect size


d_Recall_22_23.new.Rhyme_Letter <- cohens_d(
  Recall_22_23.new$Rhyme,
  Recall_22_23.new$Letter
)
d_Recall_22_23.new.Rhyme_Letter # => 0.9781913 > 0.8 => Large effect size


#####################################
# Effect size r
# r = sqrt(t^2/(t^2 + df))
effect_size_r <- function(t, df) {
  # Compute r
  r <- sqrt(t^2/(t^2 + df))
  # Return r to see it in the console
  return(r)
}

r_Recall_22_23.new.Semantic_Rhyme = effect_size_r(t_Recall_22_23.new.Semantic_Rhyme$statistic, t_Recall_22_23.new.Semantic_Rhyme$parameter)
r_Recall_22_23.new.Rhyme_Letter = effect_size_r(t_Recall_22_23.new.Rhyme_Letter$statistic, t_Recall_22_23.new.Rhyme_Letter$parameter)

r_Recall_22_23.new.Semantic_Rhyme # => 0.8051079 
r_Recall_22_23.new.Rhyme_Letter # => 0.7407303


# Report Cohen's d and r
cat("\nEffect sizes for the Rhyme-Letter sample are: Cohen's d = ",
    round(d_Recall_22_23.new.Rhyme_Letter, 2), ", r = ", round(r_Recall_22_23.new.Rhyme_Letter, 2), sep = "" )
# => Cohen's d = 0.98, r = 0.74

cat("\nEffect sizes for the Semantic-Rhyme sample are: Cohen's d = ",
    round(d_Recall_22_23.new.Semantic_Rhyme, 2), ", r = ", round(r_Recall_22_23.new.Semantic_Rhyme, 2), sep = "" )
# => Cohen's d = 1.4, r = 0.81

######################################
########### Visualisation ############
######################################
a = Recall_22_23.new$Letter
b = Recall_22_23.new$Rhyme
c = Recall_22_23.new$Semantic

ggplot(Recall_22_23.new) + geom_violin(aes(x = 'Letter', y = a, fill = 'Letter'), alpha = .6) + 
  geom_violin(aes(x = 'Rhyme', y = b, fill = 'Rhyme'), alpha = .6)  + 
  geom_violin(aes(x = 'Semantic', y = c, fill = 'Semantic'), alpha = .6) +
  geom_boxplot(aes(x = 'Letter', y = a), width = 0.4, alpha = .3) + 
  geom_boxplot(aes(x = 'Rhyme', y = b), width = 0.4, alpha = .3)  + 
  geom_boxplot(aes(x = 'Semantic', y = c), width = 0.4, alpha = .3) +
  geom_point(aes(x = 'Letter', y = a), position = "jitter", alpha = .2) + 
  geom_point(aes(x = 'Rhyme', y = b), position = "jitter", alpha = .2)  + 
  geom_point(aes(x = 'Semantic', y = c), position = "jitter", alpha = .2) +
  labs(x="Performance in Phase II (Memory Test)", y="Proportion of Words Correctly Recalled", 
       fill="Key")

cat("\nViolin plots for all 3 conditions drawn!!!")

#####################################
####### Power Analysis ##############
#####################################
# => pwr.t.test(d, power, type, alternative)
# d           : effect size of interest
# power       : minimum power we want to achieve (80% minimum recommended
#                 in general in our field)
# type        : c("two.sample", "one.sample", "paired")
# alternative : c("two.sided", "less", "greater")

pow = .8


# To avoid code repetition, create list of different effect sizes of interest
ds <- c(d_Recall_22_23.new.Rhyme_Letter, d_Recall_22_23.new.Semantic_Rhyme)
# Then initialize an empty object
min_ns <- NULL

##The sample size needed for a large (`d`) effect size at a power of `pwr` is `n` (per group).
for (i in 1:length(ds)) {
  # Run pwr.t.test() for each value in your list of effect size
  pwr_object <- pwr::pwr.t.test(
    # Tell pwr.t.test() where to find each of the values to use using indexing
    # on i
    d = ds[i], power = pow,
    type = "two.sample", alternative = "two.sided"
  )
  # Extract the n from the power analysis object and add it to the list, again
  # using ceiling() to round *up*
  min_ns[i] <- ceiling(pwr_object$n)
}


# Note the use of ceiling below, instead of round, as this round *up* to the
# nearest integer, rather than rounding to the closest one!
cat("\nThe sample size needed for the Rhyme-Letter (",
    ds[1], ") effect size at a power of ",
    pow, " is ", ceiling(min_ns[1]), " (per group).", sep = '')
# => N = 18

cat("\nThe sample size needed for the Semantic-Rhyme (",
    ds[2], ") effect size at a power of ", pow, " is ",
    ceiling(min_ns[2]), " (per group).", sep = '')
# => N = 10


#############################
### Bar Chart ###
#########

mean.enc.letter = mean(Encode_22_23$Letter)
mean.enc.rhyme = mean(Encode_22_23$Rhyme)
mean.enc.semantic = mean(Encode_22_23$Semantic)

mean.rec.letter = mean(Recall_22_23.new$Letter)
mean.rec.rhyme = mean(Recall_22_23.new$Rhyme)
mean.rec.semantic = mean(Recall_22_23.new$Semantic)
mean.rec.lure = mean(Recall_22_23.new$Lure)

Encode_22_23.means = c(mean.enc.letter, mean.enc.rhyme, mean.enc.semantic)
Recall_22_23.new.means = c(mean.rec.letter, mean.rec.rhyme, mean.rec.semantic, mean.rec.lure)

table(Encode_22_23.means)
table(Recall_22_23.new.means)

barplot(Encode_22_23.means,
        xlab="Encoding",
        ylab="Proportion Correct",
        border="red",
        col="blue",
        density=10
)

barplot(Recall_22_23.new.means,
        xlab="Encoding",
        ylab="Proportion Correct",
        border="red",
        col="orange",
        density=10
)


###########################
####### Bar Chart #########
###########################

data_22_23 <- read.table(text="
LoP,Phase,Proportion_Correct
Letter Processing,I Encoding,0.926966292134832
Letter Processing,II Test,0.485955056179775
Rhyme Processing,I Encoding,0.975842696629214
Rhyme Processing,II Test,0.668539325842697
Semantic Processing,I Encoding,0.995505617977528
Semantic Processing,II Test,0.871348314606742
Lure,Lures,0.853558050561798
", header=TRUE, sep=",")

ggplot(data_22_23, aes(x=Phase, y=Proportion_Correct, fill=LoP)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle = 90))

# sd(Recall_22_23.new$Semantic)
