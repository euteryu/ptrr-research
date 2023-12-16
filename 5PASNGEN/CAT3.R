## Filename   : k22014863_5PASNGEN_CAT3.R
## Created on : 29/11/23
## Author     : k22014863
## Version    : 1.1
## Purpose    : Data analysis script accompanying Continuous Assessment Task #3
## Note       : Please click +Zoom+ for optimal individual plot viewing. This
##              helps circumvent any 'display list redraw incomplete' errors!

################################################################################

## Chronological overview of how data analysis has been conducted below:
## Task 1.  Define hypotheses and statistical model
## Task 2.  Set up workspace
## Task 3.  Visualise plots
## Task 4.  ANOVA assumptions
## Task 5.  Compute ANOVA
## Task 6.  Interpret
## Task 7.  Planned Contrast

################################################################################

## Task 1.1.  Define hypotheses:
cat("We will test two directional hypotheses about the effect of format on
    perceived chances of winning.
    - H1a: the alternative house edge format will lead to lower perceived chances
           of winning than the original house edge format.
    - H1b: both house edge formats will lead to lower perceived chances of
           winning than the return-to-player format.\n

    We will test a directional hypothesis about the effect of graphical aid.
    - H2: adding a graphical aid to the message will lead to lower perceived
          chances of winning.\n

    We will test a non-directional hypothesis about the interaction between
    Graphical Aid Format and Message Format.
    - H3: there will be an interaction between Graphical Aid Format and
          Message Format, such that the effect of Graphical Aid Format will
          differ in the different conditions of Message Format.")

## Task 1.2.  Define statistical model
cat("We will perform 2x3 independent full factorial ANOVA.
     The manipulated, categorical independent variables are:
     Message Format (alternative house edge, original house edge, and
     return-to-player) and Graphical Aid Format (with or without).
     The dependent variable is:
     participants’ perceived chances of winning.")

################################################################################

## Task 2.1  Set up workspace

## Clean workspace
rm(list=ls())

## Load standard libraries
packages = c("ggplot2", "dplyr", "lsr", "lsmeans", "ggpubr", "tidyr", "tidyverse", "car", "lme4")
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
setwd("C:/Users/minse/Desktop/NPGroup/Psychology/Year2/5PASNGEN/5PASNGEN_CW")
getwd()

## Load data
choices <- read.csv("choices_xp_2324_clean_correct.csv", header = TRUE)

## Look at what we have
choices$format <- as.factor(choices$format)
choices$graf <- as.factor(choices$graf)
levels(choices$format)
levels(choices$graf)

## Task 2.2  Exclusion criterion
## From our 172 total participants, remove those who:
## - gamble regularly (i.e "more than once a week")
## - who failed to provide scores for their perceived chances of winning
##   (i.e remove "NA" values in "proba_judg" column of our data)
minichoices1 <- choices[!(choices$gamb_freq %in% "more than once a week"),
                       c("proba_judg", "format", "graf", "age", "gender", "ethni")
                       ] %>% drop_na("proba_judg", "format", "age")

## Check if cells are balanced by looking at counts in each cell for our two
## factors:
xtabs( ~ format + graf, data = minichoices1)
cat("Our 155 eligible participants were split into 6 survey groups,
    with counts ranging from 24 to 28 per cell.")

## Stratified sampling to balance cell entries
set.seed(2023)
minichoices2 <- minichoices1 %>%
    group_by(format, graf) %>%
    sample_n(24)
xtabs( ~ format + graf, data = minichoices2)
cat("We now have 144 participants split into 6 survey groups,
    balancing the dataset at 24 cell entries each.")

## Descriptive Statistics: gender, ethnicity, mean age
table(minichoices2$gender)
table(minichoices2$ethni)
age_mean <- mean(minichoices2$age)

################################################################################

## Task 3  Draw various plots to visually discern high-level indications of
## variance between the two factors:

## Line plot showing interaction of factors
ggline(minichoices2, x = "format", y = "proba_judg", color = "graf",
       xlab = "Message Format",
       ylab = "Perceived Chances Of Winning (%)",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

## Violin plot and Box plot
ggplot(minichoices2, aes(x=format, y=proba_judg, fill=graf)) + 
  geom_violin(position = position_dodge(1), trim=FALSE) +
  geom_boxplot(position = position_dodge(1), alpha = 0, width=0.09, fill="white") +
  labs(title="Effect of message format and graphical aid on gamblers’ perception",
       x="Message Format",
       y = "Perceived Chances Of Winning") +
  theme_classic()

################################################################################

## Task 4.1  Compute full factorial 2x3 ANOVA
minichoices2.aov <- aov(proba_judg ~ format * graf, data = minichoices2)
## See Task 4 for detailed interpretation, post-assumption check below

## Task 4.2  Effect size
etaSquared(minichoices2.aov)
cat("η2 for message format:   0.066  -->  Medium effect")
cat("η2 for graphical aid :   0.059  -->  Small  effect")
cat("η2 for interaction   : < 0.001  -->  Null   effect")

################################################################################

## Task 5.1  Check independence of observations
## See study design for more detail.
cat("There are different participants in each of the categorical, independent
    groups, with no participant being in more than one group.")

## Task 5.2  Check the homogeneity of variance assumption
plot(minichoices2.aov, 1)
cat("There is no evident relationships between residuals and fitted values
    (mean of each groups), which suggests homogeneity of variances in the
    different treatment groups.")
leveneTest(minichoices2.aov)
cat("Levene's test indicated significance is greater than 0.05
    (F = 1.80, p = 0.12). Therefore there is no evidence to suggest that the
    variance across groups is statistically significantly different.")

## Task 5.3  Check the normality assumption of residuals
plot(minichoices2.aov, 2)
cat("Q-Q plot suggests residuals are bimodally distributed. Typically, this is
    cause for concern as to the appropriateness of employing parametric tests.")
## Extract residuals
aov_residuals <- residuals(object = minichoices2.aov)
## Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
cat("Since we have a small sample size, determining the distribution of
     residuals is important for choosing an appropriate statistical method.
     The Shapiro-Wilk test showed the distribution of residuals departed
     significantly from normality (W = 0.93, p-value < 0.01).\n
     Nonetheless, we will proceed with ANOVA for the scope of this pilot study.
     The reasoning being that residuals will become normallly negligible, as we
     increase our sample size after the pilot study towards the 1380 participant
     number suggested by our power analysis to have at least 80% power to detect
     each of the main effects.")

################################################################################

## Task 6  Interpret results
summary(minichoices2.aov)
cat("I conducted a 2x3 between-subjects ANOVA to estimate the effect of Message
    Format and Graphical Aid Format on participants' Perceived Chances Of Winning
    in gambling. My two factors were: Message Format (alternative house edge, 
    original house edge, and return-to-player) and Graphical Aid Format (with or
    without).\n
    I observed a main effect of Message Format F(2, 141) = 5.24, p = .006
    and a main effect of Graphical Aid Format, F(1, 141) = 9.25, p = .002. There
    was no significant interaction between Message Format and Graphical Aid
    Format, F(2, 24) = 0.04, p = .960.")

################################################################################

## Task 7  Planned Contrasts
## Using planned contrasts to assess significance of differences between pairs
## of group means per significant main effects identified earlier above.
model2 <- lm(proba_judg ~ format * graf, data = minichoices2)
(cell_means <- lsmeans(model2, specs = c("format", "graf")))
contrast_out <- contrast(cell_means,
                         list(
                           AltHouseEdge_vs_OGHouseEdge = c(0,0,0,-1,1,0),
                           HouseEdge_vs_RTP = c(0,0,0,-1,-1,2),
                           Graphical_vs_NoGraphical = c(1,1,1,-1,-1,-1)
                         ))
summary(contrast_out)

cat("Planned contrasts revealed that both house edge formats significantly lead
    to lower perceived chances of winning than the original house edge format,
    t(138) = 2.12, p = .036, and adding a graphical aid to the message
    significantly lead to lower perceived chances of winning, t(138) = 3.04,
    p = .003.\n
    But alternative house edge formats did not significantly lead to 
    lower perceived chances of winning than the original house edge format, 
    t(138) = 1.26, p = .210.")
