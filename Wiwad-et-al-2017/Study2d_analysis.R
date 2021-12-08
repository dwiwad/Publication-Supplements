# GTGA Manuscript
# Aknin, Wiwad, & Girme
# Study 2d

# Setting the working directory
setwd("/users/dylanwiwad/dropbox/work/GTGA Recip paper/Data and Code/")

# Packages needed
library(foreign) # To read SPSS files
library(psych) # For descriptive statistics
library(plyr) # For descriptive statistics
library(pwr) # For the power analysis
library(dplyr) # For data manipulation
library(lavaan) # For the mediation models

# Read in the data
study2d <- read.spss("Study 2d.sav", to.data.frame = TRUE,
                     use.value.labels = FALSE)

# Descriptive statistics -----------------------------------------------------------------------
# Age
describe(study2d$Age)
# Gender frequency
plyr::count(study2d$Gender) # Gender is coded 1 = male, 2 = female

# a priori power analysis ---------------------------------------------------------------------
pwr.t.test(n=NULL, d = .20, sig.level = .05, power = .80)

# Main Results ---------------------------------------------------------------------------------
# Willingness to vote
# Descriptives by condition
describeBy(study2d$WillingtoHelp, study2d$Condition)
# ANOVA
summary(aov(WillingtoHelp~Condition, data=study2d))

# satisfaction with vote
# Descriptives by condition
describeBy(study2d$SatisfiedHelping, study2d$Condition)
# ANOVA
summary(aov(SatisfiedHelping~Condition, data=study2d))

# Mediation models
# Model1 - DV Willingness to vote
summary(mediate(y='WillingtoHelp', x='Condition', m='Manipulated',
                data=study2d, n.iter = 5000, alpha = 0.05, std = TRUE,main="Mediation"))

# Model2 - DV Satisfaction with vote
summary(mediate(y='SatisfiedHelping', x='Condition', m='Manipulated',
                data=study2d, n.iter = 5000, alpha = 0.05, std = TRUE,main="Mediation"))


# Change In Closeness
# Descriptives by condition
describeBy(study2d$changeincloseness, study2d$Condition)
# ANOVA
summary(aov(changeincloseness~Condition, data=study2d))
# ANOVA with happiness as a covariate
summary(aov(changeincloseness~Condition+HappyNow, data=study2d))


