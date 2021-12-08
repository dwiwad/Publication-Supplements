# GTGA Manuscript
# Aknin, Wiwad, & Girme
# Study 2b

# Setting the working directory
setwd("/users/dylanwiwad/dropbox/work/GTGA Recip paper/Data and Code/")

# Packages needed
library(foreign) # To read SPSS files
library(psych) # For descriptive statistics
library(plyr) # For descriptive statistics
library(pwr) # For the power analysis

# Read in the data
study2b <- read.spss("Study 2b.sav", to.data.frame = TRUE,
                     use.value.labels = FALSE)

# Remove all of the people who were incompletes and dropped out of the study as well as the NAs
study2b <- study2b[(study2b$IncompleteDropOut == 0), ]
study2b <- study2b[!is.na(study2b$IncompleteDropOut), ] 

# Descriptive statistics -----------------------------------------------------------------------
# Age
describe(study2b$Age)
# Gender frequency
plyr::count(study2b$Gender) # Gender is coded 1 = male, 2 = female
# On campus versus online recruitment frequencies
plyr::count(study2b$CollectionSite) # Coded as 1 = On campus, 2 = Online

# A priori power analysis ---------------------------------------------------------------------
# An f2 of .0099 roughly translates to an f of .09
pwr.f2.test(u=1,v=NULL, f2=.0099, sig.level=0.05, power=.8)

# Main Results ---------------------------------------------------------------------------------
# NOTE: Actual observed effect sizes were computed using g*power
# Willingness to help
# Descriptives by condition
describeBy(study2b$WillingtoHelp, study2b$Condition_Target)
describeBy(study2b$WillingtoHelp, study2b$Condition_Gift)
# Anova
summary(aov(WillingtoHelp~Condition_Gift+Condition_Target+(Condition_Gift*Condition_Target),
            data=study2b))

# Satisfaction with Helping
# Descriptives by condition
describeBy(study2b$SatisfiedHelp, study2b$Condition_Target)
describeBy(study2b$SatisfiedHelp, study2b$Condition_Gift)
# Anova
summary(aov(SatisfiedHelp~Condition_Gift+Condition_Target+(Condition_Gift*Condition_Target),
            data=study2b))

# Closeness
# Descriptives by condition
describeBy(study2b$IOS, study2b$Condition_Target)
describeBy(study2b$IOS, study2b$Condition_Gift)
# Anova
summary(aov(IOS~Condition_Gift+Condition_Target+(Condition_Gift*Condition_Target),
            data=study2b))

# Subjective Happiness Scale (SHS)
# Descriptives by condition
describeBy(study2b$shs, study2b$Condition_Target)
describeBy(study2b$shs, study2b$Condition_Gift)
# Anova
summary(aov(shs~Condition_Gift+Condition_Target+(Condition_Gift*Condition_Target),
            data=study2b))

# Positive Affect
# Descriptives by condition
describeBy(study2b$PA, study2b$Condition_Target)
describeBy(study2b$PA, study2b$Condition_Gift)
# Anova
summary(aov(PA~Condition_Gift+Condition_Target+(Condition_Gift*Condition_Target),
            data=study2b))







