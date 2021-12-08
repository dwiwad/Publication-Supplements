# GTGA Manuscript
# Aknin, Wiwad, & Girme
# Study 2c

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
study2c <- read.spss("Study 2c.sav", to.data.frame = TRUE,
                     use.value.labels = FALSE)

# Remove all of the people who were incompletes and dropped out of the study as well as the NAs
study2c <- study2c[(study2c$INComplete == 0), ]

# Descriptive statistics -----------------------------------------------------------------------
# Age
# Age is a factor for whatever reason; convert to numeric
study2c$Agenum <- as.character(study2c$Age)
study2c$Agenum <- as.numeric(study2c$Agenum)
describe(study2c$Agenum)
# Gender frequency
plyr::count(study2c$Gender) # Gender is coded 1 = male, 2 = female

# Sensitivity power analysis ---------------------------------------------------------------------
# An f2 of .01 roughly translates to an f of .12
pwr.f2.test(u=1,v=399, f2=NULL, sig.level=0.05, power=.8)

# Main Results ---------------------------------------------------------------------------------
# NOTE: Actual observed effect sizes were computed using g*power
# Willingness to help
# Descriptives by condition
describeBy(study2c$WILL, study2c$CondGift)
describeBy(study2c$WILL, study2c$Condition_FavorRequest)

# Anova
willing_model <- aov(WILL~CondGift+Condition_FavorRequest+(CondGift*Condition_FavorRequest)+Condition,
            data=study2c, contrasts=contrast)
summary(willing_model)

# Planned contrasts
# Descriptives for just the two relevant conditions
describeBy(study2c$WILL, study2c$Condition)

# Turn the condition into a factor
study2c$Condition <- as.factor(study2c$Condition)
# Apply the  contrast weights; 1 = Gift, favor; 2 = No Gift, Favor
contrast <- list(Condition = c(1, -1, 0, 0))
# Build a linear regression, Willingness predicted by each of the conditions
model = lm(WILL ~ Condition, 
           data = study2c)
# Use the lsmeans function to get the condition means
leastsquare = lsmeans(model, "Condition")
# Run the contrast; remember the weights are 1 and -1, so this is between conditions 1 and 2
contrast(leastsquare, contrast, adjust = "none")

# Quickly just test, run a contrast on the other two conditions
# Descriptives by conds 3 and 4
contrast2 <- list(Condition = c(0, 0, 1, -1))
contrast(leastsquare, contrast2, adjust = "none")


# Satisfaction with helping
# Descriptives by condition
describeBy(study2c$SAT, study2c$CondGift)
describeBy(study2c$SAT, study2c$Condition_FavorRequest)
# Anova
satis_model <- aov(SAT~CondGift+Condition_FavorRequest+(CondGift*Condition_FavorRequest),
                     data=study2c)
summary(satis_model)

# Planned contrasts
# Descriptives
describeBy(study2c$SAT, study2c$Condition)
# Our weights were already  defined above so don't need to redefine them here
# Build a linear regression, Willingness predicted by each of the conditions
model2 = lm(SAT ~ Condition, 
           data = study2c)
# Use the lsmeans function to get the condition means
leastsquare2 = lsmeans(model2, "Condition")
# Run the contrast; remember the weights are 1 and -1, so this is between conditions 1 and 2
contrast(leastsquare2, contrast, adjust = "none")
# And the other contrast
contrast(leastsquare2, contrast2, adjust = "none")


# Mediation models
# Model1 - DV Willingness to help
summary(mediate(y='WILL', x='Condition1and2ONLY', m=c('manicomp', 'friend'),
                data=study2c, n.iter = 5000, alpha = 0.05, std = TRUE,main="Mediation"))


# Model2 - DV Satisfaction with helping
summary(mediate(y='SAT', x='Condition1and2ONLY', m=c('manicomp', 'friend'),
                data=study2c, n.iter = 5000, alpha = 0.05, std = TRUE,main="Mediation"))


# Change in closeness
# Descriptives by condition
describeBy(study2c$ChangeInCloseness, study2c$CondGift)
describeBy(study2c$ChangeInCloseness, study2c$Condition_FavorRequest)
# Anova
CIC_model <- aov(ChangeInCloseness~CondGift+Condition_FavorRequest+(CondGift*Condition_FavorRequest),
                   data=study2c)
summary(CIC_model)



