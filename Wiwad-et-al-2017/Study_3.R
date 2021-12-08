# GTGA Manuscript
# Aknin, Wiwad, & Girme
# Study 3

# Setting the working directory
setwd("/users/dylanwiwad/dropbox/work/GTGA Recip paper/Data and Code/")

# Packages needed
library(foreign) # To read SPSS files
library(psych) # For descriptive statistics
library(plyr) # For descriptive statistics
library(pwr) # For the power analysis
library(nlme) # For the multilevel models
library(dplyr) # To create the pivot table 

# Read in the data
study3 <- read.spss("Study 3- Partner lab study.sav", to.data.frame = TRUE,
                     use.value.labels = FALSE)

# Descriptive statistics -----------------------------------------------------------------------
# Age
describe(study3$Age)
# Gender frequency
plyr::count(study3$Gender) # Gender is coded 1 = male, 2 = female

# a priori power analysis ---------------------------------------------------------------------
pwr.t.test(n=50, d = NULL, sig.level = .05, power = .80)

# Main Results ---------------------------------------------------------------------------------
# Whether or not the participants try to give help
# Descriptives - sums, so converting to 1s and 0s instead of 1s and 2s
study3$TrytoGiveHelp <- ifelse(study3$TrytoGiveHelp == 1, c(0), c(1))

# Get the percentage saying yes to help by condition
# This is just a pivot table that, by condition, sums the amount of people who gave help,
# the number of people in that condition, and then does the proportion
study3 %>%
  group_by(Condition) %>%
  summarize(total_helped = sum(TrytoGiveHelp, na.rm = TRUE), total_n = length(Condition),
            proportion = total_helped / total_n)

# The pairs weren't defined in the dataset, so I'll just define them here in a bit of a hacky way
study3$pairs <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,
                  17,17,18,18,19,19,20,20,21,21,22,22,23,23,24,24,25,25,26,26,27,27,28,28,29,29,
                  30,30,31,31,32,32,33,33,34,34,35,35,36,36,37,37,38,38,39,39,40,40,41,41,42,42)

# z-scores of the relevant DV and condition assignment
study3$help.z  <- scale(study3$TrytoGiveHelp, center = TRUE, scale = TRUE)
study3$cond.z  <- scale(study3$Condition, center = TRUE, scale = TRUE)
study3$hap.z <- scale(study3$Happy, center = TRUE, scale = TRUE)

# Nested model witth z-scores
help_model <- lme(help.z~cond.z, data=study3, random = ~1|pairs, method="ML", na.action="na.omit")
summary(help_model)

# Between subjects ANOVA for the footnote; no nesting
model <- aov(TrytoGiveHelp~Condition, data=study3)
summary(model)

# Checking whether happiness differs by condition
# Nested model
hap_model <- lme(hap.z~cond.z, data=study3, random = ~1|pairs, method="ML", na.action="na.omit")
summary(hap_model)
# Non-nested between subjects ANOVA
model2  <- aov(Happy~Condition, data=study3)
summary(model2)

# Closeness
summary(lme(Closeness~Condition, data=study3, random = ~1|pairs, method="ML", na.action = "na.omit"))
summary(aov(Closeness~Condition, data=study3))

