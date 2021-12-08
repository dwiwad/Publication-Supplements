# GTGA Manuscript
# Aknin, Wiwad, & Girme
# Study 1

# Setting the working directory
setwd("/users/dylanwiwad/dropbox/work/GTGA Recip paper/Data and Code/")

# Packages needed
library(foreign) # To read SPSS files
library(psych) # For descriptive statistics
library(plyr) # For descriptive statistics

# Read in the data
study1 <- read.spss("Study 1-National Panel N=501.sav", to.data.frame = TRUE,
                    use.value.labels = FALSE)

# Descriptive statistics -----------------------------------------------------------------------
# Mean age
describe(study1$Age)
# Gender frequency
plyr::count(study1$Gender) # Gender is coded 1 = male, 2 = female

# Main Results ---------------------------------------------------------------------------------
# Proportion having given a motivated gift
plyr::count(study1$GTGAYouGive) # Coded as 1 = Yes (given a motivated gift), 2 = No (have not)
print(c("Proportion saying they have given a motivated gift:", 173/501))

# Proportion having received a motivated gift
plyr::count(study1$GTGAYouRec) # Coded as 1 = No (have not received a motivated gift), 2 = Yes (have)
print(c("Proportion saying they have received a motivated gift:", 312/501))

# Chi-square tests 
# Need to drop the one row that is NA
study1 <- study1[!is.na(study1$GTGAYouRec), ]

# Run the first chi-square test - full sample
chisq.test(table(study1$GTGAYouRec))

# Pull out just the people who report having given a motivated gift
study1_gave <- study1[(study1$GTGAYouGive == 1), ]
# Run the chi-square test
chisq.test(table(study1_gave$GTGAYouRec))

# Pull out just the people who report they have never given a motivated gift
study1_no <- study1[(study1$GTGAYouGive == 2), ]
# Run the chi-square test
chisq.test(table(study1_no$GTGAYouRec))
