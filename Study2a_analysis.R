# GTGA Manuscript
# Aknin, Wiwad, & Girme
# Study 2a

# Setting the working directory
setwd("/users/dylanwiwad/dropbox/work/GTGA Recip paper/Data and Code/")

# Packages needed
library(foreign) # To read SPSS files
library(psych) # For descriptive statistics
library(plyr) # For descriptive statistics
library(pwr) # For the power analysis
library(effsize) # For the effect sizes

# Read in the data
study2a <- read.spss("Study 2a.sav", to.data.frame = TRUE,
                    use.value.labels = FALSE)

# Descriptive statistics -----------------------------------------------------------------------
# Gender frequency
plyr::count(study2a$gender) # Gender is coded 1 = male, 2 = female

# Sensitivity power analysis -------------------------------------------------------------------
# Specifying our sample size, 1 numerator and 355 denominator degrees of freedom
# alphe set to .05, power set to ,80
pwr.f2.test(u=1,v=355, f2=NULL, sig.level=0.05, power=.8)
# The outcome, f2 = .02, translates to an f of .15

# Main Results ---------------------------------------------------------------------------------
# Willingness to help
# Descriptives by condition
describeBy(study2a$willing, study2a$Cond)
# t-test
t.test(willing~Cond, data=study2a, var.equal=TRUE)
# Effect size and post-hoc power
d1 <- effsize::cohen.d(study2a$willing~study2a$Cond)
pwr.t.test(n=170, d=d1$estimate, sig.level=.05, power=NULL)

# Satisfaction with helping
# Descriptives by condition
describeBy(study2a$satis, study2a$Cond)
# t-test
t.test(satis~Cond, data=study2a, var.equal=TRUE)
# Effect size and post-hoc power
d2 <- effsize::cohen.d(study2a$satis~study2a$Cond)
pwr.t.test(n=170, d=d2$estimate, sig.level=.05, power=NULL)

# Closeness
# Descriptives by condition
describeBy(study2a$close, study2a$Cond)
# t-test
t.test(close~Cond, data=study2a, var.equal=TRUE)
# Effect size and post-hoc power
d3 <- effsize::cohen.d(study2a$close~study2a$Cond)
pwr.t.test(n=170, d=d3$estimate, sig.leve=.05, power=NULL)

# Positive Affect
# Descriptives by condition
describeBy(study2a$PA, study2a$Cond)
# t-test
t.test(PA~Cond, data=study2a, var.equal=TRUE)
# Effect size and post-hoc power
d4 <- effsize::cohen.d(study2a$PA~study2a$Cond)
pwr.t.test(n=356, d=d3$estimate, sig.leve=.05, power=NULL)


