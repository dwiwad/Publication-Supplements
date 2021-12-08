#--------------------------------------------------------------------------------------------------------
# Item Analysis - Support for Inequality
# Data from Study 3
#--------------------------------------------------------------------------------------------------------

# Get the file
Data <- read.csv("Study 3 - USA.csv", header = TRUE)


#--------------------------------------------------------------------------------------------------------
# Individual item analyses
#--------------------------------------------------------------------------------------------------------

# Need to reverse code items 1, 2 and 4 first
Data$tol1 <- (8 - Data$tol1)
Data$tol2 <- (8 - Data$tol2)
Data$tol3 <- (8 - Data$tol3)

# Function to recode all the questions; 1-7 becomes 0-7
recode <- function(x){
  x[x == 1] <- 0
  x[x == 2] <- 1
  x[x == 3] <- 2
  x[x == 4] <- 3
  x[x == 5] <- 4
  x[x == 6] <- 5
  x[x == 7] <- 6
  return(x)
}

# Actually use the function on each item
Data$tol1 <- recode(Data$tol1) # Item 3
Data$tol2 <- recode(Data$tol2) # Item 5
Data$tol3 <- recode(Data$tol3) # Item 8
Data$tol4 <- recode(Data$tol4) # Item 10
Data$tol5 <- recode(Data$tol5) # Item 18

# Histograms and descriptives
library(psych)
library(ggplot2)
# Item 1
item1.hist <- ggplot(Data, aes(Data$tol1)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 1", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol1, na.rm = TRUE), sd = sd(Data$tol1, na.rm = TRUE)), colour = "black", size = 1)
item1.hist
describe(Data$tol1)
# Item 2
item2.hist <- ggplot(Data, aes(Data$tol2)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 2", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol2, na.rm = TRUE), sd = sd(Data$tol2, na.rm = TRUE)), colour = "black", size = 1)
item2.hist
describe(Data$tol2)
# Item 3
item3.hist <- ggplot(Data, aes(Data$tol3)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 3", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol3, na.rm = TRUE), sd = sd(Data$tol3, na.rm = TRUE)), colour = "black", size = 1)
item3.hist
describe(Data$tol3)
# Item 4
item4.hist <- ggplot(Data, aes(Data$tol4)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 4", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol4, na.rm = TRUE), sd = sd(Data$tol4, na.rm = TRUE)), colour = "black", size = 1)
item4.hist
describe(Data$tol4)
# Item 4
item5.hist <- ggplot(Data, aes(Data$tol5)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 5", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol5, na.rm = TRUE), sd = sd(Data$tol5, na.rm = TRUE)), colour = "black", size = 1)
item5.hist
describe(Data$tol5)

# Building the Graded Response Model
library(ltm)
graded.model <- grm(Data[c(12,13,14,15,16)])
coef.grm(graded.model, prob = TRUE)
summary(graded.model)

# Get item information for the whole spectrum
# Total information provided by all 9 items together 109.14; Better than before!
information(graded.model, c(-4, 4))

# Information given by each item in the scale, over the whole range
# Again, fantastic. All the items are giving about the same level of information
# Smallest is 10ish, largest is 15ish, most items around 12.
information(graded.model, c(-4, 4), items = 1)
information(graded.model, c(-4, 4), items = 2)
information(graded.model, c(-4, 4), items = 3)
information(graded.model, c(-4, 4), items = 4)
information(graded.model, c(-4, 4), items = 5)


# Item total correlations
# All fantastic, ranging from .83 to .88
library(psychometric)
item.exam(Data, y = NULL, discrim = FALSE)

# NEED TO CONDUCT TESTS OF UNIDIMENSIONALITY IN IRTPro
# COMPOSITING RULE?

#--------------------------------------------------------------------------------------------------------
# Moving into the full Data set - Prepare the data for correlations 
# For now, moving forward as if unidimensional and mean compositing HAVENT DONE UNIDIM. YET 02/23/17
#--------------------------------------------------------------------------------------------------------

# Composite everything
#Check column numbers, which are entered below (instead of the items themselves) to create vectors for each facet
cbind(colnames(Data))

# Inequality attitudes - full (columns 12 to 16)
col.ineq <- c(12,13,14,15,16)
Data$ineq <- rowMeans(Data[,col.ineq], na.rm = TRUE)

ineq.hist <- ggplot(Data, aes(Data$ineq)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Full Ineq Scale", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$ineq, na.rm = TRUE), sd = sd(Data$ineq, na.rm = TRUE)), colour = "black", size = 1)
ineq.hist

# Level of inequality (columns 17 to 20)
Data$lvl4 <- (8 - Data$lvl4)
col.lvl <- c(17,18,19,20)
Data$lvl <- rowMeans(Data[,col.lvl], na.rm = TRUE)

# Inequality is growing (columns 21 to 24)
Data$growth1 <- (8 - Data$growth1)
Data$growth3 <- (8 - Data$growth3)
col.growth <- c(21,22,23,24)
Data$growth <- rowMeans(Data[,col.growth], na.rm = TRUE)

# Inequality is fixable (columns 25 to 31)
Data$fix5 <- (8 - Data$fix5)
Data$fix7 <- (8 - Data$fix7)
col.fix <- c(25,26,27,28,29,30,31)
Data$fix <- rowMeans(Data[,col.fix], na.rm = TRUE)

# Belief in a just world (columns 32 to 49)
col.bjw <- c(32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49)
Data$bjw <- rowMeans(Data[,col.bjw], na.rm = TRUE)

# Support for redistribution (columns 50 to 53)
col.redist <- c(50,51,52,53)
Data$redist <- rowMeans(Data[,col.redist], na.rm = TRUE)

# Beliefs about people in poverty (Warmth and competence) (columns 54 to 58; 59 to 62)
col.comp <- c(57,58,59,60,61)
Data$comp.tot <- rowMeans(Data[,col.comp], na.rm = TRUE)
col.warmth <- c(62,63,64,65)
Data$warmth.tot <- rowMeans(Data[,col.warmth], na.rm = TRUE)

# Wealth guilt (columns 63 to 67)
# Reverse code the last item
Data$wg5 <- (8 - Data$wg5)
col.wg <- c(66,67,68,69,70)
Data$wg <- rowMeans(Data[,col.wg], na.rm = TRUE)

# Empathy - 3 subscales (columns 68 to 73)
# Cog emp
Data$emp1 <- (6 - Data$emp1)
col.cog.emp <- c(71,72)
Data$cog.emp <- rowMeans(Data[,col.cog.emp], na.rm = TRUE)
# Affective Resonance
Data$emp4 <- (6 - Data$emp4)
col.aff.res <- c(73,74)
Data$aff.res <- rowMeans(Data[,col.aff.res], na.rm = TRUE)
# Affective Dissonance
Data$emp5 <- (6 - Data$emp5)
Data$emp6 <- (6 - Data$emp6)
col.aff.dis <- c(75,76)
Data$aff.dis <- rowMeans(Data[,col.aff.dis], na.rm = TRUE)
# Full empathy
col.empathy <- c(71,72,73,74,75,76)
Data$empathy <- rowMeans(Data[,col.empathy], na.rm = TRUE)

# Prosocial Tendencies (columns 74 to 76)
col.PT <- c(77,78,79)
Data$PT <- rowMeans(Data[,col.PT], na.rm = TRUE)

# Free Will (columns 74 to 76)
col.FW <- c(80,81,82,83,84)
Data$FW <- rowMeans(Data[,col.FW], na.rm = TRUE)

# Overclaiming TEST (columns 74 to 76)
col.OC <- c(99,100,101,102,103,104,105,106,107,108,109,110,111,112,113)
Data$OC <- rowMeans(Data[,col.OC], na.rm = TRUE)

# Sum of social desirability
# Recode to 0 and 1
recodeTF <- function(x){
  x[x == 1] <- 1
  x[x == 2] <- 0
  return(x)
}
Data$sd1 <- recodeTF(Data$sd1)
Data$sd2 <-recodeTF(Data$sd2)
Data$sd3 <-recodeTF(Data$sd3)
Data$sd4 <-recodeTF(Data$sd4)
Data$sd5 <-recodeTF(Data$sd5)
Data$sd6 <-recodeTF(Data$sd6)
Data$sd7 <-recodeTF(Data$sd7)
Data$sd8 <-recodeTF(Data$sd8)
Data$sd9 <-recodeTF(Data$sd9)
Data$sd10 <-recodeTF(Data$sd10)
Data$sd11 <-recodeTF(Data$sd11)
Data$sd12 <-recodeTF(Data$sd12)
Data$sd13 <-recodeTF(Data$sd13)

Data$SocDes <- Data$sd1+Data$sd2+Data$sd3+Data$sd4+Data$sd5+Data$sd6+
  Data$sd7+Data$sd8+Data$sd9+Data$sd10+Data$sd11+Data$sd12+Data$sd13

#--------------------------------------------------------------------------------------------------------
# Moving into the full Data set - Correlations 
#-------------------------------------------------------------------------------------------------------- 

# Support for inequality FULL SCALE with each DV
library(Hmisc)

# Age
Data$age <- as.numeric(Data$age)
rcorr(Data$ineq, Data$age, type = "pearson")
rcorr(Data$income, Data$age, type = "pearson")


# ideology - Only response options 1 through 7 (8 and 9 are don't know/other)
ideolData<- subset(Data, ideology <=7)
rcorr(ideolData$ineq, ideolData$ideology, type = "pearson")
# Social issues  - Only response options 1 through 7 (8 and 9 are don't know/other)
socialData <- subset(Data, soc.iss <=7)
rcorr(socialData$ineq, socialData$soc.iss, type = "pearson")
# Economic issues - Only response options 1 through 7 (8 and 9 are don't know/other)
econData <- subset(Data, econ.iss <=7)
rcorr(econData$ineq, econData$econ.iss, type = "pearson")

# Party ID - look at a mean comparison (1 Dem; 2 Ind; 3 Rep; 4 Other)
library(psych)
describeBy(Data$ineq, Data$partyID)
pIDaov <- aov(ineq~partyID, data = Data)
summary(pIDaov)
pairwise.t.test(Data$ineq, Data$partyID, p.adjust="bonferroni") 

# level of ineq (result: less perceived inequality, more support)
rcorr(Data$ineq, Data$lvl, type = "pearson")
# growth in ineq (result: more perceived growth in inequality, less support)
rcorr(Data$ineq, Data$growth, type = "pearson")
# ineq is fixable (result: perceive inequality as un-fixable, more support)
rcorr(Data$ineq, Data$fix, type = "pearson")
# BJW (More BJW, more support)
rcorr(Data$ineq, Data$bjw, type = "pearson")
# support for redistribution (Les you think the gov should/can do something, more support)
rcorr(Data$ineq, Data$redist, type = "pearson")
# Poor folk are competent (Less belief the poor are competent, more support)
rcorr(Data$ineq, Data$comp.tot, type = "pearson")
# Poor folk are warm (Less belief the poor are warm, more support)
rcorr(Data$ineq, Data$warmth.tot, type = "pearson")
# Inequality support and wealth guilt (Less wealth guilt, more support)
rcorr(Data$ineq, Data$wg, type = "pearson")
# Cognitive Empathy (Less cognitive empathy, more support)
rcorr(Data$ineq, Data$cog.emp, type = "pearson")
# Affective Resonance (Less affective resonance, more support)
rcorr(Data$ineq, Data$aff.res, type = "pearson")
# Affective Dissonance (Less affective dissonance, more support)
rcorr(Data$ineq, Data$aff.dis, type = "pearson")
# Empathy (Less empathetic, more support)
rcorr(Data$ineq, Data$empathy, type = "pearson")
# Prosocial Tendencies (Less prosocial tendency, more support)
rcorr(Data$ineq, Data$PT, type = "pearson")
# Income (Higher income, more support)
rcorr(Data$ineq, Data$income, type = "pearson")
# Free Will (Higher belief in free will, more support)
rcorr(Data$ineq, Data$FW, type = "pearson")

# Social desirability (Boo yeah no correlation)
rcorr(Data$ineq, Data$SocDes, type = "pearson")
rcorr(Data$ineq, Data$OC, type = "pearson")

