#--------------------------------------------------------------------------------------------------------
# Item Analysis - Support for Inequality
# Data from Study 2
# Support for inequality items only
# Testing finalized 5-item scale
#--------------------------------------------------------------------------------------------------------

# Get the file
Data <- read.csv("Study 3 - support.scale.only.csv", header = TRUE)

# Keep 1r, 2r, 4r, 5, 6,  
# Delete
Data$tol3 <- NULL
Data$tol7 <- NULL
Data$tol8 <- NULL
Data$tol9 <- NULL

#--------------------------------------------------------------------------------------------------------
# Individual item analyses
#--------------------------------------------------------------------------------------------------------

# Need to reverse code items 1, 2 and 4 first
Data$tol1 <- (8 - Data$tol1)
Data$tol2 <- (8 - Data$tol2)
Data$tol4 <- (8 - Data$tol4)

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
Data$tol1 <- recode(Data$tol1) # Item 5
Data$tol2 <- recode(Data$tol2) # Item 8
Data$tol4 <- recode(Data$tol4) # Item 18
Data$tol5 <- recode(Data$tol5) # Item 3
Data$tol6 <- recode(Data$tol6) # Item 10

# Rename them to be consistent with study 1
library(reshape)
Data <- rename(Data, c(tol1="tol5.r"))
Data <- rename(Data, c(tol2="tol8.r"))
Data <- rename(Data, c(tol4="tol18.r"))
Data <- rename(Data, c(tol5="tol3"))
Data <- rename(Data, c(tol6="tol10"))

# Histograms and descriptives
library(psych)
library(ggplot2)
# Item 1
item1.hist <- ggplot(Data, aes(Data$tol5.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 5", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol5.r, na.rm = TRUE), sd = sd(Data$tol5.r, na.rm = TRUE)), colour = "black", size = 1)
item1.hist
describe(Data$tol5.r)
# Item 2
item2.hist <- ggplot(Data, aes(Data$tol8.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 8", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol8.r, na.rm = TRUE), sd = sd(Data$tol8.r, na.rm = TRUE)), colour = "black", size = 1)
item2.hist
describe(Data$tol8.r)
# Item 4
item4.hist <- ggplot(Data, aes(Data$tol18.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 18", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol18.r, na.rm = TRUE), sd = sd(Data$tol18.r, na.rm = TRUE)), colour = "black", size = 1)
item4.hist
describe(Data$tol18.r)
# Item 5
item5.hist <- ggplot(Data, aes(Data$tol3)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 3", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol3, na.rm = TRUE), sd = sd(Data$tol3, na.rm = TRUE)), colour = "black", size = 1)
item5.hist
describe(Data$tol3)
# Item 6
item6.hist <- ggplot(Data, aes(Data$tol10)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 10", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol10, na.rm = TRUE), sd = sd(Data$tol10, na.rm = TRUE)), colour = "black", size = 1)
item6.hist
describe(Data$tol10)

# Building the Graded Response Model
library(ltm)
graded.model <- grm(Data)
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

#--------------------------------------------------------------------------------------------------------
# Moving into the full Data set - Prepare the data for correlations 
# For now, moving forward as if unidimensional and mean compositing HAVENT DONE UNIDIM. YET 02/23/17
#--------------------------------------------------------------------------------------------------------

# Get the data
fullData <- read.csv("Study 3 - full.questions.csv", header = TRUE)

# Keep 1r, 2r, 4r, 5, 6,  
# Delete
fullData$tol3 <- NULL
fullData$tol7 <- NULL
fullData$tol8 <- NULL
fullData$tol9 <- NULL

# Need to reverse code items 1, 2 and 4 first
fullData$tol1 <- (8 - fullData$tol1)
fullData$tol2 <- (8 - fullData$tol2)
fullData$tol4 <- (8 - fullData$tol4)

# Composite everything
#Check column numbers, which are entered below (instead of the items themselves) to create vectors for each facet
cbind(colnames(fullData))

# Inequality attitudes - full (columns 12 to 16)
col.ineq <- c(12,13,14,15,16)
fullData$ineq <- rowMeans(fullData[,col.ineq], na.rm = TRUE)

ineq.hist <- ggplot(fullData, aes(fullData$ineq)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Full Ineq Scale", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(fullData$ineq, na.rm = TRUE), sd = sd(fullData$ineq, na.rm = TRUE)), colour = "black", size = 1)
ineq.hist

# Level of inequality (columns 17 to 20)
fullData$lvl4 <- (8 - fullData$lvl4)
col.lvl <- c(17,18,19,20)
fullData$lvl <- rowMeans(fullData[,col.lvl], na.rm = TRUE)

# Inequality is growing (columns 21 to 24)
fullData$growth1 <- (8 - fullData$growth1)
fullData$growth3 <- (8 - fullData$growth3)
col.growth <- c(21,22,23,24)
fullData$growth <- rowMeans(fullData[,col.growth], na.rm = TRUE)

# Inequality is fixable (columns 25 to 31)
fullData$fix5 <- (8 - fullData$fix5)
fullData$fix7 <- (8 - fullData$fix7)
col.fix <- c(25,26,27,28,29,30,31)
fullData$fix <- rowMeans(fullData[,col.fix], na.rm = TRUE)

# Belief in a just world (columns 32 to 49)
col.bjw <- c(32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49)
fullData$bjw <- rowMeans(fullData[,col.bjw], na.rm = TRUE)

# Support for redistribution (columns 50 to 53)
col.redist <- c(50,51,52,53)
fullData$redist <- rowMeans(fullData[,col.redist], na.rm = TRUE)

# Beliefs about people in poverty (Warmth and competence) (columns 54 to 58; 59 to 62)
col.comp <- c(54,55,56,57,58)
fullData$comp.tot <- rowMeans(fullData[,col.comp], na.rm = TRUE)
col.warmth <- c(59,60,61,62)
fullData$warmth.tot <- rowMeans(fullData[,col.warmth], na.rm = TRUE)

# Wealth guilt (columns 63 to 67)
# Reverse code the last item
fullData$wg5 <- (8 - fullData$wg5)
col.wg <- c(63,64,65,66,67)
fullData$wg <- rowMeans(fullData[,col.wg], na.rm = TRUE)

# Empathy - 3 subscales (columns 68 to 73)
# Cog emp
fullData$emp1 <- (6 - fullData$emp1)
col.cog.emp <- c(68,69)
fullData$cog.emp <- rowMeans(fullData[,col.cog.emp], na.rm = TRUE)
# Affective Resonance
fullData$emp4 <- (6 - fullData$emp4)
col.aff.res <- c(70,71)
fullData$aff.res <- rowMeans(fullData[,col.aff.res], na.rm = TRUE)
# Affective Dissonance
fullData$emp5 <- (6 - fullData$emp5)
fullData$emp6 <- (6 - fullData$emp6)
col.aff.dis <- c(72,73)
fullData$aff.dis <- rowMeans(fullData[,col.aff.dis], na.rm = TRUE)
# Full empathy
col.empathy <- c(68,69,70,71,72,73)
fullData$empathy <- rowMeans(fullData[,col.empathy], na.rm = TRUE)

# Prosocial Tendencies (columns 74 to 76)
col.PT <- c(74,75,76)
fullData$PT <- rowMeans(fullData[,col.PT], na.rm = TRUE)

#--------------------------------------------------------------------------------------------------------
# Moving into the full Data set - Correlations 
# For now, moving forward as if unidimensional and mean compositing HAVENT DONE UNIDIM. YET 02/23/17
#-------------------------------------------------------------------------------------------------------- 

# Support for inequality FULL SCALE with each DV
library(Hmisc)

# Age
fullData$age <- as.numeric(fullData$age)
rcorr(fullData$ineq, fullData$age, type = "pearson")

# ideology - Only response options 1 through 7 (8 and 9 are don't know/other)
ideolData <- subset(fullData, ideology <=7)
rcorr(ideolData$ineq, ideolData$ideology, type = "pearson")
# Social issues  - Only response options 1 through 7 (8 and 9 are don't know/other)
socialData <- subset(fullData, social.iss <=7)
rcorr(socialData$ineq, socialData$social.iss, type = "pearson")
# Economic issues - Only response options 1 through 7 (8 and 9 are don't know/other)
econData <- subset(fullData, econ.iss <=7)
rcorr(econData$ineq, econData$econ.iss, type = "pearson")

# Party ID - look at a mean comparison (1 Dem; 2 Ind; 3 Rep; 4 Other)
library(psych)
describeBy(fullData$ineq, fullData$partyID)
pIDaov <- aov(ineq~partyID, data = fullData)
summary(pIDaov)
pairwise.t.test(fullData$ineq, fullData$partyID, p.adjust="bonferroni") 


# level of ineq (result: less perceived inequality, more support)
rcorr(fullData$ineq, fullData$lvl, type = "pearson")
# growth in ineq (result: more perceived growth in inequality, less support)
rcorr(fullData$ineq, fullData$growth, type = "pearson")
# ineq is fixable (result: perceive inequality as un-fixable, more support)
rcorr(fullData$ineq, fullData$fix, type = "pearson")
# BJW (More BJW, more support)
rcorr(fullData$ineq, fullData$bjw, type = "pearson")
# support for redistribution (Les you think the gov should/can do something, more support)
rcorr(fullData$ineq, fullData$redist, type = "pearson")
# Poor folk are competent (Less belief the poor are competent, more support)
rcorr(fullData$ineq, fullData$comp.tot, type = "pearson")
# Poor folk are warm (Less belief the poor are warm, more support)
rcorr(fullData$ineq, fullData$warmth.tot, type = "pearson")
# Inequality Support and wealth guilt (Less wealth guilt, more support)
rcorr(fullData$ineq, fullData$wg, type = "pearson")
# Cognitive Empathy (Less cognitive empathy, more support)
rcorr(fullData$ineq, fullData$cog.emp, type = "pearson")
# Affective Resonance (Less affective resonance, more support)
rcorr(fullData$ineq, fullData$aff.res, type = "pearson")
# Affective Dissonance (Less affective dissonance, more support)
rcorr(fullData$ineq, fullData$aff.dis, type = "pearson")
# Empathy (Less empathetic, more support)
rcorr(fullData$ineq, fullData$empathy, type = "pearson")
# Prosocial Tendencies (Less prosocial tendency, more support)
rcorr(fullData$ineq, fullData$PT, type = "pearson")
# Income (Higher income, more support)
rcorr(fullData$ineq, fullData$income, type = "pearson")

# Getting a correlation table just for these last variables
x <- fullData[80:81]
y <- fullData[82:83]

rcorr(x,y, type = "pearson")
Data80.98 <- subset(fullData, select=c(80:98))
cor80.98 <- as.data.frame(cor(Data80.98, use="complete.obs"))
