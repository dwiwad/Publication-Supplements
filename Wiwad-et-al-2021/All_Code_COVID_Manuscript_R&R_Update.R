# ----------------------------------------------------------------------------------------------------------------
# Analysis of COVID-19 Longitudinal Survey
# n = 453
# Time 1 data collection on April 24th and 25th, 2019
# Time 2 data collection on May 11th - May 15th, 2020
# June 18, 2020
# ----------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------
# 
# DATA CLEANING AND MERGING
#
# ----------------------------------------------------------------------------------------------------------------

# Set the working directory and grab the data
setwd("~/Dropbox/work/postdoc/COVID Longitudinal/Writing/Docs for OSF/")
data <- read.csv("Time_1_data.csv", header=T)

# Load Required Packages
library(plyr)
library(psych)
library(Hmisc)
library(lavaan)
library(tidyr)
library(dplyr)
library(devtools)
library(gghalves)
library(ggplot2)

# Do all the compositing ----------------------------------------------------------------------------------------
# Reverse code the SEIS first 3, higher scores means more support for inequality
data$seis1 <- 8 - data$seis1
data$seis2 <- 8 - data$seis2
data$seis3 <- 8 - data$seis3

col.seis <- c(17:21)
data$seis <- rowMeans(data[,col.seis], na.rm = TRUE)

# Redistribution
col.redist <- c(22:25)
data$redist <- rowMeans(data[,col.redist], na.rm = TRUE)

# Attributions for poverty
col.gbpdisp <- c(26:29)
col.gbpsit <- c(30:37)
data$disp <- rowMeans(data[,col.gbpdisp], na.rm = TRUE)
data$sit <- rowMeans(data[,col.gbpsit], na.rm = TRUE)

# Belief in a just world
col.bjw <- c(38:55)
data$bjw <- rowMeans(data[,col.bjw], na.rm = TRUE)

# Social Dominance Orientation
data$sdo3 <- 8 - data$sdo3
data$sdo4 <- 8 - data$sdo4
data$sdo7 <- 8 - data$sdo7
data$sdo8 <- 8 - data$sdo8

col.sdo <- c(57:64)
data$sdo <- rowMeans(data[,col.sdo], na.rm=TRUE)


# Bring in the T2 Data
# Set the working directory and grab the data2
setwd("~/Dropbox/work/postdoc/COVID Longitudinal/Writing/Docs for OSF/")
data2 <- read.csv("Time_2_data.csv", header=T)


# Do all the compositing ----------------------------------------------------------------------------------------
# Reverse code the SEIS first 3, higher scores means more support for inequality
data2$seis1 <- 8 - data2$seis1
data2$seis2 <- 8 - data2$seis2
data2$seis3 <- 8 - data2$seis3

col.seis <- c(16:20)
data2$seis <- rowMeans(data2[,col.seis], na.rm = TRUE)

# Redistribution
col.redist <- c(21:24)
data2$redist <- rowMeans(data2[,col.redist], na.rm = TRUE)

# Attributions for poverty
col.gbpdisp <- c(26:29)
col.gbpsit <- c(30:37)
data2$disp <- rowMeans(data2[,col.gbpdisp], na.rm = TRUE)
data2$sit <- rowMeans(data2[,col.gbpsit], na.rm = TRUE)

# Belief in a just world
col.bjw <- c(52:70)
data2$bjw <- rowMeans(data2[,col.bjw], na.rm = TRUE)

# Social Dominance Orientation
data2$sdo3 <- 8 - data2$sdo3
data2$sdo4 <- 8 - data2$sdo4
data2$sdo7 <- 8 - data2$sdo7
data2$sdo8 <- 8 - data2$sdo8

col.sdo <- c(72:79)
data2$sdo <- rowMeans(data2[,col.sdo], na.rm=TRUE)

# Belief that the poor have been impacted by COVID-19
data2$poor3 <- 8 - data2$poor3
data2$poor4 <- 8 - data2$poor4

col.poor <- c(106:109)
data2$poor_covid <- rowMeans(data2[,col.poor], na.rm=TRUE)

# Merge the two datasets by participant ID
merged <- merge(data, data2, by="PROLIFIC_PID")

# Drops the three people who did not actually complete the survey
merged <- merged[which(!is.na(merged$disp.x)),]

# composite economic system justification
merged$esjt_4 <- 8 - merged$esjt_4
merged$esjt_6 <- 8 - merged$esjt_6
merged$esjt_8 <- 8 - merged$esjt_8
merged$esjt_10 <- 8 - merged$esjt_10
merged$esjt_13 <- 8 - merged$esjt_13
merged$esjt_15 <- 8 - merged$esjt_15
merged$esjt_17 <- 8 - merged$esjt_17

col.esjt <- c(165:181)
merged$esjt <- rowMeans(merged[,col.esjt], na.rm = TRUE)

# Poor-specific support for redistribution
col.poor_redist <- c(133:136)
merged$poor_redist <- rowMeans(merged[,col.poor_redist], na.rm = TRUE)

# Composite Willingness to help the poor
col.helppoor <- c(130:132)
merged$help_poor <- rowMeans(merged[,col.helppoor], na.rm = TRUE)

# Political ideology - set party ID
merged$party <- -1
merged$party[which(merged$pid == 3 | merged$pid_push == 2)] <- 1

# Median split income
psych::describe(merged$income.y)
merged$inc_split <- -1
merged$inc_split[which(merged$income.y >= 6)] <- 1

# Personal impact of covid
cols <- c(182, 184:186)
merged$self_cov <- rowMeans(merged[,cols], na.rm = TRUE)

# Median split on self impact
psych::describe(merged$self_cov)
merged$self_cov_split <- -1
merged$self_cov_split[which(merged$self_cov > 3.5)] <- 1

# Median split on age
merged$age_split <- -1
merged$age.y <- as.numeric(as.character(merged$age.y))
merged$age_split[which(merged$age.y > 36)] <- 1

# ----------------------------------------------------------------------------------------------------------------
# 
# DESCRIPTIVE STATISTICS
#
# ----------------------------------------------------------------------------------------------------------------

# Descriptive statistics for each key variable
psych::describe(merged$disp.x) # Dispositional attributions at Time 1
psych::describe(merged$disp.y) # Dispositional attributions at Time 2
psych::describe(merged$sit.x) # Situational attributions at Time 1
psych::describe(merged$sit.y) # Situational attributions at Time 2
psych::describe(merged$seis.x) # Support for economic inequality at Time 1
psych::describe(merged$seis.y) # Support for economic inequality at Time 2
psych::describe(merged$redist.x) # Support for redistribution at Time 1
psych::describe(merged$redist.y) # Support for redistribution at Time 2
psych::describe(merged$poor_covid) # Belief that the poor have been impacted by COVID-19 at Time 2
psych::describe(merged$help_poor) # Willingness to personally help the poor at Time 2

# Extra variables that are not part of the main models
psych::describe(merged$bjw.x) # Belief in a just world at Time 1
psych::describe(merged$bjw.y) # Belief in a just world at Time 2
psych::describe(merged$sdo.x) # Social Dominance Orientation - Egalitarianism at Time 1
psych::describe(merged$sdo.y) # Social Dominance Orientation - Egalitarianism at Time 2
psych::describe(merged$esjt) # Economic System Justification at Time 2
psych::describe(merged$poor_redist) # Poverty specific support for redistribution at Time 2

# Descriptives on single-item COVID attitudes and emotions towards the poor
psych::describe(merged$covid_1) # Personal health
psych::describe(merged$covid_2) # The US economy
psych::describe(merged$covid_3) # Personal financial situation
psych::describe(merged$covid_4) # Day-to-day life
psych::describe(merged$covid_5) # Family and loved ones
psych::describe(merged$covid_10) # Financial situation of average Americans
psych::describe(merged$covid_11) # Financial situation of poor Americans
psych::describe(merged$covid_12) # Financial situation of rich Americans

psych::describe(merged$sym_poor_11) # Angry with the poor
psych::describe(merged$sym_poor_1) # Annoyed with the poor
psych::describe(merged$sym_poor_2) # Mad at the poor
psych::describe(merged$sym_poor_3) # Irritated with the poor
psych::describe(merged$sym_poor_4) # Sorry for the poor
psych::describe(merged$sym_poor_5) # Sympathy for the poor
psych::describe(merged$sym_poor_6) # Concern for the poor

# Cronbach's Alpha reliability for each measure
# Key Measures - Time 1
psych::alpha(merged[,c("att5", "att6", "att7", "att8", "att9", "att10", "att11")]) # Situational attributions for poverty 
psych::alpha(merged[,c("att1", "att2", "att3", "att4")]) # Dispositional attributions for poverty
psych::alpha(merged[,c("seis1.x", "seis2.x", "seis3.x", "seis4.x", "seis5.x")]) # Support for inequality
psych::alpha(merged[,c("redist1", "redist2", "redist3", "redist4")]) # Support for redistribution

# Additional Measures - Time 1
psych::alpha(merged[,c(38:55)]) # Belief in a just world
psych::alpha(merged[,c(57:64)]) # Social Dominance Orientation - Egalitarianism

# Key Measures - Time 2
psych::alpha(merged[,c("atts5", "atts6", "atts7", "atts8", "atts9", "atts10", "atts11")]) # Situational attributions for poverty
psych::alpha(merged[,c("atts1", "atts2", "atts3", "atts4")]) # Dispositional attributions for poverty
psych::alpha(merged[,c("seis1.y", "seis2.y", "seis3.y", "seis4.y", "seis5.y")]) # Support for inequality
psych::alpha(merged[,c("redist_1", "redist_2", "redist_3", "redist_4")]) # Support for redistribution
psych::alpha(merged[,c("poor1", "poor2", "poor3", "poor4")]) # Beliefs about COVID impacting the poor
psych::alpha(merged[,c(130:132)]) # Willingness to help the poor

# Additional Measures - Time 2
psych::alpha(merged[,c(137:155)]) # Belief in a just world
psych::alpha(merged[,c(157:164)]) # Social dominance orientation - egalitarianism

psych::alpha(merged[,c(165:181)]) # Economic system justification
psych::alpha(merged[,c(133:136)]) # Poverty specific support for redistribution

# ----------------------------------------------------------------------------------------------------------------
# 
# LATENT CHANGE SCORE MODELS
#
# ----------------------------------------------------------------------------------------------------------------

# Standardive the non-change variables
merged$att5z <- scale(merged$att5, scale=T, center = T)
merged$att6z <- scale(merged$att6, scale=T, center = T)
merged$att7z <- scale(merged$att7, scale=T, center = T)
merged$att8z <- scale(merged$att8, scale=T, center = T)
merged$att9z <- scale(merged$att9, scale=T, center = T)
merged$att10z <- scale(merged$att10, scale=T, center = T)
merged$att11z <- scale(merged$att11, scale=T, center = T)
merged$att12z <- scale(merged$att12, scale=T, center = T)
merged$atts5z <- scale(merged$atts5, scale=T, center = T)
merged$atts6z <- scale(merged$atts6, scale=T, center = T)
merged$atts7z <- scale(merged$atts7, scale=T, center = T)
merged$atts8z <- scale(merged$atts8, scale=T, center = T)
merged$atts9z <- scale(merged$atts9, scale=T, center = T)
merged$atts10z <- scale(merged$atts10, scale=T, center = T)
merged$atts11z <- scale(merged$atts11, scale=T, center = T)
merged$atts12z <- scale(merged$atts12, scale=T, center = T)
merged$help_poor_1z <- scale(merged$help_poor_1, scale=T, center = T)
merged$help_poor_2z <- scale(merged$help_poor_2, scale=T, center = T)
merged$help_poor_3z <- scale(merged$help_poor_3, scale=T, center = T)
merged$poor1z <- scale(merged$poor1, scale=T, center = T)
merged$poor2z <- scale(merged$poor2, scale=T, center = T)
merged$poor3z <- scale(merged$poor3, scale=T, center = T)
merged$poor4z <- scale(merged$poor4, scale=T, center = T)



# ----------------------------------------------------------------------------------------------------------------
# Four main models from the text
# ----------------------------------------------------------------------------------------------------------------

# Latent Change Score Model with dispositional attributions for poverty - Corresponds to Figure 1a
mod <- '
dispT1 =~ 1*att1 + att2 + att3 + att4
dispT1 ~~ dispT1
att1 ~~ att1
att2 ~~ att2
att3 ~~ att3
att4 ~~ att4

dispT2 =~ 1*atts1 + atts2 + atts3 + atts4
dispT2 ~~ 0*dispT2
atts1 ~~ atts1
atts2 ~~ atts2
atts3 ~~ atts3
atts4 ~~ atts4

att1 ~~ atts1
att2 ~~ atts2
att3 ~~ atts3
att4 ~~ atts4

poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

dispT2 ~ 1*dispT1
ddisp =~ 1*dispT2
ddisp ~ 1
dispT1 ~ 1
dispT2 ~ 0*1

ddisp ~~ ddisp
ddisp ~~ dispT1
ddisp ~ poor_cov
help ~ ddisp
'

fitDISP <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitDISP, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Fitting across groups - ideology, income
fitDISP <- lavaan(mod, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitDISP, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitDISP <- lavaan(mod, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitDISP, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Latent Change Score Model with situational attributions for poverty - Corresponds to Figure 1b
mod <- '
sitT1 =~  1*att5 + att6 + att7 + att8 + att9 + att10 + att11 + att12
sitT1 ~~ sitT1
att5 ~~ att5
att6 ~~ att6
att7 ~~ att7
att8 ~~ att8
att9 ~~ att9
att10 ~~ att10
att11 ~~ att11
att12 ~~ att12

sitT2 =~ 1*atts5 + atts6 + atts7 + atts8 + atts9 + atts10 + atts11 + atts12
sitT2 ~~ 0*sitT2
atts5 ~~ atts5
atts6 ~~ atts6
atts7 ~~ atts7
atts8 ~~ atts8
atts9 ~~ atts9
atts10 ~~ atts10
atts11 ~~ atts11
atts12 ~~ atts12

poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

att5 ~~ atts5
att6 ~~ atts6
att7 ~~ atts7
att8 ~~ atts8
att9 ~~ atts9
att10 ~~ atts10
att11 ~~ atts11
att12 ~~ atts12

sitT2 ~ 1*sitT1
dsit =~ 1*sitT2
dsit ~ 1
sitT1 ~ 1
sitT2 ~ 0*1

dsit ~~ dsit
dsit ~~ sitT1
dsit ~ poor_cov
help ~ dsit
'

fitSIT <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSIT, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Fitting across groups - ideology, income, self impact?
fitSIT <- lavaan(mod, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSIT, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitSIT <- lavaan(mod, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSIT, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Latent Change Score Model with support for inequality - Corresponds to Figure 1c
mod <- '
seisT1 =~ 1*seis1.x + seis2.x + seis3.x + seis4.x + seis5.x
seisT1 ~~ seisT1
seis1.x ~~ seis1.x
seis2.x ~~ seis2.x
seis3.x ~~ seis3.x
seis4.x ~~ seis4.x
seis5.x ~~ seis5.x

seisT2 =~ 1*seis1.y + seis2.y + seis3.y + seis4.y + seis5.y
seisT2 ~~ 0*seisT2
seis1.y ~~ seis1.y
seis2.y ~~ seis2.y
seis3.y ~~ seis3.y
seis4.y ~~ seis4.y
seis5.y ~~ seis5.y

seis1.x ~~ seis1.y
seis2.x ~~ seis2.y
seis3.x ~~ seis3.y
seis4.x ~~ seis4.y
seis5.x ~~ seis5.y

poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

seisT2 ~ 1*seisT1
dseis =~ 1*seisT2
dseis ~ 1
seisT1 ~ 1
seisT2 ~ 0*1

dseis ~~ dseis
dseis ~~ seisT1
dseis ~ poor_cov
help ~ dseis
'

fitSEIS <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSEIS, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Fitting across groups - ideology, income, and age?
fitSEIS <- lavaan(mod, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSEIS, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitSEIS <- lavaan(mod, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSEIS, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Latent Change Score Model with support for redistribution - Corresponds to Figure 1d
mod <- '
redistT1 =~ 1*redist1 + redist2 + redist3 + redist4
redistT1 ~~ redistT1
redist1 ~~ redist1
redist2 ~~ redist2
redist3 ~~ redist3
redist4 ~~ redist4

redistT2 =~ 1*redist_1 + redist_2 + redist_3 + redist_4
redistT2 ~~ 0*redistT2
redist_1 ~~ redist_1
redist_2 ~~ redist_2
redist_3 ~~ redist_3
redist_4 ~~ redist_4


redist1 ~~ redist_1
redist2 ~~ redist_2
redist3 ~~ redist_3
redist4 ~~ redist_4


poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

redistT2 ~ 1*redistT1
dredist =~ 1*redistT2
dredist ~ 1
redistT1 ~ 1
redistT2 ~ 0*1

dredist ~~ dredist
dredist ~~ redistT1
dredist ~ poor_cov
help ~ dredist
'

fitRED <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitRED, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Fitting across groups - ideology, income, and race?
fitRED <- lavaan(mod, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitRED, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitRED <- lavaan(mod, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitRED, fit.measures = T, standardized = T, rsquare = T, ci=T)


# ----------------------------------------------------------------------------------------------------------------
# Two additional models - change in attributions predicting change in support for inequality and redistribution
# ----------------------------------------------------------------------------------------------------------------

# Latent Change Score Model - Change in attributions predicting change in support for inequality
# Corresponds to Extended Data Figure 1
mod <- '
# SITUATIONAL
# Measurement Models/Latent Variables
sitT1 =~ 1*att5 + att6 + att7 + att8 + att9 + att10 + att11 + att12
sitT2 =~ 1*atts5 + atts6 + atts7 + atts8 + atts9 + atts10 + atts11 + atts12
dsit =~ 1*sitT2
# Regressions
sitT2 ~ 1*sitT1
# Intercepts (Means)
dsit ~ 1
sitT1 ~ 1
sitT2 ~ 0*1
# Variances/Covariances
dsit ~~ dsit
sitT1 ~~ sitT1
sitT2 ~~ 0*sitT2
dsit ~~ sitT1
att5 ~~ att5
att6 ~~ att6
att7 ~~ att7
att8 ~~ att8
att9 ~~ att9
att10 ~~ att10
att11 ~~ att11
att12 ~~ att12
atts5 ~~ atts5
atts6 ~~ atts6
atts7 ~~ atts7
atts8 ~~ atts8
atts9 ~~ atts9
atts10 ~~ atts10
atts11 ~~ atts11
atts12 ~~ atts12
att5 ~~ atts5
att6 ~~ atts6
att7 ~~ atts7
att8 ~~ atts8
att9 ~~ atts9
att10 ~~ atts10
att11 ~~ atts11
att12 ~~ atts12

# DISPOSITIONAL
# Measurement Models/Latent Variables
dispT1 =~ 1*att1 + att2 + att3 + att4 
dispT2 =~ 1*atts1 + atts2 + atts3 + atts4
ddisp =~ 1*dispT2
# Regressions
dispT2 ~ 1*dispT1
# Intercepts (Means)
ddisp ~ 1
dispT1 ~ 1
dispT2 ~ 0*1
# Variances/Covariances
ddisp ~~ ddisp
dispT1 ~~ dispT1
dispT2 ~~ 0*dispT2
ddisp ~~ dispT1
att1 ~~ att1
att2 ~~ att2
att3 ~~ att3
att4 ~~ att4
atts1 ~~ atts1
atts2 ~~ atts2
atts3 ~~ atts3
atts4 ~~ atts4
att1 ~~ atts1
att2 ~~ atts2
att3 ~~ atts3
att4 ~~ atts4

# SEIS
# Measurement Models/Latent Variables
seisT1 =~ 1*seis1.x + seis2.x + seis3.x + seis4.x + seis5.x 
seisT2 =~ 1*seis1.y + seis2.y + seis3.y + seis4.y + seis5.y 
dseis =~ 1*seisT2
# Regressions
seisT2 ~ 1*seisT1
# Intercepts (Means)
dseis ~ 1
seisT1 ~ 1
seisT2 ~ 0*1
# Variances/Covariances
dseis ~~ dseis
seisT1 ~~ seisT1
seisT2 ~~ 0*seisT2
dseis ~~ seisT1
seis1.x ~~ seis1.x
seis2.x ~~ seis2.x
seis3.x ~~ seis3.x
seis4.x ~~ seis4.x
seis5.x ~~ seis5.x
seis2.y ~~ seis2.y
seis3.y ~~ seis3.y
seis4.y ~~ seis4.y
seis5.y ~~ seis5.y
seis1.x ~~ seis1.y
seis2.x ~~ seis2.y
seis3.x ~~ seis3.y
seis4.x ~~ seis4.y
seis5.x ~~ seis5.y

# Regressing attributions change on change in support for inequality.
dseis ~ dsit
dseis ~ ddisp
'

fitLCSM <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitLCSM, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Latent Change Score Model - Change in attributions predicting change in support for redistribution
#  Corresponds to Extended Data Figure 2
mod <- '
# SITUATIONAL
# Measurement Models/Latent Variables
sitT1 =~ 1*att5 + att6 + att7 + att8 + att9 + att10 + att11 + att12
sitT2 =~ 1*atts5 + atts6 + atts7 + atts8 + atts9 + atts10 + atts11 + atts12
dsit =~ 1*sitT2
# Regressions
sitT2 ~ 1*sitT1
# Intercepts (Means)
dsit ~ 1
sitT1 ~ 1
sitT2 ~ 0*1
# Variances/Covariances
dsit ~~ dsit
sitT1 ~~ sitT1
sitT2 ~~ 0*sitT2
dsit ~~ sitT1
att5 ~~ att5
att6 ~~ att6
att7 ~~ att7
att8 ~~ att8
att9 ~~ att9
att10 ~~ att10
att11 ~~ att11
att12 ~~ att12
atts5 ~~ atts5
atts6 ~~ atts6
atts7 ~~ atts7
atts8 ~~ atts8
atts9 ~~ atts9
atts10 ~~ atts10
atts11 ~~ atts11
atts12 ~~ atts12
att5 ~~ atts5
att6 ~~ atts6
att7 ~~ atts7
att8 ~~ atts8
att9 ~~ atts9
att10 ~~ atts10
att11 ~~ atts11
att12 ~~ atts12

# DISPOSITIONAL
# Measurement Models/Latent Variables
dispT1 =~ 1*att1 + att2 + att3 + att4 
dispT2 =~ 1*atts1 + atts2 + atts3 + atts4
ddisp =~ 1*dispT2
# Regressions
dispT2 ~ 1*dispT1
# Intercepts (Means)
ddisp ~ 1
dispT1 ~ 1
dispT2 ~ 0*1
# Variances/Covariances
ddisp ~~ ddisp
dispT1 ~~ dispT1
dispT2 ~~ 0*dispT2
ddisp ~~ dispT1
att1 ~~ att1
att2 ~~ att2
att3 ~~ att3
att4 ~~ att4
atts1 ~~ atts1
atts2 ~~ atts2
atts3 ~~ atts3
atts4 ~~ atts4
att1 ~~ atts1
att2 ~~ atts2
att3 ~~ atts3
att4 ~~ atts4

# Measurement Models/Latent Variables
redT1 =~ 1*redist1 + redist2 + redist3 + redist4
redT2 =~ 1*redist_1 + redist_2 + redist_3 + redist_4
dred =~ 1*redT2
# Regressions
redT2 ~ 1*redT1
# Intercepts (Means)
dred ~ 1
redT1 ~ 1
redT2 ~ 0*1
# Variances/Covariances
dred ~~ dred
redT1 ~~ redT1
redT2 ~~ 0*redT2
dred ~~ redT1
redist1 ~~ redist1
redist2 ~~ redist2
redist3 ~~ redist3
redist4 ~~ redist4
redist_1 ~~ redist_1
redist_2 ~~ redist_2
redist_3 ~~ redist_3
redist_4 ~~ redist_4
redist1 ~~ redist_1
redist2 ~~ redist_2
redist3 ~~ redist_3
redist4 ~~ redist_4

dred ~ dsit
dred ~ ddisp
'

fitLCS <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitLCS, fit.measures = T, standardized = T, rsquare = T, ci=T)

# ----------------------------------------------------------------------------------------------------------------
# Additional models with observed variables only
# ----------------------------------------------------------------------------------------------------------------

# Latent Change Score Model, dispositional attributions for poverty - Corresponds to Extended Data Figure 3
mod <- '
disp.y ~ 1*disp.x
ddisp =~ 1*disp.y
ddisp ~ 1
disp.x ~ 1
disp.y ~ 0*1
ddisp ~~ ddisp
disp.x ~~ disp.x
disp.y ~~ 0*disp.y
ddisp ~~ disp.x
'

fitLCSM <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitLCSM, fit.measures = T, standardized = T, rsquare = T)

# Latent Change Score Model, situational attributions for poverty - Corresponds to Extended Data Figure 4
mod <- '
sit.y ~ 1*sit.x
dsit =~ 1*sit.y
dsit ~ 1
sit.x ~ 1
sit.y ~ 0*1
dsit ~~ dsit
sit.x ~~ sit.x
sit.y ~~ 0*sit.y
dsit ~~ sit.x
'

fitLCSM <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitLCSM, fit.measures = T, standardized = T, rsquare = T)

# Latent Change Score Model, support for economic inequality - Corresponds to Extended Data Figure 5
mod <- '
seis.y ~ 1*seis.x
dseis =~ 1*seis.y
dseis ~ 1
seis.x ~ 1
seis.y ~ 0*1
dseis ~~ dseis
seis.x ~~ seis.x
seis.y ~~ 0*seis.y
dseis ~~ seis.x
'

fitLCSM <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitLCSM, fit.measures = T, standardized = T, rsquare = T)

# Latent Change Score Model, support for redistribution - Corresponds to Extended Data Figure 6
mod <- '
redist.y ~ 1*redist.x
dredist =~ 1*redist.y
dredist ~ 1
redist.x ~ 1
redist.y ~ 0*1
dredist ~~ dredist
redist.x ~~ redist.x
redist.y ~~ 0*redist.y
dredist ~~ redist.x
'

fitLCSM <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitLCSM, fit.measures = T, standardized = T, rsquare = T)


# ----------------------------------------------------------------------------------------------------------------
# 
# ORIGINAL PRE-REGISTERED MODELS
#
# ----------------------------------------------------------------------------------------------------------------

# Calculate all the difference scores; Time 2 minus Time 1
merged$seis_dif <- merged$seis.y - merged$seis.x
merged$redist_dif <- merged$redist.y - merged$redist.x
merged$sit_dif <- merged$sit.y - merged$sit.x
merged$disp_dif <- merged$disp.y - merged$disp.x

# Paired t-tests
print(t.test(merged$disp.x, merged$disp.y, paired=T), digits=6) # Dispositional attributions for poverty
print(t.test(merged$sit.x, merged$sit.y, paired=T), digits=6) # Situational attributions for poverty
print(t.test(merged$seis.x, merged$seis.y, paired=T), digits=6) # Support for economic inequality
print(t.test(merged$redist.x, merged$redist.y, paired=T), digits=6) # Support for redistribution

# Standardize the measure about beliefs around COVID
merged$poor_covidz <- scale(merged$poor_covid, scale=T, center=T)

# Linear regressions
library(sjPlot)
merged$poor_covidz <- as.numeric(merged$poor_covidz)

disp_mod <- lm(disp_dif~poor_covidz, data=merged) # Dispositional attributions for poverty
summary(disp_mod)
confint(disp_mod, level=.95)
a <- plot_model(disp_mod, type="pred", terms="poor_covidz")

sit_mod <- lm(sit_dif~poor_covidz, data=merged) # situational Attributions for poverty
summary(sit_mod)
confint(sit_mod, level=.95)
b <- plot_model(sit_mod, type="pred", terms="poor_covidz")

seis_mod <- lm(seis_dif~poor_covidz, data=merged) # Support for economic inequality
summary(seis_mod)
confint(seis_mod, level=.95)
c <- plot_model(seis_mod, type="pred", terms="poor_covidz")

redist_mod <- lm(redist_dif~poor_covidz, data=merged) # Support for redistribution
summary(redist_mod)
confint(redist_mod, level=.95)
d <- plot_model(redist_mod, type="pred", terms="poor_covidz")

library(ggpubr)
ggarrange(a, b, c, d, ncol = 2, nrow = 2)


g# ----------------------------------------------------------------------------------------------------------------
#
# Build Figure 2 - Code below here is adapted from https://github.com/jorvlan/open-visualizations
#
# ----------------------------------------------------------------------------------------------------------------

# Median Split on COVID beliefs
merged$COVID <- "Low COVID-19 Impact on the Poor"
merged$COVID[which(merged$poor_covid >= 6.5)] <- "High COVID-19 Impact on the Poor"

# Create a smaller dataset that just contains our variables of interest
cols = c("PROLIFIC_PID", "seis.x", "seis.y", "COVID")
selected <- merged[cols]

data_long <- gather(selected, time, seis_atts, seis.x:seis.y, factor_key=TRUE)
data_long$Time = 0
data_long$Time[data_long$time == "seis.x"] <- 1
data_long$Time[data_long$time == "seis.y"] <- 2

# The covid beliefs variable into a factor
data_long$COVID_f = factor(data_long$COVID, levels=c("Low COVID-19 Impact on the Poor", "High COVID-19 Impact on the Poor"))

# Turn the long data into a dataset for the figure.
before = data_long$seis_atts[1:233]
after = data_long$seis_atts[234:466]
id = data_long$PROLIFIC_PID
covid = data_long$COVID_f
n <- length(before)
d <- data.frame(y = c(before, after),
                x = rep(c(1,2), each=n),
                id = id,
                covid = covid)

# Set the jitter on the datapoints
set.seed(321)
d$xj <- jitter(d$x, amount=.09)

# Figure with individual data, lines between Time 1 and 2 for each person
# as well as boxplots and density distributions, low covid beliefs on the left, high on the right
ggplot(data = d, aes(y = y)) +
  
  #Add geom_() objects
  geom_point(data = d %>% filter(x =="1"), aes(x = xj), color = 'dodgerblue', size = 1.5, 
             alpha = .6) +
  geom_point(data = d %>% filter(x =="2"), aes(x = xj), color = 'darkorange', size = 1.5, 
             alpha = .6) +
  geom_line(aes(x = xj, group = id), color = 'lightgray', alpha = .3) + facet_wrap(~covid) +
  
  geom_half_boxplot(
    data = d %>% filter(x=="1"), aes(x=x, y = y), position = position_nudge(x = -.25), 
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2, 
    fill = 'dodgerblue') +
  
  geom_half_boxplot(
    data = d %>% filter(x=="2"), aes(x=x, y = y), position = position_nudge(x = .15), 
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2, 
    fill = 'darkorange') +
  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.3), 
    side = "l", fill = 'dodgerblue') +
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = .3), 
    side = "r", fill = "darkorange") +
  
  
  #Define additional settings
  scale_x_continuous(breaks=c(1,2), labels=c("April 2019", "May 2020"), limits=c(0, 3)) +
  xlab (NULL) + ylab("Support for Economic Inequality") +
  theme_classic()+
  coord_cartesian(ylim=c(1, 7))

# ----------------------------------------------------------------------------------------------------------------
#
# Addressing R&R Concerns at JESP; November 5 2020
# Dylan Wiwad
#
# ----------------------------------------------------------------------------------------------------------------

# Age not correlated with attributions for poverty at T1 or T2, nor is it correlated with
# belief that the poor have been impacted by covid
print(corr.test(data$age, data$sit, ci=T), short = F, 5)
print(corr.test(data$age, data$disp, ci=T), short = F, 5)
print(corr.test(data$sit, data$ideology, ci=T), short = F, 5)
print(corr.test(data$disp, data$ideology, ci=T), short = F, 5)

data2$age_num <- as.numeric(as.character(data2$age))
print(corr.test(data2$age_num, data2$sit, ci=T), short = F, 5)
print(corr.test(data2$age_num, data2$disp, ci=T), short = F, 5)
print(corr.test(data2$age_num, data2$poor_covid, ci=T), short = F, 5)
print(corr.test(data2$age_num, data2$ideology, ci=T), short = F, 5)
print(corr.test(data2$sit, data2$ideology, ci=T), short = F, 5)
print(corr.test(data2$disp, data2$ideology, ci=T), short = F, 5)
print(corr.test(data2$poor_covid, data2$ideology, ci=T), short = F, 5)

library(ppcor)
d2 <- data[which(!is.na(data$age)),]
d2 <- data[which(!is.na(data$sit)),]
d2 <- data[which(!is.na(data$disp)),]
d2 <- data[which(!is.na(data$ideology)),]
print(pcor.test(d2$age, d2$sit, d2$ideology), short = F, 5)
pcor.test(d2$age, d2$disp, d2$ideology)

pcor.test(data2$age, data2$sit, data2$ideology)
pcor.test(data2$age, data2$disp, data2$ideology)
pcor.test(data2$age, data2$poor_covid, data2$ideology)

corr.test(merged$sit.x, merged$poor_covid)
corr.test(merged$disp.x, merged$poor_covid)

corr.test(merged$redist.y, merged$help_poor)
corr.test(merged$poor_covid, merged$help_poor)


# Correlations among all the key variables in the manuscript
cols <- c("sit.y", "disp.y", "seis.y", "poor_covid", "help_poor")
cordat <- merged[cols]

print(corr.test(cordat), 5)

# Let's see if there is a difference in percieved impact of COVID by age, income,
# ideology, and self_cov

t.test(merged$poor_covid~merged$age_split)
t.test(merged$poor_covid~merged$inc_split)
t.test(merged$poor_covid~merged$party)
t.test(merged$poor_covid~merged$self_cov_split)

merged$age.yz <- scale(merged$age.y, scale=T, center=T)
merged$income.yz <- scale(merged$income.y, scale=T, center=T)
merged$self_covz <- scale(merged$self_cov, scale=T, center=T)
merged$poor_covidz <- scale(merged$poor_covid, scale=T, center=T)

age <- lm(poor_covidz~age.yz, data=merged, na.action="na.omit")
income <- lm(poor_covidz~income.yz, data=merged, na.action="na.omit")
self <- lm(poor_covidz~self_covz, data=merged, na.action="na.omit")
party <- lm(poor_covidz~party, data=merged, na.action="na.omit")

plot_model(income, type="pred", terms=c("income.yz"))
ggeffects::ggemmeans(party, terms=c("party"))
ggeffects::ggemmeans(income, terms=c("income.y"))

age <- lm(self_covz~age.yz, data=merged, na.action="na.omit")
income <- lm(self_covz~income.yz, data=merged, na.action="na.omit")
party <- lm(self_covz~party, data=merged, na.action="na.omit")

# Group based models, constraining factors


# Latent Change Score Model with dispositional attributions for poverty - Corresponds to Figure 1a
modF <- '
dispT1 =~ 1*att1 + att2 + att3 + att4
dispT1 ~~ dispT1
att1 ~~ att1
att2 ~~ att2
att3 ~~ att3
att4 ~~ att4

dispT2 =~ 1*atts1 + atts2 + atts3 + atts4
dispT2 ~~ 0*dispT2
atts1 ~~ atts1
atts2 ~~ atts2
atts3 ~~ atts3
atts4 ~~ atts4

att1 ~~ atts1
att2 ~~ atts2
att3 ~~ atts3
att4 ~~ atts4

poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

dispT2 ~ 1*dispT1
ddisp =~ 1*dispT2
ddisp ~ 1
dispT1 ~ 1
dispT2 ~ 0*1

ddisp ~~ ddisp
ddisp ~~ dispT1
ddisp ~ poor_cov
help ~ ddisp
'

modC <- '
dispT1 =~ 1*att1 + att2 + att3 + att4
dispT1 ~~ dispT1
att1 ~~ att1
att2 ~~ att2
att3 ~~ att3
att4 ~~ att4

dispT2 =~ 1*atts1 + atts2 + atts3 + atts4
dispT2 ~~ 0*dispT2
atts1 ~~ atts1
atts2 ~~ atts2
atts3 ~~ atts3
atts4 ~~ atts4

att1 ~~ atts1
att2 ~~ atts2
att3 ~~ atts3
att4 ~~ atts4

poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

dispT2 ~ 1*dispT1
ddisp =~ 1*dispT2
ddisp ~ 1
dispT1 ~ 1
dispT2 ~ 0*1

ddisp ~~ ddisp
ddisp ~~ dispT1
ddisp ~ c(V1,V1)*poor_cov
help ~ ddisp
'

# Fitting across groups - ideology, income
fitDISP1 <- lavaan(modF, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitDISP1, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Fitting across groups - ideology, income
fitDISP2 <- lavaan(modC, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitDISP, fit.measures = T, standardized = T, rsquare = T, ci=T)

anova(fitDISP1,fitDISP2)

fitDISP1 <- lavaan(modF, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitDISP, fit.measures = T, standardized = T, rsquare = T, ci=T)

# Fitting across groups - ideology, income
fitDISP2 <- lavaan(modC, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitDISP, fit.measures = T, standardized = T, rsquare = T, ci=T)

anova(fitDISP1,fitDISP2)

# Latent Change Score Model with situational attributions for poverty - Corresponds to Figure 1b
modF <- '
sitT1 =~  1*att5 + att6 + att7 + att8 + att9 + att10 + att11 + att12
sitT1 ~~ sitT1
att5 ~~ att5
att6 ~~ att6
att7 ~~ att7
att8 ~~ att8
att9 ~~ att9
att10 ~~ att10
att11 ~~ att11
att12 ~~ att12

sitT2 =~ 1*atts5 + atts6 + atts7 + atts8 + atts9 + atts10 + atts11 + atts12
sitT2 ~~ 0*sitT2
atts5 ~~ atts5
atts6 ~~ atts6
atts7 ~~ atts7
atts8 ~~ atts8
atts9 ~~ atts9
atts10 ~~ atts10
atts11 ~~ atts11
atts12 ~~ atts12

poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

att5 ~~ atts5
att6 ~~ atts6
att7 ~~ atts7
att8 ~~ atts8
att9 ~~ atts9
att10 ~~ atts10
att11 ~~ atts11
att12 ~~ atts12

sitT2 ~ 1*sitT1
dsit =~ 1*sitT2
dsit ~ 1
sitT1 ~ 1
sitT2 ~ 0*1

dsit ~~ dsit
dsit ~~ sitT1
dsit ~ poor_cov
help ~ dsit
'

modC <- '
sitT1 =~  1*att5 + att6 + att7 + att8 + att9 + att10 + att11 + att12
sitT1 ~~ sitT1
att5 ~~ att5
att6 ~~ att6
att7 ~~ att7
att8 ~~ att8
att9 ~~ att9
att10 ~~ att10
att11 ~~ att11
att12 ~~ att12

sitT2 =~ 1*atts5 + atts6 + atts7 + atts8 + atts9 + atts10 + atts11 + atts12
sitT2 ~~ 0*sitT2
atts5 ~~ atts5
atts6 ~~ atts6
atts7 ~~ atts7
atts8 ~~ atts8
atts9 ~~ atts9
atts10 ~~ atts10
atts11 ~~ atts11
atts12 ~~ atts12

poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

att5 ~~ atts5
att6 ~~ atts6
att7 ~~ atts7
att8 ~~ atts8
att9 ~~ atts9
att10 ~~ atts10
att11 ~~ atts11
att12 ~~ atts12

sitT2 ~ 1*sitT1
dsit =~ 1*sitT2
dsit ~ 1
sitT1 ~ 1
sitT2 ~ 0*1

dsit ~~ dsit
dsit ~~ sitT1
dsit ~ c(V1*V1)*poor_cov
help ~ dsit
'

# Fitting across groups - ideology, income, self impact?
fitSIT1 <- lavaan(modF, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSIT, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitSIT2 <- lavaan(modC, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSIT, fit.measures = T, standardized = T, rsquare = T, ci=T)

anova(fitSIT1, fitSIT2)

fitSIT1 <- lavaan(modF, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSIT, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitSIT2 <- lavaan(modC, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSIT, fit.measures = T, standardized = T, rsquare = T, ci=T)

anova(fitSIT1, fitSIT2)


# Latent Change Score Model with support for inequality - Corresponds to Figure 1c
modF <- '
seisT1 =~ 1*seis1.x + seis2.x + seis3.x + seis4.x + seis5.x
seisT1 ~~ seisT1
seis1.x ~~ seis1.x
seis2.x ~~ seis2.x
seis3.x ~~ seis3.x
seis4.x ~~ seis4.x
seis5.x ~~ seis5.x

seisT2 =~ 1*seis1.y + seis2.y + seis3.y + seis4.y + seis5.y
seisT2 ~~ 0*seisT2
seis1.y ~~ seis1.y
seis2.y ~~ seis2.y
seis3.y ~~ seis3.y
seis4.y ~~ seis4.y
seis5.y ~~ seis5.y

seis1.x ~~ seis1.y
seis2.x ~~ seis2.y
seis3.x ~~ seis3.y
seis4.x ~~ seis4.y
seis5.x ~~ seis5.y

poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

seisT2 ~ 1*seisT1
dseis =~ 1*seisT2
dseis ~ 1
seisT1 ~ 1
seisT2 ~ 0*1

dseis ~~ dseis
dseis ~~ seisT1
dseis ~ poor_cov
help ~ dseis
'

modC <- '
seisT1 =~ 1*seis1.x + seis2.x + seis3.x + seis4.x + seis5.x
seisT1 ~~ seisT1
seis1.x ~~ seis1.x
seis2.x ~~ seis2.x
seis3.x ~~ seis3.x
seis4.x ~~ seis4.x
seis5.x ~~ seis5.x

seisT2 =~ 1*seis1.y + seis2.y + seis3.y + seis4.y + seis5.y
seisT2 ~~ 0*seisT2
seis1.y ~~ seis1.y
seis2.y ~~ seis2.y
seis3.y ~~ seis3.y
seis4.y ~~ seis4.y
seis5.y ~~ seis5.y

seis1.x ~~ seis1.y
seis2.x ~~ seis2.y
seis3.x ~~ seis3.y
seis4.x ~~ seis4.y
seis5.x ~~ seis5.y

poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

seisT2 ~ 1*seisT1
dseis =~ 1*seisT2
dseis ~ 1
seisT1 ~ 1
seisT2 ~ 0*1

dseis ~~ dseis
dseis ~~ seisT1
dseis ~ poor_cov
help ~ c(V1,V1)*dseis
'

# Fitting across groups - ideology, income, and age?
fitSEIS1 <- lavaan(modF, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSEIS1, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitSEIS2 <- lavaan(modC, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSEIS, fit.measures = T, standardized = T, rsquare = T, ci=T)

anova(fitSEIS1, fitSEIS2)

fitSEIS1 <- lavaan(modF, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSEIS, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitSEIS2 <- lavaan(modC, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitSEIS, fit.measures = T, standardized = T, rsquare = T, ci=T)

anova(fitSEIS1, fitSEIS2)



# Latent Change Score Model with support for redistribution - Corresponds to Figure 1d
modF <- '
redistT1 =~ 1*redist1 + redist2 + redist3 + redist4
redistT1 ~~ redistT1
redist1 ~~ redist1
redist2 ~~ redist2
redist3 ~~ redist3
redist4 ~~ redist4

redistT2 =~ 1*redist_1 + redist_2 + redist_3 + redist_4
redistT2 ~~ 0*redistT2
redist_1 ~~ redist_1
redist_2 ~~ redist_2
redist_3 ~~ redist_3
redist_4 ~~ redist_4


redist1 ~~ redist_1
redist2 ~~ redist_2
redist3 ~~ redist_3
redist4 ~~ redist_4


poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

redistT2 ~ 1*redistT1
dredist =~ 1*redistT2
dredist ~ 1
redistT1 ~ 1
redistT2 ~ 0*1

dredist ~~ dredist
dredist ~~ redistT1
dredist ~ poor_cov
help ~ dredist
'

modC <- '
redistT1 =~ 1*redist1 + redist2 + redist3 + redist4
redistT1 ~~ redistT1
redist1 ~~ redist1
redist2 ~~ redist2
redist3 ~~ redist3
redist4 ~~ redist4

redistT2 =~ 1*redist_1 + redist_2 + redist_3 + redist_4
redistT2 ~~ 0*redistT2
redist_1 ~~ redist_1
redist_2 ~~ redist_2
redist_3 ~~ redist_3
redist_4 ~~ redist_4


redist1 ~~ redist_1
redist2 ~~ redist_2
redist3 ~~ redist_3
redist4 ~~ redist_4


poor_cov =~ 1*poor1z + poor2z + poor3z + poor4z
poor_cov ~~ poor_cov
poor1z ~~ poor1z
poor2z ~~ poor2z
poor3z ~~ poor3z
poor4z ~~ poor4z

help =~ 1*help_poor_1z + help_poor_2z + help_poor_3z
help ~~ help
help_poor_1z ~~ help_poor_1z
help_poor_2z ~~ help_poor_2z
help_poor_3z ~~ help_poor_3z

redistT2 ~ 1*redistT1
dredist =~ 1*redistT2
dredist ~ 1
redistT1 ~ 1
redistT2 ~ 0*1

dredist ~~ dredist
dredist ~~ redistT1
dredist ~ poor_cov
help ~ c(V1,V1)*dredist
'

# Fitting across groups - ideology, income, and race?
fitRED1 <- lavaan(modF, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitRED1, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitRED2 <- lavaan(modC, data=merged, group = "party", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitRED, fit.measures = T, standardized = T, rsquare = T, ci=T)

anova(fitRED1, fitRED2)

fitRED1 <- lavaan(modF, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitRED, fit.measures = T, standardized = T, rsquare = T, ci=T)

fitRED2 <- lavaan(modC, data=merged, group = "inc_split", estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitRED, fit.measures = T, standardized = T, rsquare = T, ci=T)

anova(fitRED1, fitRED2)


# Looking at the magnitude and direction of change in each key variable by belief the poor have been impacted

fitRED <- lavaan(mod, data=merged, estimator = "mlr", fixed.x=F, missing = "fiml")
summary(fitRED, fit.measures = T, standardized = T, rsquare = T, ci=T)

merged$disp_change <- merged$disp.y - merged$disp.x
merged$sit_change <- merged$sit.y - merged$sit.x
merged$seis_change <- merged$seis.y - merged$seis.x
merged$redist_change <- merged$redist.y - merged$redist.x

disp <- lm(disp_change~poor_covid, data=merged, na.action="na.omit")
sit <- lm(sit_change~poor_covid, data=merged, na.action="na.omit")
seis <- lm(seis_change~poor_covid, data=merged, na.action="na.omit")
red <- lm(redist_change~poor_covid, data=merged, na.action="na.omit")

a <- plot_model(disp, type="pred", terms = "poor_covid") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
b <- plot_model(sit, type="pred", terms = "poor_covid") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
c <- plot_model(seis, type="pred", terms = "poor_covid") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
d <- plot_model(red, type="pred", terms = "poor_covid") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggarrange(a, b, c, d, ncol=2, nrow=2) + theme_bw() 

ggeffects::ggemmeans(disp, terms = "poor_covid [2, 6]")
ggeffects::ggemmeans(sit, terms = "poor_covid")
ggeffects::ggemmeans(seis, terms = "poor_covid")
ggeffects::ggemmeans(red, terms = "poor_covid")
