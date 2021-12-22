# ------------------------------------------------------------------------------------------
#
# Typology of Political Policy Support - Study 2
# Main Analyses
# 
# June 29, 2021
#
# ------------------------------------------------------------------------------------------


# You need to set this to be wherever your data is stored. 
# For example, if your data is in the downloads folder the line is:
# setwd("~/downloads/")
setwd("CHANGE TO WHERE YOU ARE STORING THE DATA")

# This reads in the long data file
bdata <- read.csv("long_bin.csv", header=T)

# Libraries
library(emmeans)
library(psych)
library(lme4)
library(lmerTest)
library(sjPlot)

# Create the triangle variable in the long data set --------------------------------------------------------
bdata$ineq <- 0
bdata$ineq <- bdata$rich - bdata$poor
bdata$eff <- 0
bdata$eff <- bdata$rich + bdata$poor

bdata$triangle <- 0
bdata$triangle[bdata$poor > 0 & bdata$rich > 0 & bdata$ineq  > 0] <- 1
bdata$triangle[bdata$poor > 0 & bdata$rich > 0 & bdata$ineq  < 0] <- 2
bdata$triangle[bdata$poor > 0 & bdata$rich < 0 & bdata$ineq  < 0 & bdata$eff > 0] <- 3
bdata$triangle[bdata$poor > 0 & bdata$rich < 0 & bdata$ineq  < 0 & bdata$eff < 0] <- 4

bdata$triangle[bdata$triangle == 0] <- NA
# ---------------------------------------------------------------------------------------------------------

# Create poor up, rich up/down, and ineq variables --------------------------------------------------------
bdata$poorup <- bdata$poor
bdata$poorup[bdata$poorup <= 0] <- 0

bdata$richup <- bdata$rich
bdata$richup[bdata$richup <= 0] <- 0

bdata$richd <- bdata$rich
bdata$richd[bdata$richd >= 0] <- 0
bdata$richdabs <- abs(bdata$richd)


bdata$ineq <- bdata$rich - bdata$poor

bdata$inequp <- bdata$ineq
bdata$inequp[bdata$inequp <= 0] <- 0

bdata$ineqd <- bdata$ineq
bdata$ineqd[bdata$ineqd >= 0] <- 0

# Create z-score versions of each
bdata$poorupz <- scale(bdata$poorup, scale = T, center = T)
bdata$richdz <- scale(bdata$richd, scale = T, center = T)
bdata$richdabsz <- scale(bdata$richdabs, scale = T, center = T)
bdata$richupz <- scale(bdata$richup, scale = T, center = T)
bdata$ineqdz <- scale(bdata$ineqd, scale = T, center = T)
bdata$inequpz <- scale(bdata$inequp, scale = T, center = T)

# ----------------------------------------------------------------------------------------------------------
#
# CONCEPTUAL REPLICATION OF STUDY 1
#
# ----------------------------------------------------------------------------------------------------------

# Contrast code political ideology
bdata$rep_con <- bdata$repub
bdata$rep_con[bdata$rep_con == 0] <- -1

# Code for Republicans for Simple Slopes
bdata$dem <- 0
bdata$dem[which(bdata$repub == 0)] <- 1

# First Crossed-Factors Multilevel Model
# Need to compute the change_efficiency metric. Did not compute it before
bdata$change_eff <- 0
bdata$change_eff[which(bdata$eff > 0)] <- 1
bdata$change_eff[which(bdata$eff < 0)] <- -1
bdata$change_effz <- scale(bdata$change_eff, scale=T, center=T)


# TABLE S6 MODEL 1a
model1a <- glmer(support ~ change_poorz + change_richz + change_ineqz + change_effz +
                   (1 | pid) + (1 | distribution),
                 data = bdata, family=binomial, na.action = "na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1a)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(model1a)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# TABLE S6 MODEL 1b
model1b <- glmer(support ~ change_poorz + change_richz + change_ineqz + change_effz + rep_con +
                   (change_poorz*rep_con) + (change_richz*rep_con) + 
                   (change_ineqz*rep_con) + (change_effz*rep_con) +
                   (1 | pid) + (1 | distribution),
                 data = bdata, family=binomial, na.action = "na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1b)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(model1b)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))


# TABLE S6 MODEL 1c - Democrats
model1c <- glmer(support ~ change_poorz + change_richz + change_ineqz + change_effz + repub +
                   (change_poorz*repub) + (change_richz*repub) + 
                   (change_ineqz*repub) + (change_effz*repub) +
                   (1 | pid) + (1 | distribution),
                 data = bdata, family=binomial, na.action = "na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1c)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(model1c)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# TABLE S6 MODEL 1c - Republicans
model1d <- glmer(support ~ change_poorz + change_richz + change_ineqz + change_effz + dem +
                   (change_poorz*dem) + (change_richz*dem) + 
                   (change_ineqz*dem) + (change_effz*dem) +
                   (1 | pid) + (1 | distribution),
                 data = bdata, family=binomial, na.action = "na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1d)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(model1d)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))


# Getting the model prediction percentages for the main text - Overall
ggeffects::ggemmeans(model1b, terms = c("change_poorz"))
ggeffects::ggemmeans(model1b, terms = c("change_ineqz"))
ggeffects::ggemmeans(model1b, terms = c("change_effz"))
ggeffects::ggemmeans(model1b, terms = c("change_richz"))

# Getting the model prediction percentages for the main text - Partisan Differences
ggeffects::ggemmeans(model1b, terms = c("change_poorz", "rep_con"))
ggeffects::ggemmeans(model1b, terms = c("change_ineqz", "rep_con"))
ggeffects::ggemmeans(model1b, terms = c("change_effz", "rep_con"))
ggeffects::ggemmeans(model1b, terms = c("change_richz", "rep_con"))


# ----------------------------------------------------------------------------------------------------------
#
# TYPOLOGY OF VOTING PATTERNS AMONG DEMOCRATS DIFFERENTIATE BIDEN SUPPORTERS FROM SANDERS SUPPORTERS
#
# ----------------------------------------------------------------------------------------------------------

# For this analysis, we need to use the wide data and compute octant weights
bdata_wide <- read.csv("wide_bin.csv", header=T)

# These next chunks create a variable, and then a z-score, for each triangle weight
# T1, cols 9, 16-17, 23-25, 30-33, 37-41, 44-49
cols.t1 <- c(9, 16:17, 23:25, 30:33, 37:41, 44:49)
bdata_wide$t1_weight <- rowMeans(bdata_wide[,cols.t1], na.rm = TRUE)
bdata_wide$t1_weightz <- scale(bdata_wide$t1_weight, scale=T, center=T)
# T2, cols 3-8, 11-15, 19-22, 27-29, 35-36, 43
cols.t2 <- c(3:8, 11:15, 19:22, 27:29, 35:36, 43)
bdata_wide$t2_weight <- rowMeans(bdata_wide[,cols.t2], na.rm = TRUE)
bdata_wide$t2_weightz <- scale(bdata_wide$t2_weight, scale=T, center=T)
# T3, cols 52-57, 60-64, 68-71, 76-78, 84-85, 92
cols.t3 <- c(52:57, 60:64, 68:71, 76:78, 84:85, 92)
bdata_wide$t3_weight <- rowMeans(bdata_wide[,cols.t3], na.rm = TRUE)
bdata_wide$t3_weightz <- scale(bdata_wide$t3_weight, scale=T, center=T)
# T4, cols, 58, 65-66, 72-74, 79-82, 86-90, 93-98
cols.t4 <- c(58, 65:66, 72:74, 79:82, 86:90, 93:98)
bdata_wide$t4_weight <- rowMeans(bdata_wide[,cols.t4], na.rm = TRUE)
bdata_wide$t4_weightz <- scale(bdata_wide$t4_weight, scale=T, center=T)

# Get just the registered dems
rdem <- bdata_wide[which(bdata_wide$reg_dem == 1),]

# Shrink again to those voting for Bernie and Biden
bern_bid_dat <- rdem[which(rdem$who_vote == 1 | rdem$who_vote == 2),]
bern_bid_dat$who_vote[which(bern_bid_dat$who_vote == 1)] <- 0
bern_bid_dat$who_vote[which(bern_bid_dat$who_vote == 2)] <- 1
# 0 is Biden, 1 is Bernie

# Model, corresponds to Table S7
m_bb <- glm(who_vote~t1_weightz+t2_weightz+t3_weightz+t4_weightz, 
         data=bern_bid_dat, family = "binomial", na.action="na.omit")
summary(m_bb)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(m_bb)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))


# Looking back to the original models, weighing in bernie and biden voting
# Again, restricting to registered Dems
bern_bid <- bdata[which(bdata$who_vote == 1 | bdata$who_vote == 2),]
bern_bid <- bern_bid[which(bern_bid$reg_dem == 1),]

# Contrast coding bernie and biden for Full Model
bern_bid$bb_cont <- -1
bern_bid$bb_cont[which(bern_bid$who_vote == 1)] <- 1

# To get Simple Slopes in the Biden Model
bern_bid$biden <- 0
bern_bid$biden[which(bern_bid$who_vote == 2)] <- 1

# To get Simple Slopes in the Bernie Model
bern_bid$bernie <- 0
bern_bid$bernie[which(bern_bid$who_vote == 1)] <- 1

# Table S8 - Full Model
full_mod <- glmer(support~change_poorz + change_richz + change_ineqz + change_effz + bb_cont + 
                     (change_poorz*bb_cont) + (change_richz*bb_cont) + (change_ineqz*bb_cont) + (change_effz*bb_cont) + 
                     (1 | pid) + (1 | distribution),
                   data=bern_bid, family=binomial, na.action="na.omit",
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(full_mod)

# Among bidens
bid_mod <- glmer(support~change_poorz + change_richz + change_ineqz + change_effz + biden + 
                     (change_poorz*biden) + (change_richz*biden) + (change_ineqz*biden) + (change_effz*biden) + 
                     (1 | pid) + (1 | distribution),
                   data=bern_bid, family=binomial, na.action="na.omit",
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(bid_mod)

# Among bernies
bern_mod <- glmer(support~change_poorz + change_richz + change_ineqz + change_effz + bernie + 
                     (change_poorz*bernie) + (change_richz*bernie) + (change_ineqz*bernie) + (change_effz*bernie) + 
                     (1 | pid) + (1 | distribution),
                   data=bern_bid, family=binomial, na.action="na.omit",
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(bern_mod)

# Getting the model prediction percentages for the main text - Bernie/Biden Differences
ggeffects::ggemmeans(full_mod, terms = c("change_poorz", "bb_cont"))
ggeffects::ggemmeans(full_mod, terms = c("change_ineqz", "bb_cont"))
ggeffects::ggemmeans(full_mod, terms = c("change_effz", "bb_cont"))
ggeffects::ggemmeans(full_mod, terms = c("change_richz", "bb_cont"))


# ----------------------------------------------------------------------------------------------------------
#
# POSITIVE EMOTIONS TOWARDS THE POOR ARE JUST AS IMPORTANT AS NEGATIVE EMOTIONS TOWARDS THE RICH IN 
# PREDICTING WILLINGNESS TO HARM THE RICH TO REDUCE INEQUALITY
#
# ----------------------------------------------------------------------------------------------------------

# Compute negative emotions composites
cols <- c(143:144)
bdata_wide$neg_poor <-  rowMeans(bdata_wide[,cols], na.rm = TRUE)

cols <- c(147:148)
bdata_wide$neg_rich <-  rowMeans(bdata_wide[,cols], na.rm = TRUE)

# Compute the balance variable
bdata_wide$emote_poor <- bdata_wide$poor_comp - bdata_wide$neg_poor
bdata_wide$emote_rich <- bdata_wide$rich_comp - bdata_wide$neg_rich

# standardize the emotion vars
bdata_wide$emote_poorz <- scale(bdata_wide$emote_poor, scale=T, center=T)
bdata_wide$emote_richz <- scale(bdata_wide$emote_rich, scale=T, center=T)

# Compute a variable measuring the relative preference for quadrant 2 over quadrant 1
bdata_wide$q2 <- (bdata_wide$t3_weightz + bdata_wide$t4_weightz)/2
bdata_wide$q1 <- (bdata_wide$t1_weightz + bdata_wide$t2_weightz)/2
bdata_wide$q2_min_q1 <- bdata_wide$q2 - bdata_wide$q1

# Contrast code political ideology
bdata_wide$rep_con <- bdata_wide$repub
bdata_wide$rep_con[bdata_wide$rep_con == 0] <- -1

# Overall
q2_1 <- lm(q2_min_q1 ~ emote_poorz + emote_richz + rep_con + 
             (emote_poorz*rep_con) + (emote_richz*rep_con),
           data=bdata_wide, na.action = "na.omit")
summary(q2_1)

# Among Democrats
d <- bdata_wide[which(bdata_wide$rep_con == -1),]

q2_1 <- lm(q2_min_q1 ~ emote_poorz + emote_richz,
           data=d, na.action = "na.omit")
summary(q2_1)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(q2_1)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# Reverse score emotions towards the poor to re-run the model with absolute values and compare the coefs
d$emote_poor_rev <- d$emote_poorz*(-1)

q2_1 <- lm(q2_min_q1 ~ emote_poor_rev + emote_richz,
           data=d, na.action = "na.omit")
summary(q2_1)

trends <- rbind(
  emtrends(q2_1, ~1, "emote_poor_rev"),
  emtrends(q2_1, ~1, "emote_richz")
)

# clean up so it does not error later
trends@grid$`1` <- c("emote_poor_rev", "emote_richz")
trends@misc$estName <- "trend"

trends
pairs(trends)



r$emote_poor_rev <- r$emote_poorz*(-1)
q2_1 <- lm(q2_min_q1 ~ emote_poor_rev + emote_richz,
           data=r, na.action = "na.omit")
summary(q2_1)

# Among Republicans
r <- bdata_wide[which(bdata_wide$rep_con == 1),]


q2_1 <- lm(q2_min_q1 ~ emote_poorz + emote_richz,
           data=r, na.action = "na.omit")
summary(q2_1)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(q2_1)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))
