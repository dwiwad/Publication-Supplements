# ------------------------------------------------------------------------------------------
#
# Typology of Political Policy Support - Binary Data Analyses
# 
# Jan 19th, 2020
#
# ------------------------------------------------------------------------------------------

# Read in the data
setwd("CHANGE TO WHERE DATA ARE STORED")

ldata <- read.csv("long_lik.csv", header=T)

# Libraries
library(emmeans)
library(psych)
library(lme4)
library(lmerTest)
library(sjPlot)

# Remove few rows without political ideology (results do not change)
ldata <- ldata[which(ldata$party == "Rep" | ldata$party == "Dem"),]
# Contrast code political ideology
ldata$rep_con <- ldata$repub
ldata$rep_con[ldata$rep_con == 0] <- -1

# Code for Republicans for Simple Slopes
ldata$dem <- 0
ldata$dem[which(ldata$repub == 0)] <- 1

# Code in the triangles
ldata$ineq <- 0
ldata$ineq <- ldata$rich - ldata$poor
ldata$eff <- 0
ldata$eff <- ldata$rich + ldata$poor

ldata$triangle <- 0
ldata$triangle[ldata$poor > 0 & ldata$rich > 0 & ldata$ineq  > 0] <- 1
ldata$triangle[ldata$poor > 0 & ldata$rich > 0 & ldata$ineq  < 0] <- 2
ldata$triangle[ldata$poor > 0 & ldata$rich < 0 & ldata$ineq  < 0 & ldata$eff > 0] <- 3
ldata$triangle[ldata$poor > 0 & ldata$rich < 0 & ldata$ineq  < 0 & ldata$eff < 0] <- 4
ldata$triangle[ldata$poor < 0 & ldata$rich < 0 & ldata$ineq  < 0] <- 5
ldata$triangle[ldata$poor < 0 & ldata$rich < 0 & ldata$ineq  > 0] <- 6
ldata$triangle[ldata$poor < 0 & ldata$rich > 0 & ldata$ineq  > 0 & ldata$eff < 0] <- 7
ldata$triangle[ldata$poor < 0 & ldata$rich > 0 & ldata$ineq  > 0 & ldata$eff > 0] <- 8
ldata$triangle[ldata$triangle == 0] <- NA

# Create two datasets, one for democrats and one for republicans
dems <- ldata[which(ldata$party == "Dem"),]
reps <- ldata[which(ldata$party == "Rep"),]

# Code the degree variables, standardize them, code absolute values
ldata$poorup <- ldata$poor
ldata$poorup[ldata$poorup < 0] <- 0

ldata$richup <- ldata$rich
ldata$richup[ldata$richup < 0] <- 0

ldata$poord <- ldata$poor
ldata$poord[ldata$poord > 0] <- 0

ldata$richd <- ldata$rich
ldata$richd[ldata$richd > 0] <- 0

ldata$ineq <- ldata$rich - ldata$poor

ldata$inequp <- ldata$ineq
ldata$inequp[ldata$inequp < 0] <- 0

ldata$ineqd <- ldata$ineq
ldata$ineqd[ldata$ineqd > 0] <- 0

ldata$effup <- ldata$eff
ldata$effup[ldata$effup < 0] <- 0

ldata$effd <- ldata$eff
ldata$effd[ldata$effd > 0] <- 0

ldata$effupz <- scale(ldata$effup, scale=T, center=T)
ldata$effdz <- scale(ldata$effd, scale=T, center=T)

ldata$poordz <- scale(ldata$poord, scale = T, center = T)
ldata$poorupz <- scale(ldata$poorup, scale = T, center = T)
ldata$richdz <- scale(ldata$richd, scale = T, center = T)
ldata$richupz <- scale(ldata$richup, scale = T, center = T)
ldata$ineqdz <- scale(ldata$ineqd, scale = T, center = T)
ldata$inequpz <- scale(ldata$inequp, scale = T, center = T)
ldata$ineqz <- scale(ldata$ineq, scale = T, center = T)

ldata$poordabs <- abs(ldata$poord)
ldata$poordabsz <- scale(ldata$poordabs, scale=T, center=T)

ldata$richdabs <- abs(ldata$richd)
ldata$richdabsz <- scale(ldata$richdabs, scale=T, center=T)

ldata$ineqdabs <- abs(ldata$ineqd)
ldata$ineqdabsz <- scale(ldata$ineqdabs, scale=T, center=T)

ldata$effdabs <- abs(ldata$effd)
ldata$effdabsz <- scale(ldata$effdabs, scale=T, center=T)

# ----------------------------------------------------------------------------------------------------------
#
# NEW TYPOLOGY UNCOVERS TRADEOFFS
#
# ----------------------------------------------------------------------------------------------------------

# Descriptive Results - Support by Octant
print(psych::describeBy(ldata$support, ldata$triangle),5)

# Chi-Square test comparing Octant 2 and 1
# Need to just get the success counts in each of the relevant octants
t1 <- ldata[which(ldata$triangle == 1),]
t2 <- ldata[which(ldata$triangle == 2),]
psych::describe(t1$support) # m = 4.48
psych::describe(t2$support) # m = 5.59

t12 <- ldata[which(ldata$triangle == 1 | ldata$triangle == 2),]
t.test(support~triangle, data = t12, var.equal = T)

# Chi-Square test comparing Octant 2 and 3
t3 <- ldata[which(ldata$triangle == 3),]
psych::describe(t3$support) # m = 5.23

t23 <- ldata[which(ldata$triangle == 2 | ldata$triangle == 3),]
t.test(support~triangle, data = t23, var.equal = T)

# Replicating the binary analyses, policy support is lower in octants 1 and 3 relative to 2.

# ----------------------------------------------------------------------------------------------------------
#
# HELPING VERSUS HURTING IS THE MOST IMPORTANT, BUT NOT ONLY, PREDICTOR OF POLICY SUPPORT
#
# ----------------------------------------------------------------------------------------------------------

# First Crossed-Factors Multilevel Model
# Need to compute the change_efficiency metric. Did not compute it before
ldata$change_eff <- 0
ldata$change_eff[which(ldata$eff > 0)] <- 1
ldata$change_eff[which(ldata$eff < 0)] <- -1
ldata$change_effz <- scale(ldata$change_eff, scale=T, center=T)

# TABLE 1 MODEL 1a
model1a <- lmer(supportz ~ change_poorz + change_richz + change_ineqz + change_effz +
                   (1 | pid) + (1 | distribution),
                 data = ldata, na.action = "na.omit",
                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1a)

# ----------------------------------------------------------------------------------------------------------
# PARTISAN DIFFERENCES
# ----------------------------------------------------------------------------------------------------------

# TABLE 1 MODEL 1b
model1b <- lmer(supportz ~ change_poorz + change_richz + change_ineqz + change_effz + rep_con +
                   (change_poorz*rep_con) + (change_richz*rep_con) + 
                   (change_ineqz*rep_con) + (change_effz*rep_con) +
                   (1 | pid) + (1 | distribution),
                 data = ldata, na.action = "na.omit",
                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1b)

# TABLE 1 MODEL 1c
model1c <- lmer(supportz ~ change_poorz + change_richz + change_ineqz + change_effz + repub +
                   (change_poorz*repub) + (change_richz*repub) + 
                   (change_ineqz*repub) + (change_effz*repub) +
                   (1 | pid) + (1 | distribution),
                 data = ldata, na.action = "na.omit",
                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1c)

# TABLE 1 MODEL 1d
model1d <- lmer(supportz ~ change_poorz + change_richz + change_ineqz + change_effz + dem +
                   (change_poorz*dem) + (change_richz*dem) + 
                   (change_ineqz*dem) + (change_effz*dem) +
                   (1 | pid) + (1 | distribution),
                 data = ldata, na.action = "na.omit",
                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1d)

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

# Comparing coefficients - Table S1
# This involves a series of z-tests within each model 1b, 1c, and 1d
# Also, need to make the coefficients all in the same direction as we are concerned with comparing
# absolute values here - just the strength of the effect not the direction

# Reverse code the z-scored inequality
ldata$ineq_rev <- -1.0245841648
ldata$ineq_rev[which(ldata$change_ineq == -1)] <- 1.0227574359
ldata$ineq_rev[which(ldata$change_ineq == 0)] <- -0.0009133645

# re-run models 1b through d
# TABLE 1 MODEL 1b - reversed
model1br <- lmer(supportz ~ change_poorz + change_richz + ineq_rev + change_effz + rep_con +
                    (change_poorz*rep_con) + (change_richz*rep_con) + 
                    (ineq_rev*rep_con) + (change_effz*rep_con) +
                    (1 | pid) + (1 | distribution),
                  data = ldata, na.action = "na.omit",
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# TABLE 1 MODEL 1c - reversed, democrats
dems <- ldata[which(ldata$party == "Dem"),]

model1cr <- lmer(supportz ~ change_poorz + change_richz + ineq_rev + change_effz +
                    (1 | pid) + (1 | distribution),
                  data = dems, na.action = "na.omit",
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# TABLE 1 MODEL 1d - reversed
reps <- ldata[which(ldata$party == "Rep"),]
model1dr <- lmer(supportz ~ change_poorz + change_richz + ineq_rev + change_effz +
                    (1 | pid) + (1 | distribution),
                  data = reps, na.action = "na.omit",
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# Z-tests comparing the coefficients in each model
# Model 1b
trends <- rbind(
  emtrends(model1br, ~1, "change_poorz"),
  emtrends(model1br, ~1, "change_richz"),
  emtrends(model1br, ~1, "ineq_rev"),
  emtrends(model1br, ~1, "change_effz")
)

# clean up so it does not error later
trends@grid$`1` <- c("change_poorz", "change_richz", "ineq_rev", "change_effz")
trends@misc$estName <- "trend"

trends

pairs(trends)

# Model 1c
trends <- rbind(
  emtrends(model1cr, ~1, "change_poorz"),
  emtrends(model1cr, ~1, "change_richz"),
  emtrends(model1cr, ~1, "ineq_rev"),
  emtrends(model1cr, ~1, "change_effz")
)

# clean up so it does not error later
trends@grid$`1` <- c("change_poorz", "change_richz", "ineq_rev", "change_effz")
trends@misc$estName <- "trend"

trends

pairs(trends)

# Model 1d
trends <- rbind(
  emtrends(model1dr, ~1, "change_poorz"),
  emtrends(model1dr, ~1, "change_richz"),
  emtrends(model1dr, ~1, "ineq_rev"),
  emtrends(model1dr, ~1, "change_effz")
)

# clean up so it does not error later
trends@grid$`1` <- c("change_poorz", "change_richz", "ineq_rev", "change_effz")
trends@misc$estName <- "trend"

trends

pairs(trends)

# ----------------------------------------------------------------------------------------------------------
# MODELS CONTROLLING FOR AGE, GENDER, INCOME, AND RACE - TABLES S2 AND S4
# ----------------------------------------------------------------------------------------------------------
# Bring in the wide data to merge in age, income, and gender by id
d <- read.csv("long_lik_controls.csv", header = T)

d$eff <- 0
d$eff <- d$rich + d$poor

d$change_eff <- 0
d$change_eff[which(d$eff > 0)] <- 1
d$change_eff[which(d$eff < 0)] <- -1
d$change_effz <- scale(d$change_eff, scale=T, center=T)

d <- d[which(d$party == "Dem" | d$party == "Rep"),]
d$rep_con <- d$repub
d$rep_con[d$rep_con == 0] <- -1


model1b_c <- lmer(supportz ~ change_poorz + change_richz + change_ineqz + change_effz +  
                     scale(age) + scale(income) + scale(male) + scale(white) + rep_con +
                     (change_poorz*rep_con) + (change_richz*rep_con) + (change_ineqz*rep_con) + (change_effz*rep_con) +
                     (1 | pid) + (1 | distribution),
                   data = d, na.action = "na.omit",
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1b_c)

# Democrats
model1c_c <- lmer(supportz ~ change_poorz + change_richz + change_ineqz + change_effz +  
                     scale(age) + scale(income) + scale(male) + scale(white) + repub +
                     (change_poorz*repub) + (change_richz*repub) + (change_ineqz*repub) + (change_effz*repub) +
                     (1 | pid) + (1 | distribution),
                   data = d, na.action = "na.omit",
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1c_c)

d$dem <- 0
d$dem[which(d$repub == 0)] <- 1

# Republicans
model1d_c <- lmer(supportz ~ change_poorz + change_richz + change_ineqz + change_effz +  
                     scale(age) + scale(income) + scale(male) + scale(white) + dem +
                     (change_poorz*dem) + (change_richz*dem) + (change_ineqz*dem) + (change_effz*dem) +
                     (1 | pid) + (1 | distribution),
                   data = d, na.action = "na.omit",
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1d_c)

# Now doing the same for the degree model, this corresponds to Table S4
d$poorup <- d$poor
d$poorup[d$poorup < 0] <- 0

d$richup <- d$rich
d$richup[d$richup < 0] <- 0

d$poord <- d$poor
d$poord[d$poord > 0] <- 0

d$richd <- d$rich
d$richd[d$richd > 0] <- 0

d$ineq <- d$rich - d$poor

d$inequp <- d$ineq
d$inequp[d$inequp < 0] <- 0

d$ineqd <- d$ineq
d$ineqd[d$ineqd > 0] <- 0

d$effup <- d$eff
d$effup[d$effup < 0] <- 0

d$effd <- d$eff
d$effd[d$effd > 0] <- 0

d$effupz <- scale(d$effup, scale=T, center=T)
d$effdz <- scale(d$effd, scale=T, center=T)

d$poordz <- scale(d$poord, scale = T, center = T)
d$poorupz <- scale(d$poorup, scale = T, center = T)
d$richdz <- scale(d$richd, scale = T, center = T)
d$richupz <- scale(d$richup, scale = T, center = T)
d$ineqdz <- scale(d$ineqd, scale = T, center = T)
d$inequpz <- scale(d$inequp, scale = T, center = T)
d$ineqz <- scale(d$ineq, scale = T, center = T)

# Put this in a different file for data analysis
d$poordabs <- abs(d$poord)
d$poordabsz <- scale(d$poordabs, scale=T, center=T)

d$richdabs <- abs(d$richd)
d$richdabsz <- scale(d$richdabs, scale=T, center=T)

d$ineqdabs <- abs(d$ineqd)
d$ineqdabsz <- scale(d$ineqdabs, scale=T, center=T)

d$effdabs <- abs(d$effd)
d$effdabsz <- scale(d$effdabs, scale=T, center=T)

model2b_c <- lmer(supportz~poorupz + poordabsz + richupz + richdabsz + 
                     scale(age) + scale(income) + scale(male) + scale(white) + rep_con +
                     (poorupz*richupz) + (poorupz*richdabsz) + (poordabsz*richupz) + (poordabsz*richdabsz) + 
                     (poorupz*rep_con)+(poordabsz*rep_con)+(richupz*rep_con)+(richdabsz*rep_con) + (poorupz*richupz*rep_con) +
                     (poorupz*richdabsz*rep_con) + (poordabsz*richupz*rep_con) + (poordabsz*richdabsz*rep_con) +
                     (1|pid) + (1|distribution),
                   data=d, na.action="na.omit",
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2b_c)

model2c_c <- lmer(supportz~poorupz + poordabsz + richupz + richdabsz + 
                     scale(age) + scale(income) + scale(male) + scale(white) + repub +
                     (poorupz*richupz) + (poorupz*richdabsz) + (poordabsz*richupz) + (poordabsz*richdabsz) + 
                     (poorupz*repub)+(poordabsz*repub)+(richupz*repub)+(richdabsz*repub) + (poorupz*richupz*repub) +
                     (poorupz*richdabsz*repub) + (poordabsz*richupz*repub) + (poordabsz*richdabsz*repub) +
                     (1|pid) + (1|distribution),
                   data=d, na.action="na.omit",
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2c_c)

model2d_c <- lmer(supportz~poorupz + poordabsz + richupz + richdabsz + 
                     scale(age) + scale(income) + scale(male) + scale(white) + dem +
                     (poorupz*richupz) + (poorupz*richdabsz) + (poordabsz*richupz) + (poordabsz*richdabsz) + 
                     (poorupz*dem)+(poordabsz*dem)+(richupz*dem)+(richdabsz*dem) + (poorupz*richupz*dem) +
                     (poorupz*richdabsz*dem) + (poordabsz*richupz*dem) + (poordabsz*richdabsz*dem) +
                     (1|pid) + (1|distribution),
                   data=d, na.action="na.omit",
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2d_c)

# ----------------------------------------------------------------------------------------------------------
#
# DIFFERENTIATING HELP FROM HARM
#
# ----------------------------------------------------------------------------------------------------------

# Taking Degree of change into Account
# These Models Correspond to Table S3
model2a <- lmer(supportz~poorupz+poordabsz+richupz+richdabsz +
                   (1|pid) + (1|distribution),
                 data=ldata, na.action="na.omit",
                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2a)

# Model 2b
model2b <- lmer(supportz~poorupz + poordabsz + richupz + richdabsz + rep_con +
                   (poorupz*richupz) + (poorupz*richdabsz) + (poordabsz*richupz) + (poordabsz*richdabsz) + 
                   (poorupz*rep_con)+(poordabsz*rep_con)+(richupz*rep_con)+(richdabsz*rep_con) + (poorupz*richupz*rep_con) +
                   (poorupz*richdabsz*rep_con) + (poordabsz*richupz*rep_con) + (poordabsz*richdabsz*rep_con) +
                   (1|pid) + (1|distribution),
                 data=ldata, na.action="na.omit",
                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2b)

# Model 2c - Democrats
model2c <- lmer(supportz~poorupz + poordabsz + richupz + richdabsz + repub +
                   (poorupz*richupz) + (poorupz*richdabsz) + (poordabsz*richupz) + (poordabsz*richdabsz) + 
                   (poorupz*repub)+(poordabsz*repub)+(richupz*repub)+(richdabsz*repub) + (poorupz*richupz*repub) +
                   (poorupz*richdabsz*repub) + (poordabsz*richupz*repub) + (poordabsz*richdabsz*repub) +
                   (1|pid) + (1|distribution),
                 data=ldata, na.action="na.omit",
                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2c)

# Model 2d - Republicans
model2d <- lmer(supportz~poorupz + poordabsz + richupz + richdabsz + dem +
                   (poorupz*richupz) + (poorupz*richdabsz) + (poordabsz*richupz) + (poordabsz*richdabsz) + 
                   (poorupz*dem)+(poordabsz*dem)+(richupz*dem)+(richdabsz*dem) + (poorupz*richupz*dem) +
                   (poorupz*richdabsz*dem) + (poordabsz*richupz*dem) + (poordabsz*richdabsz*dem) +
                   (1|pid) + (1|distribution),
                 data=ldata, na.action="na.omit",
                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2d)

# ----------------------------------------------------------------------------------------------------------
#
# IN MAKING TRADE-OFFS TO HELP THE POOR, REPUBLICANS TOLERATE GREATER INCREASES TO INEQUALITY THAN DEMOCRATS
#
# ----------------------------------------------------------------------------------------------------------

# Poor Up and Inequality Up (puiu) data - Zooming in
puiu <- ldata[which(ldata$poorup > 0 & ldata$inequp > 0),]

# These models correspond to the data in Table S5
m_puiu <- lmer(supportz ~ poorupz + inequpz + rep_con + (poorupz*inequpz) + (poorupz*rep_con) + (inequpz*rep_con) +
                  (poorupz*inequpz*rep_con) + (1|pid),
                data=puiu, na.action="na.omit",
                control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m_puiu)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(m_puiu)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# Democrats
m_puiu_d <- lmer(supportz ~ poorupz + inequpz + repub + (poorupz*inequpz) + (poorupz*repub) + (inequpz*repub) +
                    (poorupz*inequpz*repub) + (1|pid),
                  data=puiu, na.action="na.omit",
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m_puiu_d)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(m_puiu_d)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# Republicans
m_puiu_r <- lmer(supportz ~ poorupz + inequpz + dem + (poorupz*inequpz) + (poorupz*dem) + (inequpz*dem) +
                    (poorupz*inequpz*dem) + (1|pid),
                  data=puiu, na.action="na.omit",
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m_puiu_r)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(m_puiu_r)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))


# For ease of interpreting, I'm going to use the non-standardized model here. All is the same just makes the 
# ggemmeans output easier to interpret (e.g., find when inequality is up by 15% and poor are up bu 5%)
m_puiu <- lmer(supportz ~ poorup + inequp + rep_con + (poorup*inequp) + (poorup*rep_con) + (inequp*rep_con) +
                  (poorup*inequp*rep_con) + (1|pid),
                data=puiu, na.action="na.omit",
                control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m_puiu)

# Get percentages predicted
ggeffects::ggemmeans(m_puiu, terms=c("poorup", "inequp", "rep_con"))


# ----------------------------------------------------------------------------------------------------------
#
# FIGURES
#
# ----------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------
# Figure 1a-d
# ----------------------------------------------------------------------------------------------------------

# Heatmaps for Figure 1 can be found in the Heatmaps code file

# ----------------------------------------------------------------------------------------------------------
# Figure 2a-d
# ----------------------------------------------------------------------------------------------------------


# Change Party into a factor with the right names, this just maes the figures easier to make
# I also cleaned up the axis labels and stuff a bit in the PDF editor after exporting the figures.
# Easier to do that there instead.
ldata$party_st <- "Democrat"
ldata$party_st[which(ldata$repub == 1)] <- "Republican"
ldata$party_st <- as.factor(ldata$party_st)

fig_mod <- lmer(support ~ change_poor + change_rich + change_ineq + change_eff + party_st + 
                   (change_poor*party_st) + (change_rich*party_st) + (change_ineq*party_st) + (change_eff*party_st) + 
                   (1 | pid) + (1 | distribution),
                 data = ldata, na.action = "na.omit",
                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fig_mod)

a <- plot_model(fig_mod, type="pred", terms = c("change_poor", "party_st"), ci.lvl = .95, colors = c("#232066","#E91D0E"),
                legend.title = "Party") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  ggtitle("Policy Support by Effect on the Poor") +
  geom_hline(yintercept = .5, linetype = "longdash") + 
  xlab("How the policy affects the poor (-1 = hurts; 0 = no change; 1 = helps)") + ylab("Policy Support")
a


b <- plot_model(fig_mod, type="pred", terms = c("change_rich", "party_st"), ci.lvl = .95, colors = c("#232066","#E91D0E"),
                legend.title = "Party") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  ggtitle("Policy Support by Effect on the Rich") + 
  geom_hline(yintercept = .5, linetype = "longdash") + 
  xlab("How the policy affects the rich (-1 = hurts; 0 = no change; 1 = helps)") + ylab("Policy Support")

c <- plot_model(fig_mod, type="pred", terms = c("change_ineq", "party_st"), ci.lvl = .95, colors = c("#232066","#E91D0E"),
                legend.title = "Party") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  ggtitle("Policy Support by Effect on Inequality") + 
  geom_hline(yintercept = .5, linetype = "longdash") + 
  xlab("How the policy affects inequality (-1 = lowers; 0 = no change; 1 = raises)") + ylab("Policy Support")

d <- plot_model(fig_mod, type="pred", terms = c("change_eff", "party_st"), ci.lvl = .95, colors = c("#232066","#E91D0E"),
                legend.title = "Party") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  ggtitle("Policy Support by Effect on Net Gain/Loss") + 
  geom_hline(yintercept = .5, linetype = "longdash") + 
  xlab("How the policy affects net gain/loss (-1 = Net Loss; 0 = no change; 1 = Net Gain)") + ylab("Policy Support")

ggpubr::ggarrange(a, b, c, d, labels = c("a", "b", "c", "d"),
          ncol = 2, nrow = 2)

# ----------------------------------------------------------------------------------------------------------
# Figure 3
# ----------------------------------------------------------------------------------------------------------

# Here I just took the absolute values of the Beta Weights from Table 1, as well as their confidence
# Intervals, and then plotted them manually.

# Confidence intervals
# Model 1b
coeftbl <- as.data.frame(coef(summary(model1b)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# Model 1c
coeftbl <- as.data.frame(coef(summary(model1c)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# Model 1d
coeftbl <- as.data.frame(coef(summary(model1d)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# Reversing all the coefficients for models 1b-d and 2b-d and plotting with CIs.
coefs <- c(.526,.051,.145,.095,.612,.023,.184,.079,.440,.126,.106,.111)
low <- c(.504,.029,.123,.073,.588,-.001,.160,.055,.416,.102,.083,.087)
high <- c(.548,.073,.167,.117,.636,.047,.208,.103,.464,.150,.130,.135)
Outcome <- c("Help (vs. Harm)\nPoor", "Help (vs. Harm)\nRich", "Reduce (vs. Increase)\nInequality", "Increase (vs. Reduce)\nEfficiency",
             "Help (vs. Harm)\nPoor", "Help (vs. Harm)\nRich", "Reduce (vs. Increase)\nInequality", "Increase (vs. Reduce)\nEfficiency",
             "Help (vs. Harm)\nPoor", "Help (vs. Harm)\nRich", "Reduce (vs. Increase)\nInequality", "Increase (vs. Reduce)\nEfficiency")
Model <- c("Model 1b: Full Model", "Model 1b: Full Model", "Model 1b: Full Model", "Model 1b: Full Model",
           "Model 1c: Democrats", "Model 1c: Democrats", "Model 1c: Democrats", "Model 1c: Democrats",
           "Model 1d: Republicans", "Model 1d: Republicans", "Model 1d: Republicans", "Model 1d: Republicans")
Model2 <- c("Overall", "Overall", "Overall", "Overall", 
            "Democrats", "Democrats", "Democrats", "Democrats", 
            "Republicans", "Republicans", "Republicans", "Republicans")

listoflists=list(coefs,low,high,Outcome,Model,Model2)

coef_dat <- as.data.frame(listoflists, col.names = c("coef", "low", "high", "Outcome", "Model", "Model2"))

coef_dat$Model2 <- factor(coef_dat$Model2, levels = c("Overall", "Democrats", "Republicans"))

coef_dat$Outcome <- factor(coef_dat$Outcome,levels = c("Help (vs. Harm)\nPoor", "Help (vs. Harm)\nRich", "Reduce (vs. Increase)\nInequality", "Increase (vs. Reduce)\nEfficiency"))

ggplot(data = coef_dat, aes(Outcome, coef, color = Model2)) + geom_point(size = 4, position = position_dodge(width = .8)) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2, position = position_dodge(width = .8)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  ylab("Effect Size (Beta)") + 
  xlab("Main Effect") +
  theme_bw(base_size = 18) + theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
                                   plot.title = element_text(hjust = 0.5)) +
  scale_color_manual("Group", values = c("Overall" = "Chartreuse3", "Democrats" = "#232066", "Republicans" = "#E91D0E"))



# ----------------------------------------------------------------------------------------------------------
# Figure 4
# ----------------------------------------------------------------------------------------------------------

# Re-reunning the poor up and inequality up model without z-scores. Again, patterns all identical
# Just makes the axes for the figure interpretable.
puiu$party_st <- "Democrat"
puiu$party_st[which(puiu$repub == 1)] <- "Republican"
puiu$party_st <- as.factor(puiu$party_st)

m <- lmer(support ~ poorup + inequp + party_st + (poorup*inequp) + (poorup*party_st) + (inequp*party_st) +
             (poorup*inequp*party_st) + (1|pid) + (1|distribution),
           data=puiu, na.action="na.omit",
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m)

# This version of the figure has a few extra dots that I removed after the fact
# Basically, could not do seperate dots for each facet. So I put both all the dots on each side.
# Then, removed the Republican Dots from the Democrat side and vice versa
plot_model(model=m, type="pred", terms=c("inequp [all]", "poorup [all]", "party_st"), legend.title = "Poor Help",
           colors = "Spectral") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) + ggtitle(NULL) +
  xlab("Degree of Increase to Inequality") + ylab("Predicted Policy Support") + 
  geom_hline(yintercept = 4.5, linetype = "longdash")






