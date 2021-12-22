# ------------------------------------------------------------------------------------------
#
# Typology of Political Policy Support - Dollars Replication
# 
# Jan 19th, 2020
#
# ------------------------------------------------------------------------------------------

# Read in the data
setwd("CHANGE TO DIRECTORY DATA IS STORED")

bdata <- read.csv("long_bin.csv", header=T)

# Libraries
library(emmeans)
library(psych)
library(lme4)
library(lmerTest)
library(sjPlot)

# Remove few rows without political ideology (results do not change)
bdata <- bdata[which(bdata$party == "Rep" | bdata$party == "Dem"),]
# Contrast code political ideology
bdata$rep_con <- bdata$repub
bdata$rep_con[bdata$rep_con == 0] <- -1

# Code for Republicans for Simple Slopes
bdata$dem <- 0
bdata$dem[which(bdata$repub == 0)] <- 1

# Code in the triangles
bdata$ineq <- 0
bdata$ineq <- bdata$rich - bdata$poor
bdata$eff <- 0
bdata$eff <- bdata$rich + bdata$poor

bdata$triangle <- 0
bdata$triangle[bdata$poor > 0 & bdata$rich > 0 & bdata$ineq  > 0] <- 1
bdata$triangle[bdata$poor > 0 & bdata$rich > 0 & bdata$ineq  < 0] <- 2
bdata$triangle[bdata$poor > 0 & bdata$rich < 0 & bdata$ineq  < 0 & bdata$eff > 0] <- 3
bdata$triangle[bdata$poor > 0 & bdata$rich < 0 & bdata$ineq  < 0 & bdata$eff < 0] <- 4
bdata$triangle[bdata$poor < 0 & bdata$rich < 0 & bdata$ineq  < 0] <- 5
bdata$triangle[bdata$poor < 0 & bdata$rich < 0 & bdata$ineq  > 0] <- 6
bdata$triangle[bdata$poor < 0 & bdata$rich > 0 & bdata$ineq  > 0 & bdata$eff < 0] <- 7
bdata$triangle[bdata$poor < 0 & bdata$rich > 0 & bdata$ineq  > 0 & bdata$eff > 0] <- 8
bdata$triangle[bdata$triangle == 0] <- NA

# Create two datasets, one for democrats and one for republicans
dems <- bdata[which(bdata$party == "Dem"),]
reps <- bdata[which(bdata$party == "Rep"),]

# Code the degree variables, standardize them, code absolute values
bdata$poorup <- bdata$poor
bdata$poorup[bdata$poorup < 0] <- 0

bdata$richup <- bdata$rich
bdata$richup[bdata$richup < 0] <- 0

bdata$poord <- bdata$poor
bdata$poord[bdata$poord > 0] <- 0

bdata$richd <- bdata$rich
bdata$richd[bdata$richd > 0] <- 0

bdata$ineq <- bdata$rich - bdata$poor

bdata$inequp <- bdata$ineq
bdata$inequp[bdata$inequp < 0] <- 0

bdata$ineqd <- bdata$ineq
bdata$ineqd[bdata$ineqd > 0] <- 0

bdata$effup <- bdata$eff
bdata$effup[bdata$effup < 0] <- 0

bdata$effd <- bdata$eff
bdata$effd[bdata$effd > 0] <- 0

bdata$effupz <- scale(bdata$effup, scale=T, center=T)
bdata$effdz <- scale(bdata$effd, scale=T, center=T)

bdata$poordz <- scale(bdata$poord, scale = T, center = T)
bdata$poorupz <- scale(bdata$poorup, scale = T, center = T)
bdata$richdz <- scale(bdata$richd, scale = T, center = T)
bdata$richupz <- scale(bdata$richup, scale = T, center = T)
bdata$ineqdz <- scale(bdata$ineqd, scale = T, center = T)
bdata$inequpz <- scale(bdata$inequp, scale = T, center = T)
bdata$ineqz <- scale(bdata$ineq, scale = T, center = T)

bdata$poordabs <- abs(bdata$poord)
bdata$poordabsz <- scale(bdata$poordabs, scale=T, center=T)

bdata$richdabs <- abs(bdata$richd)
bdata$richdabsz <- scale(bdata$richdabs, scale=T, center=T)

bdata$ineqdabs <- abs(bdata$ineqd)
bdata$ineqdabsz <- scale(bdata$ineqdabs, scale=T, center=T)

bdata$effdabs <- abs(bdata$effd)
bdata$effdabsz <- scale(bdata$effdabs, scale=T, center=T)

# ----------------------------------------------------------------------------------------------------------
#
# NEW TYPOLOGY UNCOVERS TRADEOFFS
#
# ----------------------------------------------------------------------------------------------------------

# Descriptive Results - Support by Octant
print(psych::describeBy(bdata$support, bdata$triangle),5)

# Chi-Square test comparing Octant 2 and 1
# Need to just get the success counts in each of the relevant octants
t1 <- bdata[which(bdata$triangle == 1),]
t2 <- bdata[which(bdata$triangle == 2),]
plyr::count(t1$support) # 2,203 yes votes out of 2925
plyr::count(t2$support) # 2679 yes votes out of 2932
succ <- c(2203, 2679)
totals <- c(2925, 2932)


prop.test(succ, totals, p = NULL, alternative = "two.sided",
          correct = TRUE)

# Chi-Square test comparing Octant 2 and 3
t3 <- bdata[which(bdata$triangle == 3),]
plyr::count(t3$support) # 2431 yes votes out of 2934
succ <- c(2431, 2679)
totals <- c(2934, 2932)


prop.test(succ, totals, p = NULL, alternative = "two.sided",
          correct = TRUE)

# ----------------------------------------------------------------------------------------------------------
#
# HELPING VERSUS HURTING IS THE MOST IMPORTANT, BUT NOT ONLY, PREDICTOR OF POLICY SUPPORT
#
# ----------------------------------------------------------------------------------------------------------

# First Crossed-Factors Multilevel Model
# Need to compute the change_efficiency metric. Did not compute it before
bdata$change_eff <- 0
bdata$change_eff[which(bdata$eff > 0)] <- 1
bdata$change_eff[which(bdata$eff < 0)] <- -1
bdata$change_effz <- scale(bdata$change_eff, scale=T, center=T)

# TABLE 1 MODEL 1a
model1a <- glmer(support ~ change_poorz + change_richz + change_ineqz + change_effz +
                   (1 | pid) + (1 | distribution),
                 data = bdata, family=binomial, na.action = "na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1a)

# ----------------------------------------------------------------------------------------------------------
# PARTISAN DIFFERENCES
# ----------------------------------------------------------------------------------------------------------

# TABLE 1 MODEL 1b
model1b <- glmer(support ~ change_poorz + change_richz + change_ineqz + change_effz + rep_con +
                   (change_poorz*rep_con) + (change_richz*rep_con) + 
                   (change_ineqz*rep_con) + (change_effz*rep_con) +
                   (1 | pid) + (1 | distribution),
                 data = bdata, family=binomial, na.action = "na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1b)

# TABLE 1 MODEL 1c
model1c <- glmer(support ~ change_poorz + change_richz + change_ineqz + change_effz + repub +
                   (change_poorz*repub) + (change_richz*repub) + 
                   (change_ineqz*repub) + (change_effz*repub) +
                   (1 | pid) + (1 | distribution),
                 data = bdata, family=binomial, na.action = "na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1c)

# TABLE 1 MODEL 1d
model1d <- glmer(support ~ change_poorz + change_richz + change_ineqz + change_effz + dem +
                   (change_poorz*dem) + (change_richz*dem) + 
                   (change_ineqz*dem) + (change_effz*dem) +
                   (1 | pid) + (1 | distribution),
                 data = bdata, family=binomial, na.action = "na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
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
bdata$ineq_rev <- -1.02473073
bdata$ineq_rev[which(bdata$change_ineq == -1)] <- 1.02201867
bdata$ineq_rev[which(bdata$change_ineq == 0)] <- -0.00135603 

# re-run models 1b through d
# TABLE 1 MODEL 1b - reversed
model1br <- glmer(support ~ change_poorz + change_richz + ineq_rev + change_effz + rep_con +
                    (change_poorz*rep_con) + (change_richz*rep_con) + 
                    (ineq_rev*rep_con) + (change_effz*rep_con) +
                    (1 | pid) + (1 | distribution),
                  data = bdata, family=binomial, na.action = "na.omit",
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# TABLE 1 MODEL 1c - reversed, democrats
dems <- bdata[which(bdata$party == "Dem"),]

model1cr <- glmer(support ~ change_poorz + change_richz + ineq_rev + change_effz +
                    (1 | pid) + (1 | distribution),
                  data = dems, family=binomial, na.action = "na.omit",
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# TABLE 1 MODEL 1d - reversed
reps <- bdata[which(bdata$party == "Rep"),]
model1dr <- glmer(support ~ change_poorz + change_richz + ineq_rev + change_effz +
                    (1 | pid) + (1 | distribution),
                  data = reps, family=binomial, na.action = "na.omit",
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

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
#
# DIFFERENTIATING HELP FROM HARM
#
# ----------------------------------------------------------------------------------------------------------

# Taking Degree of change into Account
# These Models Correspond to Table S3
model2a <- glmer(support~poorupz+poordabsz+richupz+richdabsz +
                   (1|pid) + (1|distribution),
                 data=bdata, family = "binomial", na.action="na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2a)

# Model 2b
model2b <- glmer(support~poorupz + poordabsz + richupz + richdabsz + rep_con +
                   (poorupz*richupz) + (poorupz*richdabsz) + (poordabsz*richupz) + (poordabsz*richdabsz) + 
                   (poorupz*rep_con)+(poordabsz*rep_con)+(richupz*rep_con)+(richdabsz*rep_con) + (poorupz*richupz*rep_con) +
                   (poorupz*richdabsz*rep_con) + (poordabsz*richupz*rep_con) + (poordabsz*richdabsz*rep_con) +
                   (1|pid) + (1|distribution),
                 data=bdata, family = "binomial", na.action="na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2b)

# Model 2c - Democrats
model2c <- glmer(support~poorupz + poordabsz + richupz + richdabsz + repub +
                   (poorupz*richupz) + (poorupz*richdabsz) + (poordabsz*richupz) + (poordabsz*richdabsz) + 
                   (poorupz*repub)+(poordabsz*repub)+(richupz*repub)+(richdabsz*repub) + (poorupz*richupz*repub) +
                   (poorupz*richdabsz*repub) + (poordabsz*richupz*repub) + (poordabsz*richdabsz*repub) +
                   (1|pid) + (1|distribution),
                 data=bdata, family = "binomial", na.action="na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2c)

# Model 2d - Republicans
model2d <- glmer(support~poorupz + poordabsz + richupz + richdabsz + dem +
                   (poorupz*richupz) + (poorupz*richdabsz) + (poordabsz*richupz) + (poordabsz*richdabsz) + 
                   (poorupz*dem)+(poordabsz*dem)+(richupz*dem)+(richdabsz*dem) + (poorupz*richupz*dem) +
                   (poorupz*richdabsz*dem) + (poordabsz*richupz*dem) + (poordabsz*richdabsz*dem) +
                   (1|pid) + (1|distribution),
                 data=bdata, family = "binomial", na.action="na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2d)

# ----------------------------------------------------------------------------------------------------------
#
# IN MAKING TRADE-OFFS TO HELP THE POOR, REPUBLICANS TOLERATE GREATER INCREASES TO INEQUALITY THAN DEMOCRATS
#
# ----------------------------------------------------------------------------------------------------------

# Poor Up and Inequality Up (puiu) data - Zooming in
puiu <- bdata[which(bdata$poorup > 0 & bdata$inequp > 0),]

# These models correspond to the data in Table S5
m_puiu <- glmer(support ~ poorupz + inequpz + rep_con + (poorupz*inequpz) + (poorupz*rep_con) + (inequpz*rep_con) +
                  (poorupz*inequpz*rep_con) + (1|pid),
                data=puiu, family = "binomial", na.action="na.omit",
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m_puiu)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(m_puiu)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# Democrats
m_puiu_d <- glmer(support ~ poorupz + inequpz + repub + (poorupz*inequpz) + (poorupz*repub) + (inequpz*repub) +
                    (poorupz*inequpz*repub) + (1|pid),
                  data=puiu, family = "binomial", na.action="na.omit",
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m_puiu_d)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(m_puiu_d)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))

# Republicans
m_puiu_r <- glmer(support ~ poorupz + inequpz + dem + (poorupz*inequpz) + (poorupz*dem) + (inequpz*dem) +
                    (poorupz*inequpz*dem) + (1|pid),
                  data=puiu, family = "binomial", na.action="na.omit",
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m_puiu_r)

# Confidence intervals for the main text
coeftbl <- as.data.frame(coef(summary(m_puiu_r)))
with(coeftbl,
     +      Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))


# For ease of interpreting, I'm going to use the non-standardized model here. All is the same just makes the 
# ggemmeans output easier to interpret (e.g., find when inequality is up by 15% and poor are up bu 5%)
m_puiu <- glmer(support ~ poorup + inequp + rep_con + (poorup*inequp) + (poorup*rep_con) + (inequp*rep_con) +
                  (poorup*inequp*rep_con) + (1|pid),
                data=puiu, family = "binomial", na.action="na.omit",
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
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
bdata$party_st <- "Democrat"
bdata$party_st[which(bdata$repub == 1)] <- "Republican"
bdata$party_st <- as.factor(bdata$party_st)

fig_mod <- glmer(support ~ change_poor + change_rich + change_ineq + change_eff + party_st + 
                   (change_poor*party_st) + (change_rich*party_st) + (change_ineq*party_st) + (change_eff*party_st) + 
                   (1 | pid) + (1 | distribution),
                 data = bdata, family=binomial, na.action = "na.omit",
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fig_mod)

a <- plot_model(fig_mod, type="pred", terms = c("change_poor", "party_st"), ci.lvl = .95, colors = c("#232066","#E91D0E"),
                legend.title = "Party") + geom_segment(aes(x = -1, y = .10, xend = 0, yend = .38), color = "chartreuse3") +
  geom_segment(aes(x = 0, y = .38, xend = 1, yend = .76), color = "chartreuse3") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  ggtitle("Policy Support by Effect on the Poor") +
  geom_hline(yintercept = .5, linetype = "longdash") + 
  ylim(0,1) +
  xlab("How the policy affects the poor (-1 = hurts; 0 = no change; 1 = helps)") + ylab("Policy Support")
a


b <- plot_model(fig_mod, type="pred", terms = c("change_rich", "party_st"), ci.lvl = .95, colors = c("#232066","#E91D0E"),
                legend.title = "Party") + geom_segment(aes(x = -1, y = .33, xend = 0, yend = .38), color = "chartreuse3") +
  geom_segment(aes(x = 0, y = .38, xend = 1, yend = .43), color = "chartreuse3") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  ggtitle("Policy Support by Effect on the Rich") + 
  geom_hline(yintercept = .5, linetype = "longdash") + 
  ylim(0,1) +
  xlab("How the policy affects the rich (-1 = hurts; 0 = no change; 1 = helps)") + ylab("Policy Support")

c <- plot_model(fig_mod, type="pred", terms = c("change_ineq", "party_st"), ci.lvl = .95, colors = c("#232066","#E91D0E"),
                legend.title = "Party") + geom_segment(aes(x = -1, y = .50, xend = 0, yend = .38), color = "chartreuse3") +
  geom_segment(aes(x = 0, y = .38, xend = 1, yend = .27), color = "chartreuse3") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  ggtitle("Policy Support by Effect on Inequality") + 
  geom_hline(yintercept = .5, linetype = "longdash") + 
  ylim(0,1) +
  xlab("How the policy affects inequality (-1 = lowers; 0 = no change; 1 = raises)") + ylab("Policy Support")

d <- plot_model(fig_mod, type="pred", terms = c("change_eff", "party_st"), ci.lvl = .95, colors = c("#232066","#E91D0E"),
                legend.title = "Party") + geom_segment(aes(x = -1, y = .31, xend = 0, yend = .38), color = "chartreuse3") +
  geom_segment(aes(x = 0, y = .38, xend = 1, yend = .45), color = "chartreuse3") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  ggtitle("Policy Support by Effect on Net Gain/Loss") + 
  geom_hline(yintercept = .5, linetype = "longdash") + 
  ylim(0,1) +
  xlab("How the policy affects net gain/loss (-1 = Net Loss; 0 = no change; 1 = Net Gain)") + ylab("Policy Support")

ggarrange(a, b, c, d, labels = c("a", "b", "c", "d"),
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
coefs <- c(1.645,0.219,0.485,0.311,1.906,0.076,0.593,0.293,1.382,0.514,0.376,0.327)
low <- c(1.565,0.137,0.404,0.231,1.813,-0.022,0.501,0.198,1.292,0.423,0.283,0.241)
high <- c(1.725,0.301,0.565,0.390,2.000,0.174,0.684,0.389,1.471,0.606,0.468,0.412)
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

m <- glmer(support ~ poorup + inequp + party_st + (poorup*inequp) + (poorup*party_st) + (inequp*party_st) +
             (poorup*inequp*party_st) + (1|pid) + (1|distribution),
           data=puiu, family = "binomial", na.action="na.omit",
           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
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
  geom_hline(yintercept = .5, linetype = "longdash") +
  geom_point(aes(x=40, y=.38)) + 
  geom_point(aes(x=35, y=.61)) + 
  geom_point(aes(x=30, y=.78)) + 
  geom_point(aes(x=25, y=.88)) +
  geom_point(aes(x=20, y=.93)) +
  geom_point(aes(x=15, y=.95)) +
  geom_point(aes(x=10, y=.965)) +
  geom_point(aes(x=5, y=.97)) +
  geom_point(aes(x=40, y=.695)) +
  geom_point(aes(x=35, y=.855)) +
  geom_point(aes(x=30, y=.93)) +
  geom_point(aes(x=25, y=.96)) +
  geom_point(aes(x=20, y=.975)) + 
  geom_point(aes(x=15, y=.98)) + 
  geom_point(aes(x=10, y=.9825)) +
  geom_point(aes(x=5, y=.98))





