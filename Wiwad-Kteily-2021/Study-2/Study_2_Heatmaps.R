# ------------------------------------------------------------------------------------------
#
# Typology of Political Policy Support - Density Plots and Heat Maps
# 
# Jan 19th, 2020
#
# ------------------------------------------------------------------------------------------

setwd("CHANGE TO WHERE YOU STORED THE DATA")

bdata <- read.csv("long_bin.csv", header=T)

library(ggplot2)
library(Rmisc)
library(lattice)
library(viridis)

# Couple people have zero on party, so just gonna get rid of them (1 person I think)
bdata <- bdata[which(bdata$party == "Rep" | bdata$party == "Dem"),]

overall_sum <- summarySE(bdata, measurevar="support", 
                         groupvars=c("distribution", "party"), na.rm=T)

# Annoying shit, split into Rs and Ds
sum_r <- overall_sum[which(overall_sum$party == "Rep"),]
sum_d <- overall_sum[which(overall_sum$party == "Dem"),]

# All the distinct distributions for recoding
pdata <- bdata[which(bdata$party == "Rep" | bdata$party == "Dem"),]
ddata <- subset(pdata, !duplicated(distribution))
cols <- c(78,82:90)
ddata <- ddata[cols]

sum_r <- merge(sum_r,ddata,by="distribution")
sum_d <- merge(sum_d,ddata,by="distribution")

# Recombine, now I can graph with it.
overall_sum <- rbind(sum_r, sum_d)
overall_sum$help_poor <- as.factor(overall_sum$help_poor)
overall_sum$help_rich <- as.factor(overall_sum$help_rich)
overall_sum$hurt_poor <- as.factor(overall_sum$hurt_poor)
overall_sum$hurt_rich <- as.factor(overall_sum$hurt_rich)
overall_sum$inc_ineq <- as.factor(overall_sum$inc_ineq)
overall_sum$dec_ineq <- as.factor(overall_sum$dec_ineq)
overall_sum$change_poor <- as.factor(overall_sum$change_poor)
overall_sum$change_rich <- as.factor(overall_sum$change_rich)
overall_sum$change_ineq <- as.factor(overall_sum$change_ineq)

overall_sum$Poor <- 0
overall_sum$Rich <- 0


for(i in 1:nrow(overall_sum)){
  if(grepl("35p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 35
  }
  if(grepl("30p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 30
  }
  if(grepl("25p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 25
  }
  if(grepl("20p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 20
  }
  if(grepl("15p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 15
  }
  if(grepl("10p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 10
  }
  if(grepl("05p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 5
  }
  if(grepl("p.nc", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 0
  }
  if(grepl("35p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -35
  }
  if(grepl("30p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -30
  }
  if(grepl("25p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -25
  }
  if(grepl("20p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -20
  }
  if(grepl("15p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -15
  }
  if(grepl("10p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -10
  }
  if(grepl("05p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -5
  }
}


for(i in 1:nrow(overall_sum)){
  if(grepl("35r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 35
  }
  if(grepl("30r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 30
  }
  if(grepl("25r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 25
  }
  if(grepl("20r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 20
  }
  if(grepl("15r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 15
  }
  if(grepl("10r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 10
  }
  if(grepl("05r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 5
  }
  if(grepl("r.nc", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 0
  }
  if(grepl("35r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -35
  }
  if(grepl("30r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -30
  }
  if(grepl("25r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -25
  }
  if(grepl("20r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -20
  }
  if(grepl("15r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -15
  }
  if(grepl("10r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -10
  }
  if(grepl("05r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -5
  }
}

# Heatmap
require(lattice)
library(viridis)

# Annoying shit, split into Rs and Ds
sum_r <- overall_sum[which(overall_sum$party == "Rep"),]
sum_d <- overall_sum[which(overall_sum$party == "Dem"),]

levelplot(support~poor*rich, data=sum_d, col.regions = inferno(100),
          main=list('Policy Support Among Democrats',side=1,line=0.5),
          xlab=list(label="Poor"),
          ylab=list(label="Rich"),
          panel = function(...){
            panel.levelplot(...)
            panel.abline(h = 0, lwd = 4)
            panel.abline(v = 0, lwd = 4)
            panel.abline(c(0,1), lwd=2)
            panel.abline(c(0,-1), lwd=2)
          })

levelplot(support~poor*rich, data=sum_r, col.regions = inferno(100),
          main=list('Policy Support Among Republicans',side=1,line=0.5),
          xlab=list(label="Poor"),
          ylab=list(label="Rich"),
          panel = function(...){
            panel.levelplot(...)
            panel.abline(h = 0, lwd = 4)
            panel.abline(v = 0, lwd = 4)
            panel.abline(c(0,1), lwd=2)
            panel.abline(c(0,-1), lwd=2)
          })

# Looking at the heatmaps with Bernie and Biden

bdata <- bdata[which(bdata$party == "Dem"),]
rdem <- bdata[which(bdata$reg_dem == 1),]
rdem <- rdem[which(rdem$who_vote == 1 | rdem$who_vote == 2),]

rdem$vote <- "Bernie"
rdem$vote[which(rdem$who_vote == 1)] <- "Biden"

overall_sum <- summarySE(rdem, measurevar="support", 
                         groupvars=c("distribution", "vote"), na.rm=T)

# Annoying shit, split into Rs and Ds
sum_bern <- overall_sum[which(overall_sum$vote == "Bernie"),]
sum_bid <- overall_sum[which(overall_sum$vote == "Biden"),]

# All the distinct distributions for recoding
pdata <- bdata[which(bdata$who_vote == 1 | bdata$who_vote == 1),]
ddata <- subset(bdata, !duplicated(distribution))
cols <- c(78,82:90)
ddata <- ddata[cols]

sum_bern <- merge(sum_bern,ddata,by="distribution")
sum_bid <- merge(sum_bid,ddata,by="distribution")

# Recombine, now I can graph with it.
overall_sum <- rbind(sum_bern, sum_bid)
overall_sum$help_poor <- as.factor(overall_sum$help_poor)
overall_sum$help_rich <- as.factor(overall_sum$help_rich)
overall_sum$hurt_poor <- as.factor(overall_sum$hurt_poor)
overall_sum$hurt_rich <- as.factor(overall_sum$hurt_rich)
overall_sum$inc_ineq <- as.factor(overall_sum$inc_ineq)
overall_sum$dec_ineq <- as.factor(overall_sum$dec_ineq)
overall_sum$change_poor <- as.factor(overall_sum$change_poor)
overall_sum$change_rich <- as.factor(overall_sum$change_rich)
overall_sum$change_ineq <- as.factor(overall_sum$change_ineq)

overall_sum$Poor <- 0
overall_sum$Rich <- 0


for(i in 1:nrow(overall_sum)){
  if(grepl("35p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 35
  }
  if(grepl("30p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 30
  }
  if(grepl("25p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 25
  }
  if(grepl("20p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 20
  }
  if(grepl("15p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 15
  }
  if(grepl("10p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 10
  }
  if(grepl("05p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 5
  }
  if(grepl("p.nc", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 0
  }
  if(grepl("35p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -35
  }
  if(grepl("30p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -30
  }
  if(grepl("25p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -25
  }
  if(grepl("20p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -20
  }
  if(grepl("15p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -15
  }
  if(grepl("10p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -10
  }
  if(grepl("05p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -5
  }
}


for(i in 1:nrow(overall_sum)){
  if(grepl("35r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 35
  }
  if(grepl("30r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 30
  }
  if(grepl("25r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 25
  }
  if(grepl("20r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 20
  }
  if(grepl("15r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 15
  }
  if(grepl("10r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 10
  }
  if(grepl("05r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 5
  }
  if(grepl("r.nc", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 0
  }
  if(grepl("35r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -35
  }
  if(grepl("30r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -30
  }
  if(grepl("25r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -25
  }
  if(grepl("20r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -20
  }
  if(grepl("15r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -15
  }
  if(grepl("10r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -10
  }
  if(grepl("05r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -5
  }
}

# Annoying shit, split into Rs and Ds
sum_bid <- overall_sum[which(overall_sum$vote == "Biden"),]
sum_bern <- overall_sum[which(overall_sum$vote == "Bernie"),]

b <- levelplot(support~poor*rich, data=sum_bid, col.regions = inferno(100),
          main=list('Policy Support Among Biden Voters',side=1,line=0.5),
          xlab=list(label="Poor"),
          ylab=list(label="Rich"),
          panel = function(...){
            panel.levelplot(...)
            panel.abline(h = 0, lwd = 4)
            panel.abline(v = 0, lwd = 4)
            panel.abline(c(0,1), lwd=2)
            panel.abline(c(0,-1), lwd=2)
          })

c <- levelplot(support~poor*rich, data=sum_bern, col.regions = inferno(100),
          main=list('Policy Support Among Sanders Voters',side=1,line=0.5),
          xlab=list(label="Poor"),
          ylab=list(label="Rich"),
          panel = function(...){
            panel.levelplot(...)
            panel.abline(h = 0, lwd = 4)
            panel.abline(v = 0, lwd = 4)
            panel.abline(c(0,1), lwd=2)
            panel.abline(c(0,-1), lwd=2)
          })


a <- levelplot(support~poor*rich, data=sum_r, col.regions = inferno(100),
          main=list('Policy Support Among Republicans',side=1,line=0.5),
          xlab=list(label="Poor"),
          ylab=list(label="Rich"),
          panel = function(...){
            panel.levelplot(...)
            panel.abline(h = 0, lwd = 4)
            panel.abline(v = 0, lwd = 4)
            panel.abline(c(0,1), lwd=2)
            panel.abline(c(0,-1), lwd=2)
          })

library(ggpubr)
ggarrange(a, labels=c("a"))
ggarrange(b, labels=c("b"))
ggarrange(c, labels=c("c"))

