# ------------------------------------------------------------------------------------------
#
# Typology of Political Policy Support - Ridge Plots with Binary data
# 
# Jan 19th, 2020
#
# ------------------------------------------------------------------------------------------

setwd("~/dropbox/work/postdoc/typology of policy support/writing/docs for osf/study 1/")

bdata <- read.csv("long_bin.csv", header=T)

library(ggplot2)
library(Rmisc)
library(lattice)
library(viridis)

# Couple people have zero on party, so just gonna get rid of them (8 people I think)
pdata <- bdata[which(bdata$party == "Rep" | bdata$party == "Dem"),]

overall_sum <- summarySE(pdata, measurevar="support", 
                         groupvars=c("distribution", "party"), na.rm=T)

# Annoying shit, split into Rs and Ds
sum_r <- overall_sum[which(overall_sum$party == "Rep"),]
sum_d <- overall_sum[which(overall_sum$party == "Dem"),]

# All the distinct distributions for recoding
ddata <- subset(pdata, !duplicated(distribution))
cols <- c(17,21:29)
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
  if(grepl("50p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 50
  }
  if(grepl("45p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 45
  }
  if(grepl("40p.u", overall_sum$distribution[i])){
    overall_sum$poor[i] <- 40
  }
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
  if(grepl("50p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -50
  }
  if(grepl("45p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -45
  }
  if(grepl("40p.d", overall_sum$distribution[i])){
    overall_sum$poor[i] <- -40
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
  if(grepl("50r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 50
  }
  if(grepl("45r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 45
  }
  if(grepl("40r.u", overall_sum$distribution[i])){
    overall_sum$rich[i] <- 40
  }
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
  if(grepl("50r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -50
  }
  if(grepl("45r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -45
  }
  if(grepl("40r.d", overall_sum$distribution[i])){
    overall_sum$rich[i] <- -40
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


# Need another summary dataset but not with ideology
overall_sum2 <- summarySE(pdata, measurevar="support", 
                         groupvars=c("distribution"), na.rm=T)

for(i in 1:nrow(overall_sum2)){
  if(grepl("50p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 50
  }
  if(grepl("45p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 45
  }
  if(grepl("40p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 40
  }
  if(grepl("35p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 35
  }
  if(grepl("30p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 30
  }
  if(grepl("25p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 25
  }
  if(grepl("20p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 20
  }
  if(grepl("15p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 15
  }
  if(grepl("10p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 10
  }
  if(grepl("05p.u", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 5
  }
  if(grepl("p.nc", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- 0
  }
  if(grepl("50p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -50
  }
  if(grepl("45p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -45
  }
  if(grepl("40p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -40
  }
  if(grepl("35p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -35
  }
  if(grepl("30p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -30
  }
  if(grepl("25p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -25
  }
  if(grepl("20p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -20
  }
  if(grepl("15p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -15
  }
  if(grepl("10p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -10
  }
  if(grepl("05p.d", overall_sum2$distribution[i])){
    overall_sum2$poor[i] <- -5
  }
}


for(i in 1:nrow(overall_sum2)){
  if(grepl("50r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 50
  }
  if(grepl("45r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 45
  }
  if(grepl("40r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 40
  }
  if(grepl("35r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 35
  }
  if(grepl("30r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 30
  }
  if(grepl("25r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 25
  }
  if(grepl("20r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 20
  }
  if(grepl("15r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 15
  }
  if(grepl("10r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 10
  }
  if(grepl("05r.u", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 5
  }
  if(grepl("r.nc", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- 0
  }
  if(grepl("50r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -50
  }
  if(grepl("45r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -45
  }
  if(grepl("40r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -40
  }
  if(grepl("35r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -35
  }
  if(grepl("30r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -30
  }
  if(grepl("25r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -25
  }
  if(grepl("20r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -20
  }
  if(grepl("15r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -15
  }
  if(grepl("10r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -10
  }
  if(grepl("05r.d", overall_sum2$distribution[i])){
    overall_sum2$rich[i] <- -5
  }
}

# Let's do the same, with just republicans and democrats on two different plots
overall_sum <- summarySE(pdata, measurevar="support", 
                         groupvars=c("distribution", "party"), na.rm=T)

# Annoying shit, split into Rs and Ds
sum_r <- overall_sum[which(overall_sum$party == "Rep"),]
sum_d <- overall_sum[which(overall_sum$party == "Dem"),]

for(i in 1:nrow(sum_r)){
  if(grepl("50p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 50
  }
  if(grepl("45p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 45
  }
  if(grepl("40p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 40
  }
  if(grepl("35p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 35
  }
  if(grepl("30p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 30
  }
  if(grepl("25p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 25
  }
  if(grepl("20p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 20
  }
  if(grepl("15p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 15
  }
  if(grepl("10p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 10
  }
  if(grepl("05p.u", sum_r$distribution[i])){
    sum_r$poor[i] <- 5
  }
  if(grepl("p.nc", sum_r$distribution[i])){
    sum_r$poor[i] <- 0
  }
  if(grepl("50p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -50
  }
  if(grepl("45p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -45
  }
  if(grepl("40p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -40
  }
  if(grepl("35p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -35
  }
  if(grepl("30p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -30
  }
  if(grepl("25p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -25
  }
  if(grepl("20p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -20
  }
  if(grepl("15p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -15
  }
  if(grepl("10p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -10
  }
  if(grepl("05p.d", sum_r$distribution[i])){
    sum_r$poor[i] <- -5
  }
}


for(i in 1:nrow(sum_r)){
  if(grepl("50r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 50
  }
  if(grepl("45r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 45
  }
  if(grepl("40r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 40
  }
  if(grepl("35r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 35
  }
  if(grepl("30r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 30
  }
  if(grepl("25r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 25
  }
  if(grepl("20r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 20
  }
  if(grepl("15r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 15
  }
  if(grepl("10r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 10
  }
  if(grepl("05r.u", sum_r$distribution[i])){
    sum_r$rich[i] <- 5
  }
  if(grepl("r.nc", sum_r$distribution[i])){
    sum_r$rich[i] <- 0
  }
  if(grepl("50r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -50
  }
  if(grepl("45r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -45
  }
  if(grepl("40r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -40
  }
  if(grepl("35r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -35
  }
  if(grepl("30r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -30
  }
  if(grepl("25r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -25
  }
  if(grepl("20r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -20
  }
  if(grepl("15r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -15
  }
  if(grepl("10r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -10
  }
  if(grepl("05r.d", sum_r$distribution[i])){
    sum_r$rich[i] <- -5
  }
}


for(i in 1:nrow(sum_d)){
  if(grepl("50p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 50
  }
  if(grepl("45p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 45
  }
  if(grepl("40p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 40
  }
  if(grepl("35p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 35
  }
  if(grepl("30p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 30
  }
  if(grepl("25p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 25
  }
  if(grepl("20p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 20
  }
  if(grepl("15p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 15
  }
  if(grepl("10p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 10
  }
  if(grepl("05p.u", sum_d$distribution[i])){
    sum_d$poor[i] <- 5
  }
  if(grepl("p.nc", sum_d$distribution[i])){
    sum_d$poor[i] <- 0
  }
  if(grepl("50p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -50
  }
  if(grepl("45p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -45
  }
  if(grepl("40p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -40
  }
  if(grepl("35p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -35
  }
  if(grepl("30p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -30
  }
  if(grepl("25p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -25
  }
  if(grepl("20p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -20
  }
  if(grepl("15p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -15
  }
  if(grepl("10p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -10
  }
  if(grepl("05p.d", sum_d$distribution[i])){
    sum_d$poor[i] <- -5
  }
}


for(i in 1:nrow(sum_d)){
  if(grepl("50r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 50
  }
  if(grepl("45r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 45
  }
  if(grepl("40r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 40
  }
  if(grepl("35r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 35
  }
  if(grepl("30r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 30
  }
  if(grepl("25r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 25
  }
  if(grepl("20r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 20
  }
  if(grepl("15r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 15
  }
  if(grepl("10r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 10
  }
  if(grepl("05r.u", sum_d$distribution[i])){
    sum_d$rich[i] <- 5
  }
  if(grepl("r.nc", sum_d$distribution[i])){
    sum_d$rich[i] <- 0
  }
  if(grepl("50r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -50
  }
  if(grepl("45r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -45
  }
  if(grepl("40r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -40
  }
  if(grepl("35r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -35
  }
  if(grepl("30r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -30
  }
  if(grepl("25r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -25
  }
  if(grepl("20r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -20
  }
  if(grepl("15r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -15
  }
  if(grepl("10r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -10
  }
  if(grepl("05r.d", sum_d$distribution[i])){
    sum_d$rich[i] <- -5
  }
}


# Overall Heatmap
levelplot(support~poor*rich, data=overall_sum2, col.regions = inferno(100),
               at=seq(min(0), max(1), length.out=100),
               main=list('Overall Policy Support',side=1,line=0.5),
               xlab=list(label="Poor"),
               ylab=list(label="Rich"),
               panel = function(...){
                 panel.levelplot(...)
                 panel.abline(h = 0, lwd = 4)
                 panel.abline(v = 0, lwd = 4)
                 panel.abline(c(0,1), lwd=2)
               })

levelplot(support~poor*rich, data=sum_d, col.regions = inferno(100),
          at=seq(min(0), max(1), length.out=100),
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
          at=seq(min(0), max(1), length.out=100),
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

wfs <- read.csv("Summary_for_Dif_Heatmap.csv", header=T)

library(latticeExtra)
library(RColorBrewer)

# Let's try creating a custom pallette. Take Red blue
# Make it black in the middle, lighter red more R, lighter blue more D?
palette <- c("#FF0000",
             "#FFFFFF",
             "#0015BC")

cols <- colorRampPalette(palette)

levelplot(dminr_dif~poor*rich, data=wfs, col.regions = cols,
               at=seq(min(-.5), max(.5), length.out=100),
               main=list('Difference in Support (Democrat - Republican)',side=1,line=0.5),
               xlab=list(label="Poor"),
               ylab=list(label="Rich"),
               panel = function(...){
                 panel.levelplot(...)
                 panel.abline(h = 0, lwd = 4)
                 panel.abline(v = 0, lwd = 4)
                 panel.abline(c(0,1), lwd=2)
                 panel.abline(c(0,-1), lwd=2)
               })

