# Short r analysis script for NHB revisions - Study 2
# July 2nd, 2019
# Dylan Wiwad
#
# Code generated using Qualtrics test dummy data
# Analyses were performed identically using this script once data collection was complete

# Read in data file
setwd("~/dropbox/work/NHB_paper/NHB R&R June 2019/")
data <- read.csv("Study2_data.csv", header=T)

# Reverse score the SEIS and composite all measures
# SEIS
data$seis1 <- 8 - data$seis1
data$seis2 <- 8 - data$seis2
data$seis3 <- 8 - data$seis3

col.seis <- c(26,27,28,29,30)
data$seis <- rowMeans(data[,col.seis], na.rm = TRUE)

# Attributions for poverty
col.disp <- c(13,14,15,18)
col.sit <- c(17,20,21,22,23)
data$disp <- rowMeans(data[,col.disp], na.rm = TRUE)
data$sit <- rowMeans(data[,col.sit], na.rm = TRUE)

# Support for redistribution
col.redist <- c(31,32,33,34)
data$redist <- rowMeans(data[,col.redist], na.rm = TRUE)

# Correlations for the pre-registered hypotheses
library(psych)

# Hypothesis 1.1
corr.test(data$sit, data$seis, use = "pairwise", method = "pearson")
# Hypothesis 1.2
corr.test(data$sit, data$redist, use = "pairwise", method = "pearson")
# Hypothesis 2.1
corr.test(data$disp, data$seis, use = "pairwise", method = "pearson")
# Hypothesis 2.2
corr.test(data$disp, data$redist, use = "pairwise", method = "pearson")

# rerun controlling for ideology
summary(lm(seis~sit + ideology, data=data))
summary(lm(redist~sit + ideology, data=data))
summary(lm(seis~disp + ideology, data=data))
summary(lm(redist~disp + ideology, data=data))

# Confirm the breakdowns
library(plyr)
count(data$ideology)

# Subset the data into those who passed the attention check
data2 <- data[ which(data$attn.check == 2), ]

# Hypothesis 1.1
corr.test(data2$sit, data2$seis, use = "pairwise", method = "pearson")
# Hypothesis 1.2
corr.test(data2$sit, data2$redist, use = "pairwise", method = "pearson")
# Hypothesis 2.1
corr.test(data2$disp, data2$seis, use = "pairwise", method = "pearson")
# Hypothesis 2.2
corr.test(data2$disp, data2$redist, use = "pairwise", method = "pearson")

# Non parametric correlations on the full sample
# Hypothesis 1.1
corr.test(data$sit, data$seis, use = "pairwise", method = "spearman")
# Hypothesis 1.2
corr.test(data$sit, data$redist, use = "pairwise", method = "spearman")
# Hypothesis 2.1
corr.test(data$disp, data$seis, use = "pairwise", method = "spearman")
# Hypothesis 2.2
corr.test(data$disp, data$redist, use = "pairwise", method = "spearman")

# Mapping out the participants
library(maps)
state = map_data("state")

library(dplyr)
dat = filter(data, (long > -140 & long < -50), (lat > 20 & lat < 50))
ggplot() + geom_polygon(data=state, aes(x = long, y = lat, group = group), fill = NA, colour = "#2874A6") + 
  labs(x = "Longitude", y = "Latitude") + ggtitle("Participants location in the United States") + coord_fixed(1.3) +
  geom_point(data=dat, aes(x = long, y = lat), color = "#CD6155", size = 1) + theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = .5))

# Get state from lat and long
library(sp)
library(maptools)

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

lat_long_only <- data.frame(x = dat$long, y = dat$lat)

latlong2state(lat_long_only)

plyr::count(latlong2state(lat_long_only))

