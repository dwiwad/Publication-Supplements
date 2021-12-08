# DIF analyses- getting the ICCs from Study 2 and 5 combined, grouped on political ideology

setwd("c:/users/wiwad/dropbox/work/inequality attitudes scale construction/DIF")

data <- read.csv("combined 2and5 DIF.csv")

# Reverse code the first three seis items
data$tol1 <- (8 - data$tol1)
data$tol2 <- (8 - data$tol2)
data$tol3 <- (8 - data$tol3)

# Subset to only keep republicans and democrats
rd_data <- data[ which(data$partyID==1 | data$partyID==3), ]

#This leaves us with 878 people, how are they spread?
plyr::count(rd_data$partyID) 
# 535 Democrats and 343 Republicans. That's actually not too terrible.

# I want to see the histograms in the sub-groups
# First, let's just make sure the overall hists are the way we'd expect
hist(rd_data$tol1)
hist(rd_data$tol2)
hist(rd_data$tol3)
hist(rd_data$tol4)
hist(rd_data$tol5)

# Just getting the scale mean
col.tol <- c(2,3,4,5,6)
rd_data$seis <- rowMeans(rd_data[,col.tol], na.rm = TRUE)

# Now let's get the ICCs for each subgroup
library(ltm)

# Full group
graded.model <- grm(rd_data[c(2,3,4,5,6)])
coef.grm(graded.model, prob=TRUE)

# Make a data set for each republicans and democrats
repubs <- rd_data[ which(rd_data$partyID==3), ]
dems <- rd_data[ which(rd_data$partyID==1), ]

# Build the GRMs
graded.model.rep <- grm(repubs[c(2,3,4,5,6)])
graded.model.dem <- grm(dems[c(2,3,4,5,6)])

# Summarize the models
coef.grm(graded.model.rep, prob=TRUE)
coef.grm(graded.model.dem, prob=TRUE)

# Total scale information; full, reps, dems
information(graded.model, c(-4, 4))
information(graded.model.rep, c(-4, 4))
information(graded.model.dem, c(-4, 4))

# Information for each individual item
information(graded.model.rep, c(-4, 4), items = 1)
information(graded.model.rep, c(-4, 4), items = 2)
information(graded.model.rep, c(-4, 4), items = 3)
information(graded.model.rep, c(-4, 4), items = 4)
information(graded.model.rep, c(-4, 4), items = 5)

information(graded.model.dem, c(-4, 4), items = 1)
information(graded.model.dem, c(-4, 4), items = 2)
information(graded.model.dem, c(-4, 4), items = 3)
information(graded.model.dem, c(-4, 4), items = 4)
information(graded.model.dem, c(-4, 4), items = 5)

#add a high/low income column and re-save the data
data$medinc <- 0
data$income[data$income <= 4] <- 1
data$income[data$income > 4] <- 2

# write.csv(data, "medincdata.csv")

# Datasets
li <- data[ which(data$income==1), ]
hi <- data[ which(data$income==2), ]

# Build the GRMs
graded.model.li <- grm(li[c(2,3,4,5,6)])
graded.model.hi <- grm(hi[c(2,3,4,5,6)])

# Summarize the models
coef.grm(graded.model.li, prob=TRUE)
coef.grm(graded.model.hi, prob=TRUE)

# Total scale information; LI and HI models
information(graded.model.li, c(-4, 4))
information(graded.model.hi, c(-4, 4))

# Information for each individual item
information(graded.model.li, c(-4, 4), items = 1)
information(graded.model.li, c(-4, 4), items = 2)
information(graded.model.li, c(-4, 4), items = 3)
information(graded.model.li, c(-4, 4), items = 4)
information(graded.model.li, c(-4, 4), items = 5)

information(graded.model.hi, c(-4, 4), items = 1)
information(graded.model.hi, c(-4, 4), items = 2)
information(graded.model.hi, c(-4, 4), items = 3)
information(graded.model.hi, c(-4, 4), items = 4)
information(graded.model.hi, c(-4, 4), items = 5)




