# ------------------------------------------------------------------------------------------
#
# Typology of Political Policy Support - Study 2
# Data Cleaning and Organization for Analysis
# 
# March 27th, 2020
#
# ------------------------------------------------------------------------------------------


setwd("CHANGE TO WHERE YOU ARE STORING THE DATA")

data <- read.csv("Study_2_raw.csv", header=T)
cbind(colnames(data))
# Remove those who failed the attn check
data <- data[which(data$attn == 2),]
# this drops 29 people (5.77%)

# First 14, Last 132

# For the variable names, for example, 50.p.u_50.r.u_b
# Means 50 poor up, 50 rich up, binary.


plyr::count(data$partyid)
plyr::count(data$partyid_force)

data$party <- 0
data$party[(data$partyid == 1 | data$partyid_force == 1)] <- "Dem"
data$party[(data$partyid == 3 | data$partyid_force == 2)] <- "Rep"

data$repub <- 0
data$repub[data$party == "Rep"] <- 1

# Compile the SDO
col.pte <- c(165:168)
col.cte <- c(169:170, 172:173) 
col.ptae <- c(174:177)
col.ctae <- c(178:181)
data$pte <- rowMeans(data[,col.pte], na.rm = TRUE)
data$cte <- rowMeans(data[,col.cte], na.rm = TRUE)
data$ptae <- rowMeans(data[,col.ptae], na.rm = TRUE)
data$ctae <- rowMeans(data[,col.ctae], na.rm = TRUE)

data$sdo5 <- 8 - data$sdo5
data$sdo6 <- 8 - data$sdo6
data$sdo7 <- 8 - data$sdo7
data$sdo8 <- 8 - data$sdo8
data$sdo13 <- 8 - data$sdo13
data$sdo14 <- 8 - data$sdo14
data$sdo15 <- 8 - data$sdo15
data$sdo16 <- 8 - data$sdo16

col.sdo <- c(165:170, 172:181)
data$sdo <- rowMeans(data[,col.sdo], na.rm = TRUE)

# standardize sdo
data$ptez <- scale(data$pte, scale=T, center=T)
data$ctez <- scale(data$cte, scale=T, center=T)
data$ptaez <- scale(data$ptae, scale=T, center=T)
data$ctaez <- scale(data$ctae, scale=T, center=T)
data$sdoz <- scale(data$sdo, scale=T, center=T)

# Get summary data so we can analyze and map 
# First step is to make the data long, just the binary policy support for now.
data$pid <- 1:nrow(data)

cols_bin <- c(14:208)
wide_bin <- data[cols_bin]

# The poor and rich emotions need recoded. score - 49. Stupid qualtrics.
wide_bin$poor_anger <- wide_bin$poor_anger - 49
wide_bin$poor_disgust <- wide_bin$poor_disgust - 49
wide_bin$poor_comp <- wide_bin$poor_comp - 49
wide_bin$poor_envy <- wide_bin$poor_envy - 49
wide_bin$rich_anger <- wide_bin$rich_anger - 17
wide_bin$rich_disgust <- wide_bin$rich_disgust - 17
wide_bin$rich_comp <- wide_bin$rich_comp - 17
wide_bin$rich_envy <- wide_bin$rich_envy - 17

# Recode everything else
wide_bin$reg_dem[wide_bin$reg_dem == 2] <- 0
wide_bin$school_close[wide_bin$school_close == 2] <- 1
wide_bin$school_close[wide_bin$school_close == 4] <- 0
wide_bin$UBI[wide_bin$UBI == 1] <- 0
wide_bin$UBI[wide_bin$UBI == 2] <- 1
wide_bin$consid_econ[wide_bin$consid_econ == 2] <- 0
wide_bin$medicare[wide_bin$medicare == 1] <- 0
wide_bin$medicare[wide_bin$medicare == 2] <- 1
wide_bin$stud_debt[wide_bin$stud_debt == 1] <- 0
wide_bin$stud_debt[wide_bin$stud_debt == 2] <- 1

wide_bin$trump_overall[wide_bin$trump_overall == 23] <- 1
wide_bin$trump_overall[wide_bin$trump_overall == 24] <- 2
wide_bin$trump_overall[wide_bin$trump_overall == 25] <- 3
wide_bin$trump_overall[wide_bin$trump_overall == 26] <- 4
wide_bin$trump_overall[wide_bin$trump_overall == 27] <- 5
wide_bin$trump_overall[wide_bin$trump_overall == 28] <- 6
wide_bin$trump_overall[wide_bin$trump_overall == 29] <- 7

wide_bin$trump_covid[wide_bin$trump_covid == 22] <- 1
wide_bin$trump_covid[wide_bin$trump_covid == 23] <- 2
wide_bin$trump_covid[wide_bin$trump_covid == 24] <- 3
wide_bin$trump_covid[wide_bin$trump_covid == 25] <- 4
wide_bin$trump_covid[wide_bin$trump_covid == 26] <- 5
wide_bin$trump_covid[wide_bin$trump_covid == 27] <- 6
wide_bin$trump_covid[wide_bin$trump_covid == 28] <- 7

# Add in a participant ID
wide_bin$pi

#write.csv(wide_bin, file = "bdata_wide.csv")

library(tidyr)

long_bin <- gather(wide_bin, distribution, support, 1:119, factor_key=F)
long_bin$party <- as.factor(long_bin$party)

# Now I need to recode and get the poor and rich coords
long_bin <- long_bin[which(!is.na(long_bin$support)),]

long_bin$poor <- 0
long_bin$rich <- 0

for(i in 1:nrow(long_bin)){
  if(grepl("35p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 35
  }
  if(grepl("30p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 30
  }
  if(grepl("25p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 25
  }
  if(grepl("20p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 20
  }
  if(grepl("15p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 15
  }
  if(grepl("10p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 10
  }
  if(grepl("05p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 5
  }
  if(grepl("p.nc", long_bin$distribution[i])){
    long_bin$poor[i] <- 0
  }
  if(grepl("35p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -35
  }
  if(grepl("30p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -30
  }
  if(grepl("25p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -25
  }
  if(grepl("20p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -20
  }
  if(grepl("15p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -15
  }
  if(grepl("10p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -10
  }
  if(grepl("05p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -5
  }
}


for(i in 1:nrow(long_bin)){
  if(grepl("35r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 35
  }
  if(grepl("30r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 30
  }
  if(grepl("25r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 25
  }
  if(grepl("20r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 20
  }
  if(grepl("15r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 15
  }
  if(grepl("10r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 10
  }
  if(grepl("05r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 5
  }
  if(grepl("r.nc", long_bin$distribution[i])){
    long_bin$rich[i] <- 0
  }
  if(grepl("35r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -35
  }
  if(grepl("30r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -30
  }
  if(grepl("25r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -25
  }
  if(grepl("20r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -20
  }
  if(grepl("15r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -15
  }
  if(grepl("10r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -10
  }
  if(grepl("05r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -5
  }
}

# Check to make sure all good
plyr::count(long_bin$poor)
plyr::count(long_bin$rich)

plot(long_bin$poor, long_bin$rich)

# Now lets create the features from Nour's suggestion
long_bin$inc_ineq <- 0 #
long_bin$dec_ineq <- 0 #
long_bin$help_poor <- 0 #
long_bin$hurt_poor <- 0 #
long_bin$help_rich <- 0 #
long_bin$hurt_rich <- 0 #
long_bin$change_poor <- 0
long_bin$change_rich <- 0
long_bin$change_ineq <- 0

# Code them
long_bin$help_poor[long_bin$poor > 0] <- 1
long_bin$hurt_poor[long_bin$poor < 0] <- 1
long_bin$help_rich[long_bin$rich > 0] <- 1
long_bin$hurt_rich[long_bin$rich < 0] <- 1
long_bin$inc_ineq[(long_bin$rich - long_bin$poor) > 0] <- 1
long_bin$dec_ineq[(long_bin$rich - long_bin$poor) < 0] <- 1
long_bin$change_poor[long_bin$poor < 0] <- -1
long_bin$change_poor[long_bin$poor > 0] <- 1
long_bin$change_rich[long_bin$rich < 0] <- -1
long_bin$change_rich[long_bin$rich > 0] <- 1
long_bin$change_ineq[(long_bin$rich - long_bin$poor) > 0] <- 1
long_bin$change_ineq[(long_bin$rich - long_bin$poor) < 0] <- -1

long_bin$help_poorz <- scale(long_bin$help_poor, scale=T, center=T)
long_bin$hurt_poorz <- scale(long_bin$hurt_poor, scale=T, center=T)
long_bin$help_richz <- scale(long_bin$help_rich, scale=T, center=T)
long_bin$hurt_richz <- scale(long_bin$hurt_rich, scale=T, center=T)
long_bin$inc_ineqz <- scale(long_bin$inc_ineq, scale=T, center=T)
long_bin$dec_ineqz <- scale(long_bin$dec_ineq, scale=T, center=T)
long_bin$change_poorz <- scale(long_bin$change_poor, scale=T, center=T)
long_bin$change_richz <- scale(long_bin$change_rich, scale=T, center=T)
long_bin$change_ineqz <- scale(long_bin$change_ineq, scale=T, center=T)

# Write the files
write.csv(long_bin, file = "long_bin.csv")

