# ------------------------------------------------------------------------------------------
#
# Typology of Political Policy Support
# Data Cleaning and Organization for Analysis
# 
# Jan 18th, 2020
#
# ------------------------------------------------------------------------------------------


setwd("CHANGE TO DIRECTORY WHERE YOU STORED THE DATA")

data <- read.csv("Typology_fulldata_clean.csv", header=T)
cbind(colnames(data))
# Remove those who failed the attn check
data <- data[which(data$attn == 2),]
# First 15, Last 454
# first two 455, last two 894

# For the variable names, for example, 50.p.u_50.r.u_b
# Means 50 poor up, 50 rich up, binary.

plyr::count(data[15])

library(dplyr)

list <- c()

for(i in 15:454){
  t <- sum(!is.na(data[i]))
  list <- c(list, t)
}

list2 <- c()

for(i in 455:894){
  t <- sum(!is.na(data[i]))
  list2 <- c(list2, t)
}

lists <- cbind(list, list2)

lists <- as.data.frame(lists)
psych::corr.test(lists$list, lists$list2)

psych::describe(list)
psych::describe(list2)

plyr::count(data$partyid)
plyr::count(data$partyid_force)

data$party <- 0
data$party[(data$partyid == 1 | data$partyid_force == 1)] <- "Dem"
data$party[(data$partyid == 3 | data$partyid_force == 2)] <- "Rep"

data$repub <- 0
data$repub[data$party == "Rep"] <- 1

data$male <- 0
data$male[data$gender == 1] <- 1

data$white <- 0
data$white[data$ethnicity == 1] <- 1

# Compile the SEIS and SDO
data$seis1 <- 8 - data$seis1
data$seis2 <- 8 - data$seis2
data$seis3 <- 8 - data$seis3

col.seis <- c(896:900)
data$seis <- rowMeans(data[,col.seis], na.rm = TRUE)

col.pte <- c(903:906)
col.cte <- c(907:910)
col.ptae <- c(911:914)
col.ctae <- c(915:918)
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

col.sdo <- c(903:918)
data$sdo <- rowMeans(data[,col.sdo], na.rm = TRUE)

# standardize sdo and seis first
data$seisz <- scale(data$seis, scale=T, center=T)
data$ptez <- scale(data$pte, scale=T, center=T)
data$ctez <- scale(data$cte, scale=T, center=T)
data$ptaez <- scale(data$ptae, scale=T, center=T)
data$ctaez <- scale(data$ctae, scale=T, center=T)
data$sdoz <- scale(data$sdo, scale=T, center=T)

# Get summary data so we can analyze and map 
# First step is to make the data long, just the binary policy support for now.
data$pid <- 1:nrow(data)

cols_bin <- c(15:454,933:949,922, 920)
wide_bin <- data[cols_bin]

cols_lik <- c(455:894,933:949, 920, 922)
wide_lik <- data[cols_lik]

library(tidyr)

long_bin <- gather(wide_bin, distribution, support, 1:440, factor_key=F)
long_lik <- gather(wide_lik, distribution, support, 1:440, factor_key=F)
long_bin$party <- as.factor(long_bin$party)
long_lik$party <- as.factor(long_lik$party)

# Now I need to recode and get the poor and rich coords
long_bin <- long_bin[which(!is.na(long_bin$support)),]
long_lik <- long_lik[which(!is.na(long_lik$support)),]

long_bin$poor <- 0
long_bin$rich <- 0
long_lik$poor <- 0
long_lik$rich <- 0

for(i in 1:nrow(long_bin)){
  if(grepl("50p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 50
  }
  if(grepl("45p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 45
  }
  if(grepl("40p.u", long_bin$distribution[i])){
    long_bin$poor[i] <- 40
  }
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
  if(grepl("50p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -50
  }
  if(grepl("45p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -45
  }
  if(grepl("40p.d", long_bin$distribution[i])){
    long_bin$poor[i] <- -40
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
  if(grepl("50r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 50
  }
  if(grepl("45r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 45
  }
  if(grepl("40r.u", long_bin$distribution[i])){
    long_bin$rich[i] <- 40
  }
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
  if(grepl("50r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -50
  }
  if(grepl("45r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -45
  }
  if(grepl("40r.d", long_bin$distribution[i])){
    long_bin$rich[i] <- -40
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


for(i in 1:nrow(long_lik)){
  if(grepl("50p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 50
  }
  if(grepl("45p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 45
  }
  if(grepl("40p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 40
  }
  if(grepl("35p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 35
  }
  if(grepl("30p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 30
  }
  if(grepl("25p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 25
  }
  if(grepl("20p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 20
  }
  if(grepl("15p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 15
  }
  if(grepl("10p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 10
  }
  if(grepl("05p.u", long_lik$distribution[i])){
    long_lik$poor[i] <- 5
  }
  if(grepl("p.nc", long_lik$distribution[i])){
    long_lik$poor[i] <- 0
  }
  if(grepl("50p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -50
  }
  if(grepl("45p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -45
  }
  if(grepl("40p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -40
  }
  if(grepl("35p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -35
  }
  if(grepl("30p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -30
  }
  if(grepl("25p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -25
  }
  if(grepl("20p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -20
  }
  if(grepl("15p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -15
  }
  if(grepl("10p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -10
  }
  if(grepl("05p.d", long_lik$distribution[i])){
    long_lik$poor[i] <- -5
  }
}


for(i in 1:nrow(long_lik)){
  if(grepl("50r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 50
  }
  if(grepl("45r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 45
  }
  if(grepl("40r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 40
  }
  if(grepl("35r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 35
  }
  if(grepl("30r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 30
  }
  if(grepl("25r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 25
  }
  if(grepl("20r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 20
  }
  if(grepl("15r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 15
  }
  if(grepl("10r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 10
  }
  if(grepl("05r.u", long_lik$distribution[i])){
    long_lik$rich[i] <- 5
  }
  if(grepl("r.nc", long_lik$distribution[i])){
    long_lik$rich[i] <- 0
  }
  if(grepl("50r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -50
  }
  if(grepl("45r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -45
  }
  if(grepl("40r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -40
  }
  if(grepl("35r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -35
  }
  if(grepl("30r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -30
  }
  if(grepl("25r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -25
  }
  if(grepl("20r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -20
  }
  if(grepl("15r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -15
  }
  if(grepl("10r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -10
  }
  if(grepl("05r.d", long_lik$distribution[i])){
    long_lik$rich[i] <- -5
  }
}







plyr::count(long_bin$poor)
plyr::count(long_bin$rich)
plyr::count(long_lik$poor)
plyr::count(long_lik$rich)

plot(long_bin$poor, long_bin$rich)
plot(long_lik$poor, long_lik$rich)

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
long_bin$rising_tide <- 0

long_lik$inc_ineq <- 0 #
long_lik$dec_ineq <- 0 #
long_lik$help_poor <- 0 #
long_lik$hurt_poor <- 0 #
long_lik$help_rich <- 0 #
long_lik$hurt_rich <- 0 #
long_lik$change_poor <- 0
long_lik$change_rich <- 0
long_lik$change_ineq <- 0
long_lik$rising_tide <- 0

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
long_bin$rising_tide[long_bin$rich > 0 & long_bin$poor > 0] <- 1

long_lik$help_poor[long_lik$poor > 0] <- 1
long_lik$hurt_poor[long_lik$poor < 0] <- 1
long_lik$help_rich[long_lik$rich > 0] <- 1
long_lik$hurt_rich[long_lik$rich < 0] <- 1
long_lik$inc_ineq[(long_lik$rich - long_lik$poor) > 0] <- 1
long_lik$dec_ineq[(long_lik$rich - long_lik$poor) < 0] <- 1
long_lik$change_poor[long_lik$poor < 0] <- -1
long_lik$change_poor[long_lik$poor > 0] <- 1
long_lik$change_rich[long_lik$rich < 0] <- -1
long_lik$change_rich[long_lik$rich > 0] <- 1
long_lik$change_ineq[(long_lik$rich - long_lik$poor) > 0] <- 1
long_lik$change_ineq[(long_lik$rich - long_lik$poor) < 0] <- -1
long_lik$rising_tide[long_lik$rich > 0 & long_lik$poor > 0] <- 1

long_bin$help_poorz <- scale(long_bin$help_poor, scale=T, center=T)
long_bin$hurt_poorz <- scale(long_bin$hurt_poor, scale=T, center=T)
long_bin$help_richz <- scale(long_bin$help_rich, scale=T, center=T)
long_bin$hurt_richz <- scale(long_bin$hurt_rich, scale=T, center=T)
long_bin$inc_ineqz <- scale(long_bin$inc_ineq, scale=T, center=T)
long_bin$dec_ineqz <- scale(long_bin$dec_ineq, scale=T, center=T)
long_bin$change_poorz <- scale(long_bin$change_poor, scale=T, center=T)
long_bin$change_richz <- scale(long_bin$change_rich, scale=T, center=T)
long_bin$change_ineqz <- scale(long_bin$change_ineq, scale=T, center=T)
long_bin$rising_tidez <- scale(long_bin$rising_tide, scale=T, center=T)
long_bin$supportz <- scale(long_bin$support, scale=T, center=T)

long_lik$help_poorz <- scale(long_lik$help_poor, scale=T, center=T)
long_lik$hurt_poorz <- scale(long_lik$hurt_poor, scale=T, center=T)
long_lik$help_richz <- scale(long_lik$help_rich, scale=T, center=T)
long_lik$hurt_richz <- scale(long_lik$hurt_rich, scale=T, center=T)
long_lik$inc_ineqz <- scale(long_lik$inc_ineq, scale=T, center=T)
long_lik$dec_ineqz <- scale(long_lik$dec_ineq, scale=T, center=T)
long_lik$change_poorz <- scale(long_lik$change_poor, scale=T, center=T)
long_lik$change_richz <- scale(long_lik$change_rich, scale=T, center=T)
long_lik$change_ineqz <- scale(long_lik$change_ineq, scale=T, center=T)
long_lik$rising_tidez <- scale(long_lik$rising_tide, scale=T, center=T)
long_lik$supportz <- scale(long_lik$support, scale=T, center=T)

# Write the files
write.csv(long_bin, file = "long_bin.csv")
write.csv(long_lik, file = "long_lik.csv")

write.csv(long_bin, file = "long_bin_controls.csv")
write.csv(long_lik, file = "long_lik_controls.csv")
