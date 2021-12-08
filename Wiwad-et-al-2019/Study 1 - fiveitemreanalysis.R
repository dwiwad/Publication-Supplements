#----------------------------------------------------
# Item Analysis - Support for Inequality
# Data from Pilot, w/ Azim's self-driving car study
# Tolerance for inequality items only
# Testing shortened scale v4 (5 Items)
#----------------------------------------------------

# Get the file
Data <- read.csv("Study 1 - final5item.csv", header = TRUE)

# Function to recode all the questions; 1-7 becomes 0-6
recode <- function(x){
  x[x == 1] <- 0
  x[x == 2] <- 1
  x[x == 3] <- 2
  x[x == 4] <- 3
  x[x == 5] <- 4
  x[x == 6] <- 5
  x[x == 7] <- 6
  return(x)
}

# Actually use the function on each item
Data$tol3 <- recode(Data$tol3)
Data$tol5.r <- recode(Data$tol5.r)
Data$tol8.r <- recode(Data$tol8.r)
Data$tol10 <- recode(Data$tol10)
Data$tol18.r <- recode(Data$tol18.r)

# Building the Graded Response Model
library(ltm)
graded.model <- grm(Data)
summary(graded.model)

# Get item information for the whole spectrum
information(graded.model, c(-4, 4))

# Information given by each item in the scale, over the whole range
information(graded.model, c(-4, 4), items = 1)
information(graded.model, c(-4, 4), items = 2)
information(graded.model, c(-4, 4), items = 3)
information(graded.model, c(-4, 4), items = 4)
information(graded.model, c(-4, 4), items = 5)

describe(Data$tol3)
describe(Data$tol5.r)
describe(Data$tol8.r)
describe(Data$tol10)
describe(Data$tol18.r)

