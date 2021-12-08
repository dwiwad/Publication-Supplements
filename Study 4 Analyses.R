# Study 4 Analysis
# January 24th, 2017

setwd("/Users/dylanwiwad/Dropbox/Work/Dissertation/Writing/Scale Development/RE-submit, Jan 2018/Predictive Validity Study")
getwd()

# Get the data
data <- read.csv("Study 4.csv", header=TRUE)

# Need to code it properly to be in line with the final scale. Only five items, coded
# such that higher scores mean more support for inequality.
data$tol_1.r <- 8-data$tol_1
data$tol_2.r <- 8-data$tol_2
data$tol_8.r <- 8-data$tol_8
data$tol_9.r <- 8-data$tol_9
data$tol_10.r <- 8-data$tol_10

cbind(colnames(data))
col.scale <- c(78,79,80,81,82)
data$scale <- rowMeans(data[,col.scale], na.rm = TRUE)

# Taking a quick look at the composited scale to verify all is good
hist(data$scale)

# regression for the agreement question
modelA <- lm(petition_agree~scale, data=data)
summary(modelA)

modelB <- lm(petition_agree~scale+age+gender+edu_1+income_2+political_1+inequality_1, data=data)
summary(modelB)

# Logistic regression with just the TFI scale
model1 <- glm(signed ~ scale,family=binomial(link='logit'),data=data)
summary(model1)

# Logistic regression with all controls
model2 <- glm(signed ~ scale+age+gender+edu_1+income_2+political_1+inequality_1,family=binomial(link='logit'),data=data)
summary(model2)
