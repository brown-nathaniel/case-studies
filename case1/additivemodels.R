
#############################################################################

set.seed(440)
knitr::opts_chunk$set(echo = FALSE, warning = FALSE)
library(ggplot2); library(dplyr); library(reshape2); library(knitr); library(rstan); library(lme4); library(mvtnorm); library(gridExtra)
library(gbm); library(mgcv)


dat <- read.table("alllabs.txt", header = TRUE, stringsAsFactors = FALSE, na.strings = ".")
dat <- dat[!(is.na(dat$blot) | is.na(dat$body)),]
dat$fakelab <- sample(x=1:19, size=nrow(dat), replace=TRUE)

ndat <- nrow(dat)
trainrows <- sample(x=1:ndat, size = ceiling(0.75*ndat))
testrows <- setdiff(1:ndat, trainrows)
dat_train <- dat[trainrows,]
dat_test <- dat[testrows,]

#############################################################################

#root mean squared error penalizes more for extreme error
rms_err <- function(actual, predicted){
  error <- actual - predicted
  return(sqrt(mean((error)^2)))
}

#absolute error weighs all error equally
abs_err <- function(actual, predicted){
  error <- actual - predicted
  mean(abs(error))
}

compute_errs <- function(mod, test = dat_test, untrans_y = exp){
  preds <- untrans_y(predict(mod, test))
  absolute <- abs_err(test$blot, preds) %>% round(4)
  rootmean <- rms_err(test$blot, preds) %>% round(4)
  return(list(abs_err = absolute, rms_err = rootmean))
}

#############################################################################


#generalized additive model.
#s() smooths terms in the model.
#k argument specifies the number of unique values.
#I'm not sure why this is needed but if you don't specify for small k then the model won't fit. Tt has something to do with degrees of freedom.
gam1 <- gam(log(blot) ~ s((body)) + s(dose1, k=length(unique(dat$dose1))) + s(dose2, k=length(unique(dat$dose2))) + as.factor(proto) + as.factor(lab), data=dat_train)
plot(gam1, rug=TRUE, select = 2) #hey! that's not too far off from the dose1 effect curves we fit before!

compute_errs(gam1) #this is much better than m2! and it was so much faster! 

#termplot = "partial residuals vs predictor" plots. not entirely sure how to interpret for factors.
#anyways all these labs have widely different effect intervals
par(mfrow=c(2,1))
termplot(gam1, se=TRUE, rug=TRUE, ask=FALSE, col.se=2) 

#simulating fake labs so that there will be no lab effect
gam1fake <- gam(log(blot) ~ s((body)) + s(dose1, k=length(unique(dat$dose1))) + s(dose2, k=length(unique(dat$dose2))) + as.factor(proto) + as.factor(fakelab), data=dat_train)
plot(gam1fake, rug=TRUE, select = 2) 
compute_errs(gam1fake) 
par(mfrow=c(2,1))
termplot(gam1fake, se=TRUE, rug=TRUE, ask=FALSE, col.se=2) 
par(mfrow=c(1,1))

#yeah the fakelabs are all a straight line.
#so is this enough evidence to determine an inconsistent lab effect???

################################################################################################3

#the bayes version of everything that happened up there
bam1 <- bam(log(blot) ~ s((body)) + s(dose1, k=length(unique(dat$dose1))) + s(dose2, k=length(unique(dat$dose2))) + as.factor(proto) + as.factor(lab), data=dat_train)
plot(bam1, rug=TRUE, select = 2) #hey! that's not too far off from the dose1 effect curves we fit before! looks like it might be overfitting for dose=10
compute_errs(bam1) 
par(mfrow=c(2,1))
termplot(bam1, se=TRUE, rug=TRUE, ask=FALSE, col.se=2) 
par(mfrow=c(1,1))

#this all looks the same