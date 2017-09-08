# Lab 3
# Simple Linear Regression

# 1. More on Model Validation
# We have seen in class what will happen when the linear assumption or
# the equal variance assumption are violated. 

# 1.1 Let us first see what will happen if the independent assumption is violated
# Simulate Data (from a 0.95 auto-correlation AR(1) process)
x <- 1:1000
y <- 1+0.0005*x
epsilon <- rep(0,1000)
epsilon[1] <- 0.1*rnorm(1)
for (i in 2:1000){
  epsilon[i] <- 0.95*epsilon[i-1] + sqrt(0.01*(1-0.95^2))*rnorm(1)
}
y <- y + epsilon

# The trace of the data looks really "sticky"
plot(x,y)
abline(reg=lm(y ~ x),col="red")


myreg <- lm(y~x)
plot(myreg,1) # What did you see? 
#stickiness- cannot abruptly change signs very easily (e.g. value tends to stick in previous region, changes little by little)
#clearly some patterns, if was indpendent would look completely random 

# 1.2 Let us then see what will happen if the normality assumption is violated
# Simulate Data (error comes from a Gamma dist, which is skewed)
x <- 1:1000
y <- 1+0.005*x
epsilon <- rgamma(1000,shape=2,rate=1/2)
epsilon[1] <- 0.1*rnorm(1)
hist(epsilon,100)
y <- y + epsilon

# Data above the line and the data below the line seem to have different spread
plot(x,y)
abline(reg=lm(y ~ x),col="red")
#spread of data much larger above reg line 

myreg <- lm(y~x)
# Residual vs fitted plot
plot(myreg,1) # What did you see?
#positive resids have much larger spread than negative resids

# quantile-quantile plot (Q-Q plot) of the residuals 
#*** POST FITTING CHECK: to check normality after you run linreg model (because it's run on the residuals)
# Either directly from the lm object
plot(myreg,2)
# or manually
qqnorm(myreg$residuals, ylab = "Residuals", main = "Normal quantile plot of residuals")
qqline(myreg$residuals)
# What did you see?
#definitely not normal lol. lots of cupping the line 

# 1.3 Conclusion
# You always need to check the following four assumptions: 1. Linearity;
# 2. Equal Variance; 3. Independence; 4. Normality


#1. Check scatter plot
#2. Do points deviate from regression line equally? 
#3. Check for randomness in residual plot
#4. Use qq plot 


# 2. Sleuth 3 exerciese 7.29
# (TA: Introduce the question)
x <- c(0, 5, 17, 37, 75, 100) #dist. between interviewers and door of the place where people voted 
y <- c(5.3, 6.4, 5.6, 7.6, 9.6, 12.3) #overestimate rate (estimated results v. actual results)
plot(x, y, xlab = "Distance", ylab = "Overestimate", 
     main = "Overestimate versus Distance")
abline(reg=lm(y ~ x),col="red")

#fit a linear regression model
myreg <- lm(y ~ x)

#view the results of the regression
#(TA: explain )
summary(myreg)
#slope tells you if there is a linear relationship (if positive, indicates increase -> increase)
#if p value is small, regression model is significant 
  #note: this is a one sided test, bc only want to see if slope is positive & not zero 
  #p-value in summary is two sided 
#if r^2 close to 1, tells you model fits data well 

#get 95% confidence intervals for the coefficients
confint(myreg)

#Manually, getting values from summary: 
#CI for slope
est=0.065167
SE=0.007483
df=4
c(est-SE*qt(.975,df),est+SE*qt(.975,df))
#p-val for the slope
est/SE
2*(1-pt(est/SE,df)) #for two sided test 
(1-pt(est/SE,df)) #for one sided test 
#(optional) F-stat
total <- sum((y-mean(y))^2)
ressum <- sum(myreg$residuals^2)
model <- total - ressum
(Fstat <- model/ressum*4)

# Question: How strong is the evidence that the mean Kerry overestimate increases
# with the increasing distance of interviewer from the door?
# Tricky part: looking at the p-val of the slope is not directly answering the question
# since the p-val is testing "H0: slope equals zero". It is not directly answering our question
# So what we really need is a one sided test
# H0: slope <=0 versus H1: slope > 0

# Previous: 2*(1-pt(est/SE,df)) (two-sided)
1-pt(est/SE,df)
# Conclusion: very strong evidence!

# Question: Can we make a statement of causality?
# Answer: No, since there might be other confounders
# Potential confounder please refer to Sleuth3 exercise 7.30
# One could imagine that the younger interviewers are located at long distances
# and the elder interviewers are close to the door.

# 3. Do-it-yourself: Sleuth3 exercise 7.30
# # (TA: Introduce the question)
x <- c(22, 30, 40, 50, 60, 65)
y <- c(0.39, 0.38, 0.35, 0.32, 0.31, 0.29)
plot(x, y, xlab = "Age", ylab = "Refusal", 
     main = "Refusal versus Age")
abline(reg=lm(y ~ x),col="red")

# Question: What evidence do these data provide that the mean refusal rate
# decreased with increasing age of interviewer?

myreg <- lm(y~x)
summary(myreg)
confint(myreg)

est=-0.0023468
SE=0.0001557
df=4
c(est-SE*qt(.975,df),est+SE*qt(.975,df))
#p-val for the slope
est/SE
pt(-abs(est/SE),df)

#Strong evidence

