################################################################################
# R Data Analysis Examples: Multinomial Logistic Regression
# ref: http://www.ats.ucla.edu/stat/r/dae/mlogit.htm
# Multinomial logistic regression is used to model nominal outcome variables, 
# in which the log odds of the outcomes are modeled as a linear combination of 
# the predictor variables.
################################################################################
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


################################################################################
# Example. Entering high school students make program choices among general 
# program, vocational program and academic program. Their choice might be 
# modeled using their writing score and their social economic status. 
################################################################################
# read in the data
# The data set contains variables on 200 students. 
# The outcome variable is "prog", program type. The predictor variables are 
# social economic status("ses", a three-level categorical variable) and writing 
# score ("write", a continuous variable)
ml <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")
# getting some descriptive statistics of the variables of interest.
with(ml, table(ses, prog))
with(ml, do.call(rbind, tapply( write, prog, function(x) {
    c(M = mean(x), SD = sd(x))} 
    )))

# choose the level of outcome that will be used as the baseline. e.g."academic"
ml$prog2 <- relevel(ml$prog, ref = "academic")
# run the model: multinominal(y~x1,x2)
test <- multinom(prog2 ~ ses + write, data = ml)
# The multinom package does not include p-value calculation for the regression 
# coefficients, so we calculate p-values using Wald tests (here z-tests).
z <- summary(test)$coefficients/summary(test)$standard.errors
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# "relative risk": the ratio of the probability of choosing one outcome 
# category over the probability of choosing the baseline category
riskratio = exp(coef(test))

# try pridicting probabilities for each outcome level from the known data 
# (suppose we know the independant variable without knowing the dependant ones)
pp <- fitted(test)
