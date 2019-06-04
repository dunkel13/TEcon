#################### Lab 2: Dichotomous  Logit and Probit models ######################

# We will  obtain predicted probabilities, predictions of the dependent variable, coefficients, marginal effects for the variables, 
#model diagnostics, and hypothesis tests. In addition, we provide programs that obtain each of these outputs 
#the hard way for illustrative purposes only.

######################## Example 1: logit and Probit estimation using glm

# The data is a sample of 32 observations from a study by Spector and Mazzeo on pre and post tests for economic literacy. 
#The goal is to determinate the effect of teaching method (PSI) on the performance of students (GRADE). The variables are:

# GPA = grade point average
# TUCE = test score on teaching college level economics (pre-test)
# PSI = program participation variable (school district)
# GRADE = response: 1 if grade on post-test is higher than on pre-test, 0 otherwise.

Spector = read.csv("/Users/bvcfce/Documents/aldrichnelson.csv", header=TRUE, sep=";")
View(Spector)
names(Spector)
attach(Spector)
summary(Spector)

# We will first estimate a Probit and Logit using glm.
library(carData)
library(car)
library(stats)
library(MASS)

#--------------------------------------- Probit ----------------------------

probitglm.out = glm( GRADE ~ GPA + TUCE + PSI, family = binomial(link=probit), data = Spector)
summary(probitglm.out)

# Put the summary into an R object
probitsummary = summary(probitglm.out)
names(probitglm.out)
names(probitsummary)

# We can do likelihood ratio, Wald, and F tests on each of the individual coefficients in the model as follows. 
#Note the upper case Anova. Note that the F test is inappropriate when using a binomial family distribution such as this one. 
#It is here just for illustration.
Anova(probitglm.out, test = 'LR')
Anova(probitglm.out, test = 'Wald')
Anova(probitglm.out, test = 'F')

# To compare nested models we can do likelihood ratio and F tests for subsets of coefficients as follows. 
#Note the lower case anova. Again, the F is inappropriate with the binomial family distribution.

anova(update(probitglm.out, GRADE ~ GPA + TUCE + PSI ), probitglm.out, test = 'Chisq')
anova(update(probitglm.out, GRADE ~ GPA + TUCE + PSI ), probitglm.out, test = 'F')
anova(update(probitglm.out, GRADE ~ GPA + TUCE + PSI + TUCE*PSI), probitglm.out, test = 'Chisq')
anova(update(probitglm.out, GRADE ~ GPA + TUCE + PSI + TUCE*PSI), probitglm.out, test = 'F')

#--------------------------------------- Logit estimation ------------------------------------------
logitglm.out = glm( GRADE ~ GPA + TUCE + PSI, family = binomial, data = Spector)
summary(logitglm.out)

# Following are some interesting values that can be extracted from the model for later use.
# The value of the loglikelihood at the maximum
loglik = 1/2*probitglm.out$deviance
loglik
# Akaike's Information Criterion
aic = probitglm.out$aic
aic
# Model Coefficients
beta = probitglm.out$coefficients
beta
# Model Residuals
residuals = probitglm.out$residuals
residuals
# Fitted Probabilities
fitted.probs =probitglm.out$fitted.values
fitted.probs
# Covariance of the coefficients.
covb = probitsummary$cov.unscaled
covb
# 95% CI for the coefficients
confint(logitglm.out) 
logitglm.out$coefficients
# exponentiated coefficients (Odds ratios)
exp(coef(logitglm.out)) 
# 95% CI for exponentiated coefficients
exp(confint(logitglm.out)) 
# predicted values
predict(logitglm.out, type="response") 

# Marginal effects
install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)
install.packages("zoo")
library(zoo)
install.packages("mfx")
library(mfx)

probit.mfx.out = probitmfx(GRADE ~ GPA + TUCE + PSI, data = Spector)
probit.mfx.out
logit.mfx.out = logitmfx(GRADE ~ GPA + TUCE + PSI, data = Spector)
logit.mfx.out
logit.mfx.out$mfxest
logit.mfx.out$fit
logit.mfx.out$dcvar # indicates for which variable a discrete change marginal effect is captured

# Odds ratios
logit.or.out=logitor(GRADE ~ GPA + TUCE + PSI, data = Spector)
logit.or.out

# Now, get the underlying latent index, z, from some of this stuff and show how to get the set of probabilities for each case the hard way. Note that these correspond to fitted.probs above.
beta = probitglm.out$coefficients
x = cbind(1,as.matrix(Spector[,1:3]))
z = x%*% beta
head(z)
pz = pnorm(c(z))
head(pz)

# Generating the model predictions and a prediction success table.
Predict = recode(fitted.probs, "0:0.499999=0; 0.5:1=1")
Predict
table(Predict,GRADE)

# Calculate marginal effects with all variables at their means from the probit coefficients and a scale factor. These can be interpreted much like slopes. See table 21.1 on p. 675 of Greene.
beta = probitglm.out$coefficients
xbar = as.matrix(colMeans(cbind(1,Spector[1:3])))
zxbar = t(xbar) %*% beta
scalefactor = dnorm(zxbar)
scalefactor
margin = scalefactor* beta[2:4]
margin

# Calculate marginal effects with all variables at their means from the logit coefficients and a scale factor. Again, these can be interpreted much like slopes. Compare the results to those on p. 675 of Greene.
beta1 =logitglm.out$coefficients
xbar1 =as.matrix(colMeans(cbind(1,Spector[1:3])))
zxbar1 = t(xbar1) %*% beta1
lambda = 1/(1+exp(-zxbar1))
scalefactor1 =lambda*(1-lambda)
scalefactor1
margin1 = scalefactor*beta[2:4]
margin1

# The Zelig library is more flexible allowing us to do the same things as are done

install.packages("Zelig")
library(Zelig)
probit.out = zelig(GRADE ~ GPA + TUCE + PSI, model = "probit", data = Spector)
summary(probit.out)

# Execute the following to get the auxiliary outputs from probit.out and the summary object.
# Now for interpretation of the model.

#Simulate the predicted probability of making an A, PSI=0, rest at mean
x.out = setx(probit.out, PSI = 0)
s.out = sim(probit.out, x = x.out)
summary(s.out)

#Simulate the predicted probability of making an A, PSI=1, rest at mean
x.out = setx(probit.out, PSI = 1)
s.out = sim(probit.out, x = x.out)
summary(s.out)

# We could just take the difference between these to get a first difference. However, Zelig can do this in one step.
# Simulated first difference and risk ratios when Shifting PSI from 0 to 1 #
psi.low = setx(probit.out, PSI = 0)
psi.high = setx(probit.out, PSI = 1)
s.out = sim(probit.out, x = psi.low, x1 = psi.high)
summary(s.out)

# You can also plot the results from the simulations in Zelig.
plot(s.out)

# Another example
# First Difference when Shifting GPA from 2 to 3 with PSI=0 and TUCE at mean
gpa.low = setx(probit.out, GPA=2, PSI=0)
gpa.high = setx(probit.out, GPA = 3, PSI=0)
s.out1 = sim(probit.out, x = gpa.low, x1 = gpa.high)
summary(s.out1)
plot(s.out1)