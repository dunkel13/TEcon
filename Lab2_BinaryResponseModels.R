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

# ROC plots can be used to gauge the relative fit of two models using prediction success. In R, the best model is the one that comes closest to the topright corner of the graph. A completely uninformative model would be one which falls along the diagonal reference line.
# For example, consider the models below where PSI has been omitted from the second model.
probit1.out = zelig(GRADE ~ GPA + TUCE + PSI, model="probit", data=Spector)
probit2.out = zelig( GRADE ~ GPA + TUCE , model="probit", data=Spector)
rocplot(probit1.out, probit2.out)

# All that is needed to move to logit is changing the model = "probit" option to model = "logit". Just change the output object to logit.out for the other calculations. The only calculation that is different is for the marginal effects.
# Let's run the logit and calculate the marginal effects, leaving the other calculations above for you to do on your own.
logit.out = zelig( GRADE ~ GPA + TUCE + PSI, model = "logit", data = Spector)
summary(logit.out)

######################## Example 2: let's estimate a Probit the hard way and obtain the interesting quantities.

# First, create the data matrix, dependent vector, and other necessary quantities.
x = cbind(1,GPA,TUCE,PSI)
y = as.matrix(GRADE)
z = as.matrix(PSI)
K = ncol(x)
K


# Define Probit Log Likelihood
llik.probit=function(par, X, Y){
    Y = as.matrix(y)
    X = as.matrix(x)
    b = as.matrix(par[1:K])
    phi = pnorm(X%*%b, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
    sum(Y*log(phi)+(1-Y)*log(1-phi))
}

#Fit Probit Model

values.start = lm(GRADE ~ GPA + TUCE + PSI)$coef
mod.probit = optim(values.start, llik.probit, Y=Y, X=X)
mod.probit = optim(values.start, llik.probit, Y=Y, X=X, method=c("BFGS"), control=list(maxit=2000, fnscale=-1), hessian=T)
mod.probit

# Save the log likelihood for later use
LR = mod.probit$value
LR

# Calculate the variance matrix from the Hessian matrix.
v  = -solve(mod.probit$hessian)
v
# Calculate the standard errors from the variance matrix.
se = sqrt(*diag(v))
se

# Calculate the z statistics from the coefficients and standard errors
b = mod.probit$par
b
zstat = b/se
zstat

# Calculate p values from the z statistics
pz = 2*(1-pnorm(abs(zstat)))
pz

# Put the results together in a table.
table = cbind(b,se,zstat,pz)
table

# Obtain the underlying latent index, z.
z = x%*% b
z

# Calculate the probabilities for each value of z
pz = pnorm(c(z))

######################## Example 3: let's estimate a Logit the hard way and obtain the interesting quantities.

# Define Logit Log Likelihood
llik.logit=function(par, X, Y){
    Y = as.matrix(y)
    X = as.matrix(x)
    b = as.matrix(par[1:K])
    Lambda = 1/(1+exp(-X%*%b))
    sum(Y*log(Lambda)+(1-Y)*log(1-Lambda))
}


values.start = lm( GRADE ~ GPA + TUCE + PSI)$coef
mod.logit = optim(values.start, llik.logit, Y=Y, X=X, method="BFGS", control=list(maxit=2000, fnscale=-1), hessian=T)
mod.logit

# Save the log likelihood for later use
LR = mod.logit$value
LR

# Calculate the variance matrix from the Hessian matrix.
v = -solve(mod.logit$hessian)
v

# Calculate the standard errors from the variance matrix.
se  = sqrt(diag(v))
se

# Calculate the z statistics from the coefficients and standard errors
b = mod.logit$par
b
zstat = b/se
zstat

# Calculate p values from the z statistics
pz = 2*(1-pnorm(abs(zstat)))
pz

# Put the results together in a table.
table = cbind(b,se,zstat,pz)
table

# Obtain the underlying logit latent index, z.
z = x%*% b
z

# Calculate the logit probabilities for each value of z
pz = 1/(1+exp(-c(z)))
pz

