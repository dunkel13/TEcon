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

