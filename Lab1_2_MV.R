#################### Tópico de econometría: Microeconometría ###################### 
#################### Lab 1.2: Regression Methods: ML ###################### 

######################## Example 1 

# For consistency set the seed explicitly. 
set.seed(1123) 
#first simulate some normal data with expected mean of 0 and sd of 1 
x = rnorm(100) 

# scale the data the way that we would like 
x = x/sd(x) * 8 # sd of 8 
x = x-mean(x) + 10 # mean of 10 
c('mean'=mean(x),'sd'=sd(x)) # double check 

# Histogram 
hist(x, freq=FALSE,col='tan') 
lines(density(x),col='red',lwd=2)

# Specify the single value normal probability function 
norm_lik = function(x, m, s){y = 1/sqrt(2*pi*s^2)*exp((-1/(2*s^2))*(x-m)^2)} 

# and plot it just to make sure 
plot(seq(-3,3,.1),sapply(seq(-3,3,.1),FUN=norm_lik,m=0,s=1),type='l', ylab='',xlab='', main='Gaussian Normal') 

# log of the normal likelihood 
# -n/2 * log(2*pi*s^2) + (-1/(2*s^2)) * sum((x-m)^2) 
#return the negative to maximize rather than minimize 
llik = function(x,par){ 
  m=par[ 1] 
  s=par[ 2]  
  n=length(x) 
  ll = -(n/ 2)*(log(2*pi*s^2)) + (-1/(2*s^2)) * sum((x-m)^2)  
  return(-ll) 
} 

# log likelihood curve 
plot(seq(-3,3,.1),-1*sapply(seq(-3,3,.1),FUN=llik,par=c(0,1)),type='l', ylab='',xlab='') 

####### Optimization 
# call optim with the starting values 'par', 
# the function (here 'llik'), 
# and the observations 'x' 
res0 = optim(par=c(.5,.5), llik, x=x) 
res0$par

# Comparison 
mean(x) 
sd(x) 
# To do tables in Latex 
library(webshot) 
library(viridisLite) 
library(httr) 
library(rvest) 
library(rstudioapi) 
library(evaluate) 
library(rmarkdown) 
library(xfun) 
library(knitr) 
library(htmltools) 
library(kableExtra) 
print(kable(cbind('direct'=c('mean'=mean(x),'sd'=sd(x)), 'optim'=res0$par), digits=3)) 

######################## Example 2 
# slope effect 
b1 = 0.85 
# simulated data 
x = 1:60 
dat = data.frame(x=x,y=(x*b1)+rnorm(60)) # from the lm() function 
lm1 = lm(y~x, data=dat) 
lm1 

######################## Example 3: 
# Say that you are interested in whether or not a mother’s level of education relates to her child’s high school GPA. 
# =ur totally made up data 
MomEd = c(0, 1, 3, 4) 
HSGPA = c(3.0, 3.2, 3.3, 3.7) # fit a linear model 
coef(lm(HSGPA ~ MomEd) -> lm0) 
# sum of squares function 
SS_min = function(data,par){ 
  b0=par[1] 
  b1=par[2]  
  loss = with(data,sum((b0+b1*x-y)^ 2))  
  return(loss) 
} 

# data on mom's ed and HS GPA from above 
dat=data.frame(x=MomEd,y=HSGPA)

# min resid sum of squares 
res1a = optim(par=c(.5,.5),SS_min, data=dat) 
res1a$par
print(kable(cbind('lm()'=coef(lm0),'SS_min'=res1a$par),digits=4)) 
