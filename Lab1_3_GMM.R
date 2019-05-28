#################### Tópico de econometría: Microeconometría ###################### 
#################### Karoll Gómez Portilla ###################### 
#################### Universidad Nacional de Colombia ###################### 
#################### Lab 1.3: Regression Methods: MM y GMM ###################### 

######################## Example 1: Estimating the parameters of a normal distribution 
library(gmm) 

# The moment conditions 
g1 <- function(tet,x) {     
  m1 <- (tet[1]-x)     
  m2 <- (tet[2]^2 - (x - tet[1])^2)     
  m3 <- x^3-tet[1]*(tet[1]^2+3*tet[2]^2)     
  f <- cbind(m1,m2,m3)     
  return(f)     
} 

# Th gradient matrix 
Dg <- function(tet,x) {    
  G <- matrix(c( 1,     2*(-tet[1]+mean(x)),     -3*tet[1]^2-3*tet[2]^2,     0,     2*tet[2],     -6*tet[1]*tet[2]),     nrow=3,ncol=2)     
  return(G)     
} 

# We need to generate normally distributed random numbers: 
set.seed(123) 
n <- 200 
x1 <- rnorm(n, mean = 4, sd = 2) 

print(res <- gmm(g1,x1,c(mu = 0, sig = 0), grad = Dg)) 
summary(res) 

######################## Example 2: CAPM Model 
# --------------------- Loading packges ----------------------------
# Before to call the package you should go to packages & data, then package installer, write the name of the package in the package serch box, press get the list, select the package and install selected. 
library(gmm) 
library(tseries) 
library(zoo) 
library(lmtest)
# --------------------- Import Data ----------------------------
datos = read.delim("/Users/karollgomez/Documents/Cursos/Econometria1_pregrado/ Metrix_2015_01/datos_CAPM.txt",header=TRUE,sep = "\t", dec = ",") 
#datos <- read.table(file=file.choose(), header=T, sep="\t", dec=",") 
head(datos) 
summary(datos) 
# ---------------------- Gaphs ---------------------------------
# Let us plot the data and see how the time series look like. In adition we also create the excess returns for SP500 (rx) and INTC. 
p <- cbind(datos$rx, datos$INTC) 
colnames(p) <- c("rx","INTC") # rename column names 

# Returns 
returns=diff(log(p)) # estimate continuous returns NOTE: we lose one observation 

par(mfcol=c(2, 2)) # create a a subplot 2x2 
plot(p[,1],main="Price of SP500",ylab="price",xlab="", type="l") 
plot(returns[,1],main="Returns of SP500",ylab="return",xlab="", type="l") 
plot(p[,2],main="Price of Intel Corporation",ylab="price",xlab="", type="l") 
plot(returns[,2],main="Returns of Intel Corporation",ylab="return",xlab="", type="l") 

# Risk free rate 
rfr <- datos$rf 
rf <- as.matrix((1+rfr/100)^(1/250)-1) # transform to daily returns 

par(mfcol=c(1, 1)) # reset graph for only 1 plot 
plot(rf,main="CBOE 10y interest rate T-Note",ylab="risk free rate",xlab="", type="l") 

# Excess returns 
t <- length(rf[,1])  
rxm <- returns[,1]-rf[2:t,1] 
rxintc <- returns[,2]-rf[2:t,1] 
rets <- cbind(returns,rf[2:t,1],rxm,rxintc) # add excess return to data 
colnames(rets)[3:5] <- c("rf","exRetSP500","exRetINTC") 
ret <- data.frame(rets) # for ols estimation
head(ret) 
# ---------------------- 1. OLS Estimation ------------------------
ols.model <- lm(ret$exRetINTC~ret$exRetSP500,data=ret) 
summary(ols.model) 
coeftest(ols.model, df=Inf,vcov = NeweyWest(ols.model,lag=4,prewhite=FALSE))

# ---------------------- 2. LM Estimation ------------------------
# extract only the data from returns series
exRetSP500 <- coredata(ret$exRetSP500) # Generic functions for extracting the core data contained in a (more complex) object and replacing it 
exRetINTC <- coredata(ret$exRetINTC) 
data=cbind(exRetINTC,exRetSP500) 

# MLE estimation 
# Another way to do it (uncomment to try that you get the same result): #The maxLik version would be: 
library(miscTools) 
library(maxLik) 
LL1 <- function(param,data=data){  
  y=data[,1]  
  x=cbind(1,data[,-1]) # to eliminate the first variable  
  beta <- param[-1]   
  sigma2 <- param[1]  
  if(sigma2<=0) return(NA)  
  epsilon=y-x%*%beta  # calculate residuals  
  # log-likelihood of errors  
  logLik=-0.5*(log(2*pi)+log(sigma2)+(epsilon)^2/sigma2) 
} 

# The optim version would be (uncomment the lines below to try optim): 
# LL <- function(param,data=data){  
  # y=data[,1]  
  # x=cbind(1,data[,-1]) # # add the intercept and remove y=exRetINTC (1st data col)  
  # beta <- param[-1] # exclude the first  
  # sigma2 <- param[1]   
  # if(sigma2<=0) return(NA)  
  # epsilon=y-x%*%beta  # calculate residuals  
  # # log-likelihood of errors  
  # logLik=-0.5*(log(2*pi)+log(sigma2)+(epsilon)^2/sigma2)  
  # -sum(logLik)
# } 

# Let us estimate the CAPM using MLE 
# The maxLik version would be 
theta.start = c(0.00017,0,1) 
MLE <- maxLik(LL1,start=theta.start,data=data,method="BFGS") 
coef(MLE) 
# Note: if your initial guess for the parameters is too far off then things can go seriously wrong! This applies especially when objective function is (almost) flat or in boundary solutions. 

# The optim version would be (uncomment the lines below to try optim): 
# theta.start = c(0.00017,0,1) 
# MLE <- optim(theta.start,LL,gr=NULL,data,method="BFGS",hessian=TRUE) #"LBFGS-B" 
# mle.param <- as.matrix(MLE$par) 
# fish <- MLE$hessian
# stdErr <- sqrt(diag(solve(fish))) 
# tStat <- mle.param/stdErr 
# mle.model <- cbind(mle.param,stdErr,tStat) 
# rownames(mle.model) <- c("sigma2","alpha","beta") 
# colnames(mle.model) <- c("Estimates","Std. errors","t.Stat") 
# mle.model 

# ---------------------- 3. GMM Estimation ------------------------
# Moment conditions for linear regression model (introduced in section 1) can be written as follows. 
ols.moments = function(param,data=NULL) {  
  data = as.matrix(data)  
  y=data[,1]  
  x=cbind(1,data[,-1]) # add the intercept and remove y (1st data col)  
  x*as.vector(y - x%*%param) 
} 

# Let us estimate the model using gmm 
start.vals=c(0,1) 
names(start.vals) <- c("alpha","beta") 
gmm.model=gmm(ols.moments,data,t0=start.vals,vcov="HAC") 
summary(gmm.model) 
print(specTest(gmm.model)) 

# ---------------------- 4. Comparison ------------------------
# Let us graphically check whether the estimates from different models are the same. 
plot(exRetSP500,exRetINTC,main="Comparison of OLS, MLE and GMM") 
abline(ols.model,col="blue") 
abline(a=mle.param[2],b=mle.param[3],col="green") 
abline(gmm.model,col="red") 
legend('topleft',c("OLS","MLE","GMM"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5), col=c("blue","green","red"))
