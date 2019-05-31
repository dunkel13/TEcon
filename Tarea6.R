######################## Example 2 
set.seed(1123)
# slope effect 
b1 = 0.85 
# simulated data 
x = 1:60 
dat = data.frame(x=x,y=(x*b1)+rnorm(60)) 
llik = function(data, par){ 
  b1e=par[1]
  s=par[2]
  Res= data$y-(data$x*b1e)
  n=length(Res) 
  ll = -(n/ 2)*(log(2*pi*s^2)) + (-1/(2*s^2)) * sum(Res^2)# instead of (x-m)^2 given that E(res)=0
  return(-ll) 
} 

res0 = optim(par=c(0.5,0.5), llik, data=dat) 
res0$par

######################## Example 3: 
# Say that you are interested in whether or not a mother’s level of education relates to her child’s high school GPA. 
# =ur totally made up data 
MomEd = c(0, 1, 3, 4)
HSGPA = c(3.0, 3.2, 3.3, 3.7) # fit a linear model 
# data on mom's ed and HS GPA from above 
dat2=data.frame(x=MomEd,y=HSGPA)
llik2 = function(data, par){ 
  b0e=par[1]
  b1e=par[2]
  s=par[3]
  Res= data$y-(b0e+data$x*b1e)
  n=length(Res) 
  ll = -(n/ 2)*(log(2*pi*s^2)) + (-1/(2*s^2)) * sum(Res^2)# instead of (x-m)^2 given that E(res)=0
  return(-ll) 
} 
res1 = optim(par=c(0.5,0.5,0.5), llik2, data=dat2) 
res1$par

###########################
install.packages("AER")
library(AER)
data(CASchools)
attach(CASchools)
names(CASchools)
Score<-(read+math)/2
STR<- students/teachers

X<-matrix(c(rep(1,420), lunch, calworks, STR), nr=420, nc=4)

SS_min = function (y,X, par){
  beta=c(par[1],par[2],par[3],par[4])
  loss= t(y-X%*%beta)%*%(y-X%*%beta)
  return(loss)
}
det(t(X)%*%X)
res2a = optim(par=c(0.5,  0.5,  0.5,  0.5), SS_min, y=Score, X=X)
iteracion1=as.vector(res2a$par)
res2a1 = optim(par=iteracion1, SS_min, y=Score, X=X)
iteracion2=as.vector(res2a1$par)
res2a2 = optim(par=iteracion2, SS_min, y=Score, X=X)
res2a2$par
coef(lm(Score ~ lunch + calworks + STR)->lm0)
