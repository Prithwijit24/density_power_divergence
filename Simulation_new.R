##2D Model
set.seed(43639)
library(mvtnorm)
x = rnorm(1000,0,1)
z = rnorm(10,-10,1)

y = 5 + 0.5*x

x[991:1000] = z
D = data.frame(x,y,class)
class = c(numeric(990)+1,numeric(10)+3)
plot(D$x,D$y,col = D$class,pch = D$class+16,xlab = "x",ylab = "y",
     main = paste("Sctter plot of x and y"))
Model = lm(y~x,data = D)
abline(a = Model$coefficients[1],b = Model$coefficients[2])


Objective = function(Y,X,beta,alpha)
{
  sigma = beta[length(beta)]
  beta = beta[-length(beta)]
  f = NULL
  for ( i in 1:length(Y))
  { 
    Mean = X[i,]%*%beta
    f[i] = (1/((2*pi)^(alpha/2)*(sigma^alpha)*sqrt(1+alpha))) - 
      (1+(1/alpha))*dnorm(Y[i],mean = Mean,sd = sigma)^alpha
  }
  return(mean(f))
}

Final = matrix(nrow = 3,ncol = 8)
alpha = c(0.05,0.1,0.25,0.04,0.5,0.6,0.8,1)
new = c("red","blue","green","violet","orange","pink","yellow","black")
Legend = c("0.05","0.1","0.25","0.4","0.5","0.6","0.8","1")

x = data.frame(numeric(length(y))+1,x)
x = data.matrix(x)
Initial_beta = c(1,1,1)




X = x
Y = y
Initial_beta = c(0.11,0.11,0.11)
wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.05)
Final1 = optim(par = Initial_beta,wrapper)$par
abline(a = Final1[1],b = Final1[2],col = "red")

























##3D Model
set.seed(65688)
library(rgl)
library(magick)
library(webshot2)
library(LaplacesDemon)
library(mvtnorm)
xm = rmvnorm(1000,c(0,0),diag(2)*10)
zm = rmvnorm(10,c(-10,-50),diag(2))

ym = 5 + (xm[,1]*5)+(xm[,2]*10) + rnorm(1000,-10,10)
xm[991:1000,] = zm
class = c(rep("yellow",990),rep("green",10))
Dm = data.frame(xm,ym,class)


plot3d(Dm,col = Dm$class,size = 5,radius = 2,type = "s")

Modelm = lm(ym~X1+X2,data = Dm)
summary(Modelm)
P = Modelm$coefficients
Data = data.frame(xm,Modelm$fitted.values)
planes3d(a = P[2],b = P[3],c = -1,d = P[1],col = "red",alpha = 0.9)




Objective = function(Y,X,beta,alpha)
{
  sigma = beta[length(beta)]
  beta = beta[-length(beta)]
  f = NULL
  for ( i in 1:length(Y))
  {
    Mean = crossprod(X[i,],beta)
    f[i] = (1/((2*pi)^(alpha/2)*(sigma^alpha)*sqrt(1+alpha))) - 
      (1+(1/alpha))*dnorm(Y[i],mean = Mean,sd = sigma)^alpha
  }
  return(mean(f))
}



xm = data.frame(numeric(length(ym))+1,xm)
xm = data.matrix(xm)
X = xm
Y = ym
Initial_beta = c(0.11,0.11,0.11,0.11)



wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.05)
Final1 = optim(par = Initial_beta,wrapper)$par
planes3d(a = Final1[2],b = Final1[3],c= -1,d = Final1[1],col = "Green")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.4)
Final2 = optim(par = Initial_beta,wrapper)$par
planes3d(a = Final2[2],b = Final2[3],c = -1,d = Final2[1],col = "grey")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.5)
Final3 = optim(par = Initial_beta,wrapper)$par
planes3d(a = Final3[2],b = Final3[3],c = -1,d = Final3[1],col = "Blue")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.8)
Final4 = optim(par = Initial_beta,wrapper)$par
planes3d(a = Final4[2],b = Final4[3],c = -1,d = Final4[1],col = "pink")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,1)
Final5 = optim(par = Initial_beta,wrapper)$par
planes3d(a = Final3[2],b = Final3[3],c = -1,d = Final3[1],col = "orange")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.6)
Final6 = optim(par = Initial_beta,wrapper)$par
planes3d(a = Final3[2],b = Final3[3],c = -1,d = Final3[1],col = "violet")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.8)
Final7 = optim(par = Initial_beta,wrapper)$par
planes3d(a = Final3[2],b = Final3[3],c = -1,d = Final3[1],col = "darkgreen")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,1)
Final8 = optim(par = Initial_beta,wrapper)$par
planes3d(a = Final3[2],b = Final3[3],c = -1,d = Final3[1],col = "darkorange")







