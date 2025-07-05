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

#1.
#######################################################
#Star datas-et
library(robustbase)
D = data.frame(starsCYG)
#View(D)
p = plot(D,pch = 16,main = paste("regression lines for least square fitting"))


alpha = c(0.05,0.1,0.25,0.04,0.5,0.6,0.8,1)
new = c("red","blue","green","violet","orange","pink","yellow","black")
Legend = c("0.05","0.1","0.25","0.4","0.5","0.6","0.8","1")

Y = D$log.light
X = data.frame(rep(1,nrow(D)),D$log.Te)
colnames(X) = c("constant","log.Temperature")
X = data.matrix(X)
Model = lm(D$log.light~D$log.Te)
summary(Model)
abline(a = Model$coefficients[1],b = Model$coefficients[2])



Initial_beta = c(1,1,1)
wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.05)
Final1 = optim(par = Initial_beta,wrapper)$par
abline(a = Final1[1],b = Final1[2],col = "red")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.1)
Final2 = optim(par = Initial_beta,wrapper)$par
abline(a = Final2[1],b = Final2[2],col = "blue")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.25)
Final3 = optim(par = Initial_beta,wrapper)$par
abline(a = Final3[1],b = Final3[2],col = "green")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.4)
Final4 = optim(par = Initial_beta,wrapper)$par
abline(a = Final4[1],b = Final4[2],col = "violet")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.5)
Final5 = optim(par = Initial_beta,wrapper)$par
abline(a = Final5[1],b = Final5[2],col = "orange")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.6)
Final6 = optim(par = Initial_beta,wrapper)$par
abline(a = Final6[1],b = Final6[2],col = "pink")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,0.8)
Final7 = optim(par = Initial_beta,wrapper)$par
abline(a = Final7[1],b = Final7[2],col = "yellow")

wrapper = function(beta)Objective(Y=Y,X=X,beta = beta,1)
Final8 = optim(par = Initial_beta,wrapper)$par
abline(a = Final8[1],b = Final8[2])

legend(3.5,6,legend = Legend,col=new,lty = 1:8,cex = 1.0,
       y.intersp = 0.8,text.font = 16,ncol = 2,lwd = 2,
       box.lwd = 0)
















#2.
#######################################################
#Belgium Telephone call data
Phone = data.frame(MASS::phones)
#View(Phone)
library(ggplot2)
ph = plot(Phone,pch = 16,main = paste("regression lines for different values of alpha"))
Model = lm(Phone$calls~Phone$year)
abline(a = Model$coefficients[1],b = Model$coefficients[2])

#Applying Method
Phone_Call = matrix(0,nrow = 3,ncol = 8)
alpha2 = c(0.05,0.1,0.25,0.04,0.5,0.6,0.8,1)
new2 = c("red","blue","green","violet","orange","pink","yellow","black")

Y2 = Phone$calls
X2 = data.frame(rep(1,nrow(Phone)),Phone$year)
colnames(X) = c("constant","year")
X2 = data.matrix(X2)
Initial_beta2 = c(1,1,1)
for(i in 1:length(alpha2))
{
  wrapper2 = function(beta2)Objective(Y=Y2,X=X2,beta = beta2,alpha = alpha2[i])
  Phone_Call[,i] = optim(par = Initial_beta2,wrapper2)$par
  abline(a = Phone_Call[1,i],b = Phone_Call[2,i],col = new[i],lty = i,lwd = 2)
}
legend(50,200,legend = Legend,col=new,lty = 1:8,cex = 1.0,
       y.intersp = 0.8,text.font = 16,ncol = 2,lwd = 2,
       box.lwd = 0)





#3.
#######################################################
#Salinity data
library(robustbase)
Salinity_data = data.frame(salinity)
#View(Salinity_data)


#Applying Method
Sali_da = matrix(nrow = 5,ncol = 8)
alpha3 = c(0.05,0.1,0.25,0.04,0.5,0.6,0.8,1)
new3 = c("red","blue","green","violet","orange","pink","yellow","black")

Y3 = Salinity_data$Y
Z3 = data.frame(rep(1,nrow(Salinity_data)),Salinity_data$X1,Salinity_data$X2,Salinity_data$X3)
colnames(Z3) = c("constant","X1","X2","X3")
Z3 = data.matrix(Z3)
Initial_beta3 = c(1,1,1,1,1)
for(i in 1:length(alpha3))
{
  wrapper3 = function(beta3)Objective(Y=Y3,X=Z3,beta = beta3,alpha = alpha3[i])
  Sali_da[,i] = optim(par = Initial_beta3,wrapper3,method = "BFGS")$par
}


rownames(Sali_da) = c("beta_1","beta_2","beta_3","beta_4","sigma")
colnames(Sali_da) = c("0.05","0.1","0.25","0.4","0.5","0.6","0.8","1")

Sali_da



