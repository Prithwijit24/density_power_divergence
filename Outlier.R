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

Fit_ls = lm(Y3~.,data = as.data.frame(Z3))
res_ls = Fit_ls$residuals
index = c(1:length(Y3))
plot(index,res_ls,pch = 16,ylab = "residuals",main = paste("Residuals from the least square optimization"))

wrapper3 = function(beta3)Objective(Y=Y3,X=Z3,beta = beta3,alpha = 0.25)
Sali_25 = optim(par = Initial_beta3,wrapper3,method = "BFGS")$par
Fit_25 = Z3%*%Sali_25[-5]
Res_25 = Y3-Fit_25
plot(index,Res_25,pch = 16,ylab = "residuals",main = paste("Residuals for alpha = 0.25"))


wrapper3 = function(beta3)Objective(Y=Y3,X=Z3,beta = beta3,alpha = 0.50)
Sali_50 = optim(par = Initial_beta3,wrapper3,method = "BFGS")$par
Fit_50 = Z3%*%Sali_50[-5]
Res_50 = Y3-Fit_50
plot(index,Res_50,pch = 16,ylab = "residuals",main = paste("Residuals for alpha = 0.50"))















