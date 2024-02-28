# This script put all the results (estimation of beta, sensi, speci) together
# to build Data_final of size 1100 (50x11x2) x 507 (including 501 coeff estim)
# also compute the RMSE calculated on a test dataset


load("data/Regressors.RData") # coeff estimation for 11 scenarii and 50 datasets for semi-LASSO
load("data/Regressors_glmnet.RData") # coeff estimation for 11 scenarii and 50 datasets for glmnet
load("data/Data_test.RData") # test dataset of size 100
load("data/beta.RData") # load real beta
p = dim(data_test)[2]-1
n = dim(data_test)[1]
K=50
# Creation of a dataset containing all the results loaded above
Data_semilasso = data.frame()
for (k in 1:K){
  for (s in 1:11){
    Data_semilasso = rbind(Data_semilasso, as.data.frame(t(Regressors_K[[k]][[s]])))
  } }
Data_semilasso = cbind(Data_semilasso, data.frame(PriorKnowledge=rep((0:10)*10, K)))
Data_semilasso = cbind(Data_semilasso, data.frame(Simulation=sort(rep(1:K, 11))))
Data_semilasso = cbind(Data_semilasso, data.frame(Method = "semi-LASSO"))
Data_glmnet = data.frame()
for (k in 1:K){
  for (s in 1:11){
    Data_glmnet = rbind(Data_glmnet, as.data.frame(t(Regressors_Kglmnet[[k]][[s]])))
  }
}
Data_glmnet = cbind(Data_glmnet, data.frame(PriorKnowledge = rep((0:10)*10,K)))
Data_glmnet = cbind(Data_glmnet, data.frame(Simulation = sort(rep(1:K,11))))
Data_glmnet = cbind(Data_glmnet, data.frame(Method="0-1 weighted LASSO"))
# Fusion of the two created dataframes
Data_final = rbind(Data_semilasso, Data_glmnet)
# We add 2 more columns to Data_final for sensitivity and specificity
sensitivity = c()
specificity = c()
for (i in 1:dim(Data_final)[1]) {
   Bet_chap = Data_final[i,1:p] # remove the intercept estimation
   se = length(which(Bet_chap!=0 & beta!=0))/(length(which(Bet_chap!=0 & beta!=0))+length(which(Bet_chap==0 & beta!=0)))
   sp = length(which(Bet_chap==0 & beta==0))/(length(which(Bet_chap==0 & beta==0))+length(which(Bet_chap!=0 & beta==0)))
   sensitivity=c(sensitivity,se)
   specificity=c(specificity,sp)
   }
   Data_final= cbind(Data_final,data.frame(Sensitivity=sensitivity),data.frame(Specificity=specificity))
   # Change some columns as factors
   Data_final$Method = as.factor(Data_final$Method)
   Data_final$Simulation = as.factor(Data_final$Simulation)
   Data_final$PriorKnowledge = as.factor(Data_final$PriorKnowledge)
   # Compute the RMSE on test dataset
   RMSE_test = c()
   RMSE_glmnet_test = c()
   for (s in 1:(K*11)){
     y_chap = as.matrix(data_test[,-(p+1)])%*%t(as.matrix(Data_final[s,1:p]))+
       rep(1,n)*Data_final[s,p+1]
     error = sqrt(sum((y_chap-data_test$Y)*(y_chap-data_test$Y))/n)
     y_chap_glmnet = as.matrix(data_test[,-(p+1)])%*%t(as.matrix(Data_final[(K*11+s),1:p]))+
       rep(1,n)*Data_final[(K*11+s),p+1]
     error_glmnet =sqrt(sum((y_chap_glmnet-data_test$Y)*(y_chap_glmnet-data_test$Y))/n)
     RMSE_test = c(RMSE_test, error)
     RMSE_glmnet_test = c(RMSE_glmnet_test, error_glmnet)
   }
   RMSE = c(RMSE_test, RMSE_glmnet_test)
   # Add to Data_final
   Data_final = cbind(Data_final,data.frame(RMSEtest = RMSE))
