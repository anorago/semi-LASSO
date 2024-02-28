# Estimation of the beta coefficients using both semi-LASSO and 0-1 weighted LASSO
# for a dataset, and 11 scenarii of prior knowledge

load("data/Data.RData") 
options( "digits"=5, "scipen"=0) 
# loading of the list containing the K = 50 datasets
# We show how the estimation works using only the first dataset
source("functions/semi_LASSO.R")
source("functions/Groups_construction.R")
source("scripts/prior_knowledge.R")
data_K = Data[[1]]

# semi-LASSO
regressors_list_by_knowledge = list() # creation of the list of regressors
# each element of the list will correspond to a particular scenario
index = 1
for (l in seq(0, 1, by = 0.1)){ # levels of prior knowledge
  A_priori_group = Groups_construction(prior_know,l) # construction of G_K and G_U
  result = semi_LASSO(A_priori_group[[1]], A_priori_group[[2]], data_K,
                      colnames(data_K)[length(colnames(data_K))],
                      inter = TRUE)
  regressors_list_by_knowledge[[index]] = result[[1]]
  index = index + 1
}
names(regressors_list_by_knowledge) = seq(0, 1, by = 0.1)

# 0-1 weighted LASSO
regressors_list_by_knowledge_glmnet = list()
index = 1
for (l in seq(0,1, by = 0.1)){
  weights = as.numeric(prior_know>l) # transformation of prior_know into 0-1 weights
  # the p+1th variable in data_K is Y
  reg = cv.glmnet(x = as.matrix(data_K[,-(p+1)]), y = as.matrix(data_K[,(p+1)]), penalty.factor = weights, grouped=FALSE, alpha=1, intercept=TRUE)
  result = coef(reg)[-1,] # reorder the position of the intercept estimation
  result = c(result, coef(reg)[1,])
  names(result)[p+1] = "Intercept"
  regressors_list_by_knowledge_glmnet[[index]] = result
  index = index + 1
}
names(regressors_list_by_knowledge_glmnet)<-seq(0, 1, by = 0.1)
