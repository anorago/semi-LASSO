semi_LASSO <- function(known_group, unknown_group, data_transform, response_var, inter = TRUE){
  # known and unknown groupe give the indices of variables for which we have (or not) an a priori
  # data_transform is the dataset containing all covariates X and the response
  # variable Y
  # response_var gives the name of the variable in the dataset to be considered
  # as the response variable
  # inter is TRUE or FALSE to include or not an intercept into the model
  warn_if(response_var %in% known_group ||response_var %in% unknown_group  ,isTRUE,
          msg = "Your response variable will be used as an explicative variable. 
          Maybe you put it in one of the two groups")
  # Warn if the response variable is put in G_K or G_U
  n = dim(data_transform)[1]
  p= dim(data_transform)[2]-1 # remove the response variable
  G_K = known_group
  G_U = unknown_group
  ind = which(colnames(data_transform)==response_var)
  
  if (length(G_K)==0){
    # Case when there is no a priori.
    # Then it's just a classic LASSO
    model = cv.glmnet(as.matrix(data_transform[,-ind]),as.matrix(data_transform[,ind]),alpha = 1,grouped=FALSE, intercept = inter)
    lambda = model$lambda.1se
    coeff = coef(model)
    beta_NK = coeff[2:length(coeff)]
    beta = rep(0,p+1)
    names(beta) = c(colnames(data_transform[,-ind]),"Intercept")
    for(j in 1:length(G_U)){
      beta[G_U[j]]= beta_NK[j]
    }
    # we add the value of intercept
    beta[p+1] = coeff[1]
  }
  if (length(G_U)==0){
    # Case when all regressors are known
    # Then it's a classic regression (we suppose that the number of regressors 
    # will not exceed the number of observations)
    
    list_names_data = colnames(data_transform)[G_K]
    lambda = 0
    
    if (length(list_names_data)!=0){
      reg_j = lm(formula = as.formula(paste(response_var, "~ 0+", paste(list_names_data, collapse = "+"))), data = data_transform)
      resume_j = reg_j$coefficients
    }
    else {
      # The case when j is an initial node with no parent
      resume_j = rep(0,p)
    }
    
    
    # Creation of the final vector of coefficients
    beta = rep(0,p+1)
    names(beta) = c(colnames(data_transform[,-ind]),"Intercept")
    for(j in 1:length(G_K)){
      beta[G_U[j]]= resume_j[j]
    }
    
    
  }
  if ((length(G_U)!=0)&(length(G_K)!=0)){
    # Semi-LASSO
    # Creation of all necessary objects X_NK and X_K
    X_NK =as.matrix(data_transform[,G_U]) 
    X_K = cbind(as.matrix(data_transform[,G_K]),rep(1,n)) # we add a column for the intercept
    Y = as.matrix(data_transform[,ind])
    # Creation of U and V
    I_n = diag(n)
    U = (I_n - X_K%*%solve(t(X_K)%*%X_K)%*%t(X_K))%*%Y  # new Y
    V = X_NK - X_K%*%solve(t(X_K)%*%X_K)%*%t(X_K)%*%X_NK
    # Performing first step LASSO, to find beta_NK
    model_NK = cv.glmnet(V,U,alpha = 1,grouped=FALSE, intercept = inter)
    lambda = model_NK$lambda.1se
    # Getting the estimated coefficients and constructing beta_NK
    # Be careful, the first parameter is the intercept in the coef() function
    coeff_NK = coef(model_NK)
    beta_NK = coeff_NK[2:length(coeff_NK)]
    
    # We can deduce beta_K from beta_NK
    beta_K = solve(t(X_K)%*%X_K)%*%t(X_K)%*%(Y-X_NK%*%beta_NK)
    beta_K
    
    # Construction of the final beta
    beta = rep(0,p+1)
    names(beta) = c(colnames(data_transform[,-ind]),"Intercept")
    for(j in 1:length(G_K)){
      beta[G_K[j]]= beta_K[j]
    }
    for(j in 1:length(G_U)){
      beta[G_U[j]]= beta_NK[j]
    }
    #beta[p+1] = coeff_NK[1]
    beta[p+1] = beta_K[length(beta_K)]
  }
  list(beta,lambda)
}