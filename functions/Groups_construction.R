Groups_construction <- function (prior,partial_know){
  # prior : vector of the prior knowledge indicating when the variable enters into the G_K group
  # partial_know : proportion of partial knowledge to include (between 0 and 1)
  G_K = c()
  G_U = c()
  # G_K = group with a priori
  # G_U = group with no a priori
  for (j in 1:length(prior)){
    if((prior[j]<= partial_know)){ 
      G_K = c(G_K, names(prior)[j])
    }
    else{
      G_U = c(G_U, names(prior)[j])
    }
  }
  list(G_K, G_U)
}