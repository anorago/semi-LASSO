# Construction of the vector of prior knowledge used to progressively add variables in G_K
H=5
load("data/Data.RData") 
data = Data[[1]]
prior_know = c()
for (h in 1:(H-1)){
  # for the first 4 groups, the first 8 variables will be progressively added to G_K 
  # (corresponding to scenarii s_1 to s_8)
  prior_know_h = c(1/10*1:8, rep("not used",92)) 
  # for the variables not used in the model, prior_know is set to `not used`. 
  # It means they will never be included as prior knowledge in the model
  prior_know = c(prior_know, prior_know_h)
}
# Concerning the last group (h=5), the first 8 variables will be added in s_9 and s_10
prior_know = c(prior_know, rep(0.9,4),rep(1,4), rep("not used",92))
names(prior_know) = colnames(data[,-dim(data)[2]])