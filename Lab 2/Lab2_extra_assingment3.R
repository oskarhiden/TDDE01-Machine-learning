RNGversion('3.5.1')
library(readr)

state = read.csv2("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Lab 2/state.csv")
state = data.frame(state)
#---Step 1---
state_ordered = state[order(state$MET, decreasing = FALSE),]
plot(state_ordered$MET, state_ordered$EX, main ="MET vs EX", xlab = "MET", ylab = "EX")
# qudratic or polinomial?

#---Step 2---
library(tree)
tree_model = tree(EX ~ MET, state_ordered, control = tree.control(nobs = nrow(state_ordered), minsize = 8))
plot(tree_model)
text(tree_model, pretty = 0)
cv_tree = cv.tree(tree_model)
best_size = cv_tree$size[which.min(cv_tree$dev)]

#following plot showes that best = 4 is the best tree.
plot(x=cv_tree$size, y=cv_tree$dev, type = "b", col= "blue")

pruned_tree = prune.tree(tree_model, best = best_size)
pred = predict(pruned_tree, newdata = state_ordered)

hist(residuals(pruned_tree), breaks = 20)

plot(x=state_ordered$MET, y=state_ordered$EX)
points(x=state_ordered$MET , y=pred, col="red", type = "l")


#---step 3---
set.seed(12345)
B=1000
sample_size = nrow(state_ordered)

boot_predictions = matrix(nrow=sample_size, ncol=B)
for(i in 1:B){
  samples = sample(seq(1,nrow(state_ordered),1), size = sample_size, replace=TRUE)
  data = state_ordered[samples, ]
  
  tree_model = tree(EX ~ MET, data = data, control = tree.control(nobs= nrow(state_ordered), minsize = 8))
  pruned_tree = prune.tree(tree_model, best=best_size)
  boot_predictions[, i] = predict(pruned_tree, newdata = state_ordered)
}
#calcultate std deviations and confidence bands using quantiles function
conf_band = apply(boot_predictions, 1, function(x){
  band = quantile(x, probs = c(0.025, 0.975))
  return(band)
})

#Plot conf. intervalls:
plot(x=state_ordered$MET, y=state_ordered$EX, col = "red", xlab = "MET", ylab = "EX", main = "95% conf. bands")
points(x=state_ordered$MET , y=pred, col="red", type = "l")
lines(x=state_ordered$MET, y=conf_band[1, ], col = "blue")
lines(x=state_ordered$MET, y=conf_band[2, ], col = "blue")


#use bootstrap ---------------TEST
library(boot)
#function to generate datapoints for non-parametic bootstrap
f=function(data, ind){
  data1=data[ind,]# extract bootstrap sample
  tree_model=tree(EX ~ MET, data1, control = tree.control(nobs = nrow(data1), minsize = 8)) #fit linear model
  pruned_tree = prune.tree(tree_model, best = best_size)
  
  #predict values for all Area values from the original data
  priceP=predict(pruned_tree, newdata=state_ordered)
  return(priceP)
}
res=boot(state_ordered, f, R=1000) #make bootstrap
res
boot_data = data.frame(res$t)
conf_band = apply(boot_data, 2, function(col){
  band = quantile(col, probs = c(0.025, 0.975))
  return(band)})
conf_band

plot(x=state_ordered$MET, y=state_ordered$EX, col = "red", xlab = "MET", ylab = "EX", main = "95% conf. bands")
points(x=state_ordered$MET , y=pred, col="red", type = "l")
lines(x=state_ordered$MET, y=conf_band[1, ], col = "blue")
lines(x=state_ordered$MET, y=conf_band[2, ], col = "blue")

# end ----------------------TEST

#-----Step 4-------
#parametric bootstrap for confidence intervalls:
#values for distr_gen. sigmna = std_dev
tree_model = tree(EX ~ MET, state_ordered, control = tree.control(nobs = nrow(state_ordered), minsize = 8))
pruned_tree = prune.tree(tree_model, best = best_size)
pred = predict(pruned_tree, newdata = state_ordered)
residuals = state_ordered$EX - pred
std_dev = sd(residuals)

distr_gen = function(data, tree_model){
   pred = predict(tree_model, newdata = data)
   res = rnorm(nrow(data), mean = pred, sd= std_dev )
   data$EX = res
   return(data)
}

tree_model = pruned_tree

stat = function(data){
  tree_model = tree(EX ~ MET, data=data,
                    control = tree.control(nobs = nrow(data), 
                                           minsize = 8))
  pruned_tree = prune.tree(tree_model, best = best_size)
  pred = predict(pruned_tree, newdata = state_ordered)
  return(pred)
}

res_para = boot(state_ordered, statistic = stat , 
     mle = tree_model, 
     R = 1000, 
     sim = "parametric", 
     ran.gen = distr_gen )

boot_data = data.frame(res_para$t)
conf_band = apply(boot_data, 2, function(col){
  band = quantile(col, probs = c(0.025, 0.975))
  return(band)})
conf_band

plot(x=state_ordered$MET, y=state_ordered$EX, col = "red", xlab = "MET", ylab = "EX", main = "95% conf. bands")
points(x=state_ordered$MET , y=pred, col="red", type = "l")
lines(x=state_ordered$MET, y=conf_band[1, ], col = "blue")
lines(x=state_ordered$MET, y=conf_band[2, ], col = "blue")

test = envelope(res_para)
lines(x=state_ordered$MET, y=test$point[1,], col = "green")
lines(x=state_ordered$MET, y=test$point[2,], col = "green")

#prediction bands
statistic = function(data){
  tree_model = tree(EX ~ MET, data=data,
                    control = tree.control(nobs = nrow(data), 
                                           minsize = 8))
  pruned_tree = prune.tree(tree_model, best = best_size)
  pred = predict(pruned_tree, newdata = state_ordered)
  res = rnorm(nrow(data), mean = pred, sd= std_dev )
  return(res)
}
res_pred_band = boot(state_ordered, statistic = statistic , 
                mle = tree_model, 
                R = 1000, 
                sim = "parametric", 
                ran.gen = distr_gen )
pred_band = envelope(res_pred_band)
lines(x=state_ordered$MET, y=pred_band$point[1,])
lines(x=state_ordered$MET, y=pred_band$point[2,])

#step 5

hist(residuals(pruned_tree), 
     breaks = 20, main ="Histogram of the residuals",
     xlab = "Residual")
#chi-square model would be preffered. 