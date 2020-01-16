RNGversion('3.5.1')
library(readr)

influenza = read.csv("C:/Users/oskar/OneDrive/Universitet/Luleå Tekniska högskola/Databaser 1/Dokument/Git Repro/TDDE01-Machine-learning/Tentor/exam_without_solutions - TDDE01/Influenza.csv")
#video = read.csv("Desktop/video.csv")


# Step 1
hist(sort(influenza$Mortality, decreasing=T), breaks = 50)

lambda = seq(10, 2910, by=100)

log_p = function(y, lambda){
  res = -lambda + y*log(lambda)- sum(log(1:y))
  return(-res)
}

loglike = numeric(length(lambda))
for(j in 1:length(lambda)){
  for(i in 1:length(influenza$Mortality)){
    loglike[j] = loglike + log_p(influenza$Mortality[i], lambda[j])
  }
}

loglike
plot(x = lambda, y = loglike)
best_lambda = lambda[which.min(loglike[-1])+1]


# Step 2
scaled_data = influenza
scaled_data [ , -3] = scale(scaled_data[, -3])

data = scaled_data
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

library(glmnet)
lasso = glmnet(as.matrix(train[,-3]), train[,3], alpha=1, family= "poisson") 
plot(lasso, xvar = "lambda", label = T)

lasso_mse = cv.glmnet(as.matrix(train[,-3]), train[,3], alpha=1, family= "poisson")
plot(lasso_mse)
lambda_min = lasso_mse$lambda.min

final_lasso = glmnet(as.matrix(train[,-3]), train[,3], alpha=1, family= "poisson", lambda = lambda_min) 
pred_test = predict(final_lasso, newx = as.matrix(test[,-3]))
MSE = sum((exp(pred_test) - test$Mortality)^2)/length(pred_test)
MSE

# Is MSE actually the best way to measure error rate? 
# No since Max-likelihood is the same as min MSE only when pred is normaly distributed.
# instead poisson deviance should be used. 

final_lasso$df #number of vaiabls used
final_lasso$beta #lasso coefi
# biggest impact would be Influenza_lag2, influenza and temp deficit

final_lasso$a0 # intercept
exp(final_lasso$a0)
best_lambda
mean(influenza$Mortality)
# they are similar, the exp(intercept) is simmilar to the best lambda(close to sample mean), 
log(1600)
log(2600)


# Step 3 - regression tree
library(tree)
tree_model = tree(Mortality ~ ., data = train)
plot(tree_model)
text(tree_model)
cv_tree = cv.tree(tree_model)
plot(cv_tree)
cv_tree$dev
cv_tree
# best tree = 8 levs
best_tree = prune.tree(tree_model, best =8)
pred_tree = predict(best_tree, newdata = test)
MSE_tree = sum((pred_tree - test$Mortality)^2)/length(pred_tree)

# Compare MSE
MSE
MSE_tree

plot(test$Mortality)
points(pred_tree, col="green")
points(exp(pred_test), col="blue")
# In a tree we do varible selection by limiting the number of leavs in the tree. The varibles
# that explains most of the variation is used.


# Step 4
pca_data = train
pca_data$Mortality = c()

pca = prcomp(pca_data)
lambda = pca$sdev^2
sprintf("%2.3f",cumsum(lambda)/sum(lambda)*100) 
# 5 principal components is needed

pca$rotation
pca_data = pca$x
pca_data$PC1

x = pca_data[, (1:5)]
str(x)

#lasso
lasso_model_pca = glmnet(x, train$Mortality, alpha=1, family= "poisson" )
lasso_model_pca
lasso_model_pca = cv.glmnet(x, train$Mortality, alpha=1, family= "poisson", lambda = seq(0, 50, by=0.1) )
lambda_min_pca = lasso_model_pca$lambda.min
lambda_min_pca #5.2
plot(lasso_model_pca) # error vs log(lambda)
# No, higher values of lambda makes the model more simple by removing number of pca components
# used in the lasso-regression
coef(lasso_model_pca, s=lambda_min_pca)
coef(glmnet(x, train$Mortality, alpha=1, family= "poisson" , lambda=lambda_min_pca))
# 3 PC-components are used!
# Yi~P(exp(7.484791442 -0.029906449*PC1 -0.013042022*PC2 +0.005404855*PC4))


#Assignment 2 - Neural Network
install.packages("neuralnet")
library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 3)
tr <- data.frame(Var, Sin=sin(Var))
Var <- runif(50, 3, 9)
te <- data.frame(Var, Sin=sin(Var))

winit = runif(20, -1, 1)

nn = neuralnet(Sin ~ Var, data = tr, hidden = c(3), startweights = winit)
pred = predict(nn, newdata = te)

plot(tr, ylim=c(-2,2), xlim=c(0, 8))
points(x=te$Var, y=pred, col="red")
points(te, col="blue")

# Step 2
# nn weights:
nn$weights
plot(nn)

# calculate hidden units for data:
logistic_sigmoid = function (x){
  return(1/(1+exp(-x)))
}
x = te$Var
x = sort(x)
z1 = logistic_sigmoid(x*0.6170477-1.5198771)
z2 = logistic_sigmoid(1.995*x-1.27708)
z3 = logistic_sigmoid(-1.61733*x+4.89639)
y=-3.92871*z1+2.67522*z2+0.84607*z3-0.62953

plot(z1, ylim = c(0,1)) # -> 1
points(z2, col="blue") # -> 1
points(z3, col="red") # -> zero
# For larges values y torwards:
-3.92871+2.67522-0.62953
print(y) # -> -3.92871+2.67522-0.62953 = -1.88302

