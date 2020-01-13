RNGversion('3.5.1')
library(readr)

spambase <- read_csv2("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Lab 1/spambase.csv")

library(glmnet)
x_words = as.matrix(subset(spambase, select = -Spam))
y_spam =  as.matrix(subset(spambase, select = Spam))

#testing lasso
lasso = glmnet(x_words, y_spam, family = "binomial", alpha = 1)
plot(lasso, label = TRUE)

lasso_mse = cv.glmnet(x_words, y_spam, alpha=1, family = "binomial")
plot(lasso_mse)
lasso_mse$lambda.min
log(lasso_mse$lambda.min)
lasso_mse
coef(lasso_mse, s="lambda.min")

prediction = predict(lasso_mse, newx = x_words, type = "class")
prediction
#prediction = ifelse(prediction>0.5, 1, 0)
table = table(y_spam, prediction)
table
misclass = 1-(sum(diag(table))/sum(table))
misclass


#Testing bootstrap
library(boot)
data2 = spambase
#data2=data[order(data$Word1),]#reordering data according to Area
# computing bootstrap samples
f=function(data, ind){
  data1=data[ind,]# extract bootstrap sample
  res=cv.glmnet(, data=data1) #fit linear model
  #predict values for all Area values from the original data
  priceP=predict(res,newdata=data2)
  return(priceP)
}
res=boot(data2, f, R=100) #make bootstrap
res$t0
res
plot(res$t)

#Random lasso
n = ncol(spambase)-1
p_1 = round(n/2)
p_2 = p_1
B = 10 # use 100 later
beta = matrix(0, ncol = B, nrow = n )

stat = function(data, ind){
  data2 = data #[ind,]
  x_words = as.matrix(subset(data2, select = -Spam))
  y_spam =  as.factor(as.matrix(subset(data2, select = Spam)))
  
  lasso_mse = cv.glmnet(x_words, y_spam, alpha=1, family = "binomial", nfolds = 4)
  coef = coef(lasso_mse, s="lambda.min")
  #prediction = predict(lasso_mse, newx = x_words, type = "class")
  return(coef)
}

for(i in 1:B){
  set.seed(12345)
  sample = sort(sample(1:n, p_1))
  sample = c(sample, 49)
  #spambase[,sample]
  
  beta[i,sample] = boot(spambase[,sample], stat, R= 10)$t0
  
}

set.seed(12345)
sample = sort(sample(1:n, p_1))
sample = c(sample, 49)
#spambase[,sample]

beta[sample,1] = boot(spambase[,sample], stat, R= 10)$t0


set.seed(12345)
lasso_mse = cv.glmnet(x_words, y_spam, alpha=1, family = "binomial", nfolds = 4)
prediction = predict(lasso_mse, newx = x_words, type = "class")

table = table(y_spam, prediction)
misclass = 1-(sum(diag(table))/sum(table))



#Random lasso test 2 without boot()
n = ncol(spambase)-1
p_1 = round(n/2)
p_2 = p_1
B = 10 # use 100 later
beta = matrix(0, ncol = B, nrow = n )

stat = function(data){
  x_words = as.matrix(subset(data, select = -Spam))
  y_spam =  as.matrix(subset(data, select = Spam))
  
  lasso_mse = cv.glmnet(x_words, y_spam, alpha=1, family = "binomial", nfolds = 4)
  coef = coef(lasso_mse, s="lambda.min")
  #prediction = predict(lasso_mse, newx = x_words, type = "class")
  return(as.numeric(coef[2:(p_1+1),1]))
}

#bootstrap loop 1
set.seed(12345)
for(i in 1:B){
  sample = sort(sample(1:n, p_1))
  sample = c(sample, 49)
  #spambase[,sample]
  
  beta[sample[1:24],i] = stat(spambase[,sample])
  
}

beta
#1c calculate I
I = rep(0, n)

for(i in 1:n){
  I[i] = sum(beta[i,])/length(beta[i,])
}


#step 2
beta_2 = matrix(0, ncol = B, nrow = n )
for(i in 1:B){
  sample = sort(sample(1:n, p_1, prob = abs(I)))
  sample = c(sample, 49)
  #spambase[,sample]
  
  beta_2[sample[1:24],i] = stat(spambase[,sample])
  
}
beta_2

#calculate final beta:
final_beta = rep(0, n)

for(i in 1:n){
  final_beta[i] = sum(beta_2[i,])/length(beta_2[i,])
}
final_beta_matrix = data.frame((1:48),final_beta, abs(final_beta))
final_beta_matrix = final_beta_matrix[order(final_beta_matrix$abs.final_beta., decreasing = TRUE),]

#plot of 10 most important features:ö
x = final_beta_matrix[1:10, 3]
names(x) = final_beta_matrix[1:10, 1]
barplot(x)





#old shit that i do not use:
set.seed(12345)
sample = sort(sample(1:n, p_1))
sample = c(sample, 49)
#spambase[,sample]
data = spambase[,sample]
x_words = as.matrix(subset(data, select = -Spam))
y_spam =  as.matrix(subset(data, select = Spam))

lasso_mse = cv.glmnet(x_words, y_spam, alpha=1, family = "binomial", nfolds = 4)
coef = coef(lasso_mse, s="lambda.min")

beta[sample(1:24),1] <- as.numeric(coef[2:(p_1+1),1])
stat(spambase[,sample])


women = read.csv2("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Special tasks/Women.csv")
