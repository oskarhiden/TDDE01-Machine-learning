RNGversion('3.5.1')
library(readr)
library(glmnet)
spambase <- read_csv2("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Lab 1/spambase.csv")


#Random lasso
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
  
  return(coef)
}

#bootstrap loop 1
set.seed(12345)
for(i in 1:B){
  sample = sort(sample(1:n, p_1))
  sample = c(sample, 49)
  #spambase[,sample]
  coef = stat(spambase[,sample])
  beta[sample[1:24],i] =as.numeric(coef[2:(p_1+1),1]) 
  
}
#1c calculate I
I = rep(0, n)

for(i in 1:n){
  I[i] = sum(abs(beta[i,]))/length(beta[i,])
}

#plot of 10 most important features:
importance_matrix = data.frame((1:48),I, abs(I))
importance_matrix = importance_matrix[order(importance_matrix$abs.I., decreasing = TRUE),]

x = importance_matrix[1:10, 3]
names(x) = importance_matrix[1:10, 1]
barplot(x, main = "10 most important features")



#step 2
beta_2 = matrix(0, ncol = B, nrow = n )
intercept = rep(0, B)
set.seed(12345)
for(i in 1:B){
  sample = sort(sample(1:n, p_1, prob = abs(I)))
  sample = c(sample, 49)
  coef = stat(spambase[,sample])
  beta_2[sample[1:24],i] =as.numeric(coef[2:(p_1+1),1]) 
  intercept[i] = coef[1,1]
}

#calculate final beta:
final_beta = rep(0, n)

for(i in 1:n){
  final_beta[i] = sum(beta_2[i,])/length(beta_2[i,])
}


x_words = as.matrix(subset(spambase, select = -Spam))
y_spam =  as.matrix(subset(spambase, select = Spam))

#final prediction
y = x_words%*% final_beta + sum(intercept)/B

prediction = ifelse(y >=0, 1, 0)
table = table(y_spam, prediction)
table

misclass = 1-sum(diag(table))/sum(table)
misclass

