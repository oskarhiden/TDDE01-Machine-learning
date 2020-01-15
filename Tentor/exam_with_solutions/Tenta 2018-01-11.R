RNGversion('3.5.1')
library(readr)

#Assignment 1 

#Step 1
video = read.csv("C:/Users/oskar/OneDrive/Universitet/Luleå Tekniska högskola/Databaser 1/Dokument/Git Repro/TDDE01-Machine-learning/Tentor/exam_with_solutions/video.csv")
#video = read.csv("Desktop/video.csv")


set.seed(12345)
n=dim(video)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=video[id,]
test=video[-id,]

train1 = train
train1$codec = c()
train1$utime = c()
res = prcomp(train1)

lambda = res$sdev^2
lambda
#cumsum(lambda)
sprintf("%2.3f",lambda/sum(lambda)*100)
#sprintf("%2.3f",cumsum(lambda)/sum(lambda)*100)
# only one varible nessesary

#scaled
train1_scaled = scale(train1)
res_scaled = prcomp(train1_scaled)
lambda_scaled = res_scaled$sdev^2
sprintf("%2.3f",lambda_scaled/sum(lambda_scaled)*100)
sprintf("%2.3f",cumsum(lambda_scaled)/sum(lambda_scaled)*100)
# 9 varibles neassesary

# Different varibles has different scale. 
# Whithour scaling a lot of the variation comes from varibles with big absolute values, wheres in 
# % the change can be small.

#Step 2
library(pcr)
train$codec = c()
test$codec = c()
# = data.frame(scale(train))
#test = data.frame(scale(test))
MSE_train = numeric(17)
MSE_test = numeric(17)
for(i in 1:17){
  pcr_model = pcr(utime ~ ., ncomp=17, data=train, scale=TRUE)
  pred_train = predict(pcr_model, ncomp = i)
  pred_test = predict(pcr_model, ncomp = i, newdata = test)
  
  MSE_train[i] = mean((pred_train - train$utime)^2)
  MSE_test[i] = mean((pred_test - test$utime)^2)
  }
MSE_train
MSE_test

which.min(MSE_test)

plot(MSE_train, ylim = c(100,300), col = "red", type = "b", ylab = "MSE", xlab = "# of components")
points(MSE_test, col="blue", type = "b")
# When the number of components increases, the model becomes more complex and the bias goes 
# down while variance goes up. The optimal model should have the lowest test error, in this 
# case M=8. "Simpler models with similar test errors is often choosen, for ex. M=8."

#Step 3
pcr_model = pcr(utime ~ ., ncomp=8, data=train, scale=TRUE)
pcr_model$terms
# Equation: 
# Loadings:
#   Comp 1 Comp 2 Comp 3 Comp 4 Comp 5 Comp 6 Comp 7 Comp 8
# utime  1.736 -1.598 -6.774  0.953 -3.466  3.369 -3.255 -5.517

#Step 4
#class = numeric(nrow(video))
class = ifelse(video$codec == "mpeg4", "mpeg", "other")
id = which(class == "mpeg")
mpeg = video[id,]
other = video[-id,]
plot(y= mpeg$duration, x=mpeg$frames, col="blue",  main="duration vs frames")
points(y= other$duration, x= other$frames, col="red")
# an decion boundary would be good. 

# Step 5
library(MASS)
data = video
data$codec = c()
data = data.frame(scale(data))
video2 = cbind(data, as.factor(class))

lda_model = lda(class ~ duration + frames, data = video2)
pred_lda = predict(lda_model)
pred_lda$class
id = which(pred_lda$class == "mpeg")
mpeg = video[id,]
other = video[-id,]
plot(y= other$duration, x=other$frames, col="red",  main="duration vs frames")
points(y=mpeg$duration , x= mpeg$frames, col="blue")
#plot(x=video2$duration, y=video2$frames, col=pred_lda$class)

conf_matrix = table(class, pred_lda$class)
missclass = 1 - sum(diag(conf_matrix))/sum(conf_matrix)
missclass
# Why bad at classify?
# 
dur = lda_model$scaling[1]
fra = lda_model$scaling[2]

test = cbind(video2$duration*dur, video2$frames*fra)

# wtith col diff:
id = which(class == "mpeg")
mpeg = test[id,]
other = test[-id,]
plot(y= other[,1], x=other[,2], col="red",  main="duration vs frames", ylim = c(0,2), xlim = c(-2.5,1.5))
points(y=mpeg[,1] , x= mpeg[,2], col="blue")

lda_model$scaling
lda_model$prior
# The result of classification is rather bad. It is clear that covariance matrices per 
# class are very different. In addition, class-conditional distributions do not look like 
# multivariate normal.
#Step 6
library(tree)
# TEST
data = data.frame(cbind(video$duration, video$frames, as.factor(class)))
names(data) = c("duration", "frames", "class")
tree_model = tree(class~duration + frames, data = data)
plot(tree_model)
text(tree_model, pretty = 0)

dev = numeric(11)
for(i in 2:11){
  pruned_tree = prune.tree(tree_model, best=i)
  #predict(pruned_tree)
  dev[i] = deviance(pruned_tree)
}
dev
plot(dev)
#plot(cv.tree(tree_model)) ?????????????????
# END TEST

data3 = video
data3$class = ifelse(data3$codec == "mpeg4", "mpeg", "other")
data3$codec = c()
data3$class = as.factor(data3$class)

tree_model = tree(class ~ duration + frames, data = data3)
cv_tree = cv.tree(tree_model)
best_size = cv_tree$size[which.min(cv_tree$dev)] 
best_size #11 leaves in final tree with current settings in tree()
plot(cv_tree$size, cv_tree$dev, type="b")

# final tree
print(tree_model)
plot(tree_model)
text(tree_model)

# Since the decision boundary between the two classes is linear, but not perpendicular to
# any of the cordinate axis. The tree has to create this boundary by producing a "stair-like"
# decision bounary between the two classes. More leaves => more lika a linear boundary. 


# Assignment 2 - Support vector machines
spam = read.csv2("C:/Users/oskar/OneDrive/Universitet/Luleå Tekniska högskola/Databaser 1/Dokument/Git Repro/TDDE01-Machine-learning/Tentor/exam_with_solutions/spambase.csv")
#spam = read.csv2("Desktop/spambase.csv")
library(kernlab)

#pic model based on validation data(holdout method:
data = spam
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.6))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.2))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

#train models
svm_05 = ksvm(as.factor(Spam) ~ ., data=train, kernel="rbfdot", kpar= list(sigma=0.05), C = 0.5)
svm_1 =  ksvm(as.factor(Spam) ~ ., data=train, kernel="rbfdot", kpar= list(sigma=0.05), C = 1)
svm_5 =  ksvm(as.factor(Spam) ~ ., data=train, kernel="rbfdot", kpar= list(sigma=0.05), C = 5)

pred_05 = predict(svm_05, newdata = valid)
pred_1 = predict(svm_1, newdata = valid)
pred_5 = predict(svm_5, newdata = valid)
#table(valid$Spam, pred_05)

misclass = function(true, predict){
  table = table(true, predict)
  return(1-(sum(diag(table)/sum(table))))
}
misclasserror = numeric(3)

misclasserror[1] = misclass(valid$Spam, pred_05) 
misclasserror[2] = misclass(valid$Spam, pred_1) 
misclasserror[3] = misclass(valid$Spam, pred_5) 

misclasserror
plot(misclasserror, type="b")
# smallest error form C = 5, that model is choosen

# Step 2 - error estimation using retrained model on tarin+valid and then test on test data.
train_valid = rbind(test, valid)
final_model = ksvm(as.factor(Spam) ~ ., data=train_valid, kernel="rbfdot", kpar= list(sigma=0.05), C = 5)
final_pred = predict(final_model, newdata=test)
final_error = misclass(test$Spam, final_pred)
final_error
# Final error is 0.04561912

# TEST
alpha(final_model)
nSV(final_model)
coef(final_model)
# End TEST

# User_model by traing on all data, C=5
user_model = ksvm(as.factor(Spam) ~ ., data=spam, kernel="rbfdot", kpar= list(sigma=0.05), C = 5)

# C is the cost of constraints violation, this is the 'C'-constant of 
# the regularization term in the Lagrange formulation. Higher C means higher bias in training.


# Assignment 2 - Neural Networks
library(neuralnet)
set.seed(12345)

value = runif(50,0,10)
sin = sin(value)
data = data.frame(value, sin)
plot(data)

#devide data
train=data[1:25,]
valid=data[26:50,]
plot(train, col="blue")
points(valid, col="red")

#set.seed(12345)
winit = runif(31, -1, 1)

SE_tr = vector("numeric", length = 10)
SE_va = vector("numeric", length = 10) 

for(i in 1:10){
  nn <- neuralnet(sin~value, data = train, hidden = c(10), startweights = winit, threshold = i/1000)
  
  p_tr = predict(nn, newdata = train)
  SE_tr[i] = sum((train$sin - p_tr)^2)
  p_va = predict(nn, newdata = valid)
  SE_va[i] = sum((valid$sin - p_va)^2)
}

best_1 = which.min(SE_va) 
plot(SE_tr, col = "red",  ylab = "Sum of Squared Error", main = "one layer")
par(new=TRUE)
plot(SE_va, col = "blue", ylab = "Sum of Squared Error") 

#set.seed(12345)
#weight = runif(22, -1, 1)
SE_tr_2 = vector("numeric", length = 10)
SE_va_2 = vector("numeric", length = 10) 
for (i in 1:10){
  nn_2 = neuralnet(sin~value, data = train, hidden = c(3, 3), startweights = winit, threshold = threshold[i])
  
  p_tr_2 = predict(nn_2, newdata = train)
  SE_tr_2[i] = sum((train$sin - p_tr_2)^2)
  p_va_2 = predict(nn_2, newdata = valid)
  SE_va_2[i] = sum((valid$sin - p_va_2)^2)
}

best_2 = which.min(SE_va_2) 
plot(SE_tr_2, col = "red",  ylab = "Sum of Squared Error", main= "two layer")
par(new=TRUE)
plot(SE_va_2, col = "blue", ylab = "Sum of Squared Error") 

SE_va[best_1]
SE_va_2[best_2]

#best model = 1 layer of 10 hidden units with threshold = 1/1000
nn <- neuralnet(sin~value, data = train, hidden = c(10), startweights = winit, threshold = best_1/1000)

p_tr = predict(nn, newdata = train)
p_va = predict(nn, newdata = valid)

plot(x = valid$value, y=p_va, col="blue")
points(valid, col="red")

# This model was choosen because it generated the lowest squared error for validation data. 

# Step 2 - Generalisation error for NN above:
# Sampling more data:
set.seed(12345)
value = runif(25, 0, 10)
sin = sin(value)
test = data.frame(value, sin)

p_test = predict(nn, newdata = test)
MSE_test = sum((p_test-test$sin)^2)/nrow(test)
MSE_test
plot(x = test$value, y=p_test, col="blue")
points(test, col="red")

# 1 layer was the best model according to our error, to test our model further we could
# se that with the new data, error was still low. Therfore, a deeper nn is not always better
# it could lead to overfitting the data. generating a high variance by capturing a lot fo noise
