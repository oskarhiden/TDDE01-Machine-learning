RNGversion('3.5.1')
library(readr)

#Assignment 1 

#Step 1
video = read.csv("C:/Users/oskar/OneDrive/Universitet/Luleå Tekniska högskola/Databaser 1/Dokument/Git Repro/TDDE01-Machine-learning/Tentor/exam_with_solutions/video.csv")

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
