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
plot(x= mpeg$duration, y=mpeg$frames, col="blue",  main="duration vs frames")
points(x= other$duration, y= other$frames, col="red")
# an decion boundary would be good. 

# Step 5
