library(readr)
library(glmnet)
data <- read_csv2("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Lab 1/spambase.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

predModel = glm(Spam~., data=train, family = binomial)

missclass=function(X, Xfit){
  n=length(X)
  print(table(X, Xfit))
  return (1-sum(diag(table(X, Xfit)))/n)
}

#Pred 1
prediction_test = predict(predModel, test, type="response")
prediction2_test = prediction_test
prediction_test[prediction_test<=0.5]=0
prediction_test[prediction_test>0.5]=1
plot(prediction_test)
missClassResult_test = missclass(test[[49]], prediction_test)
print(missClassResult_test)

prediction_train = predict(predModel, train, type="response")
prediction2_train = prediction_train
prediction_train[prediction_train<=0.5]=0
prediction_train[prediction_train>0.5]=1
plot(prediction_train)
missClassResult_train = missclass(train[[49]], prediction_train)
print(missClassResult_train)

#Pred 2
prediction2_train[prediction2_train<=0.8]=0
prediction2_train[prediction2_train>0.8]=1

prediction2_test[prediction2_test<=0.8]=0
prediction2_test[prediction2_test>0.8]=1
#plot(prediction2)

missClassResult_8_train = missclass(train[[49]], prediction2_train)
missClassResult_8_test = missclass(test[[49]], prediction2_test)
print(missClassResult_8_train)
print(missClassResult_8_test) #New rule makes more missclassifications.


#kknn
library(kknn)

knnn= kknn(as.factor(Spam)~., train, test, k=30)
missClassResultk30 = missclass(test[[49]], knnn$fitted.values)
knnn= kknn(as.factor(Spam)~., train, train, k=30)
missClassResultk30_train = missclass(train[[49]], knnn$fitted.values)

knnn1= kknn(as.factor(Spam)~., train, test, k=1)
missClassResultk1 = missclass(test[[49]], knnn1$fitted.values)
knnn1= kknn(as.factor(Spam)~., train, train, k=1)
missClassResultk1_train = missclass(train[[49]], knnn1$fitted.values)

print(missClassResultk30)
print(missClassResultk30_train)

print(missClassResultk1) #not as good
print(missClassResultk1_train)




#Assignment2
machines<- read_csv2("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Lab 1/machines.csv")

hist(machines$Length)#exponential

#log-likelihood
log_likelihood = function(theta, data){
  p=0
  data = data$Length
  for(i in data){
    p = p + log( theta*exp(-theta*i))
  }
  return (p)
}

#find my Theta
findTheta = function(data){
  p = 1:100
  i=1
  for(theta in seq(from=0, to=10, by=0.1)){
    p[i]= log_likelihood(theta, data)
    i = i+1
  }
  theta = seq(from=0, to=10, by=0.1)
  plot(theta, p)
  return(p)
}

logPTotal = findTheta(machines)
logPSix = findTheta(machines[(1:6), ])

thetaTotal = seq(from=0, to=10, by=0.1)[which.max(logPTotal)]
thetaSixFirst = seq(from=0, to=10, by=0.1)[which.max(logPSix)]

print(thetaTotal)
print(thetaSixFirst)
theta = seq(from=0, to=10, by=0.1)
plot(theta, logPTotal, col="blue", ylim=c(-100,0))
par(new=TRUE)
plot(theta, logPSix, col="red", ylim=c(-100,0))

#whti is this?
#curve(dim(machines)[1]*log(x)-x*sum(machines), from=min(machines), ylim=c(-80,0), col="blue",  to=4, ylab="log(p(x|??))", sub="Red: 6 obs | Blue: All obs", xlab="??", add=FALSE)

#part4
#log-likelihood Bayesian
#log_likelihood_Bayesian = function(theta,lambda, data){
#  p=0
#  data = data$Length
#  for(i in data){
#    p = p +  #log( theta*exp(-theta*i)*lambda*exp(-lambda*theta))
#  }
#  return (p)
#}

findThetaBayesian = function(lambda, data){
  p = 1:100
  i=1
  for(theta in seq(from=0, to=10, by=0.1)){
    log_like_lambda = log(lambda)+(-theta*lambda)
    p[i]= log_like_lambda + log_likelihood(theta, data)
    i = i+1
  }
  theta = seq(from=0, to=10, by=0.1)
  plot(theta, p)
  return(p)
}

theta = seq(from=0, to=10, by=0.1)
LogsThetaBay = findThetaBayesian(10, machines)
thetaMaxBay = seq(from=0, to=10, by=0.1)[which.max(LogsThetaBay)]

plot(theta, logPTotal, col="blue", ylim=c(-100,-30))
par(new=TRUE)
plot(theta, LogsThetaBay, ylim=c(-100,-30))
print(thetaMaxBay)


#5
set.seed(12345)
#thetaTotal = seq(from=0, to=10, by=0.1)[which.max(logPTotal)]
new_Data = rexp(50, rate=thetaTotal)
old_Data = machines$Length

#plot hist in one diagram
p1 <- hist(old_Data)                     
p2 <- hist(new_Data)                     
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,7), ylim=c(0,35))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,7),ylim=c(0,35), add=T) #second histogram

# Both behave the same way. Are distributed alike, both following the exponential distributon. 
# new data followingtheta=1.1. which are generated from old_data

#Assignment 4
tecator <- read_csv2("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Lab 1/tecator.csv")

#-------1---------
plot(tecator$Moisture, tecator$Protein)
# Looks lika a line, makes me think that a linear model would be good.

#-------2---------
#Consider model ???????????????? in which Moisture is normally distributed, and the expected
#Moisture is a polynomial function of Protein including the polynomial terms up to power
#???????? (i.e M1 is a linear model, M2 is a quadratic model and so on). Report a probabilistic
#model that describes ????????????????. Why is it appropriate to use MSE criterion when fitting 
#this model to a training data?

#-------3---------

n=dim(tecator)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
t_train=tecator[id,]
t_test=tecator[-id,]

mse = function(x, x_pred){
  squared_error = (x-x_pred)^2
  mse = sum(squared_error)/length(x)
  return(mse)
}

#sum (i=0-n) bi*x^i

model_i_1 = glm(Moisture~Protein, data=t_train)
model_i_2 = glm(Moisture~Protein + I(Protein^2), data=t_train)
model_i_3 = glm(Moisture~Protein + I(Protein^2) + I(Protein^3), data=t_train)
model_i_4 = glm(Moisture~Protein + I(Protein^2) + I(Protein^3) + I(Protein^4), data=t_train)
model_i_5 = glm(Moisture~Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5), data=t_train)
model_i_6 = glm(Moisture~Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5) + I(Protein^6), data=t_train)

#predictions for test data
pred_i_1 = predict(model_i_1, newdata = t_test)
pred_i_2 = predict(model_i_2, newdata = t_test)
pred_i_3 = predict(model_i_3, newdata = t_test)
pred_i_4 = predict(model_i_4, newdata = t_test)
pred_i_5 = predict(model_i_5, newdata = t_test)
pred_i_6 = predict(model_i_6, newdata = t_test)

#predictions for train data
pred_i_1_train = predict(model_i_1, newdata = t_train)
pred_i_2_train = predict(model_i_2, newdata = t_train)
pred_i_3_train = predict(model_i_3, newdata = t_train)
pred_i_4_train = predict(model_i_4, newdata = t_train)
pred_i_5_train = predict(model_i_5, newdata = t_train)
pred_i_6_train = predict(model_i_6, newdata = t_train)

#mse for test data
mse_test_1 = mse(t_test$Moisture, pred_i_1)
mse_test_2 = mse(t_test$Moisture, pred_i_2)
mse_test_3 = mse(t_test$Moisture, pred_i_3)
mse_test_4 = mse(t_test$Moisture, pred_i_4)
mse_test_5 = mse(t_test$Moisture, pred_i_5)
mse_test_6 = mse(t_test$Moisture, pred_i_6)

#mse for train data
mse_test_1_train = mse(t_train$Moisture, pred_i_1_train)
mse_test_2_train = mse(t_train$Moisture, pred_i_2_train)
mse_test_3_train = mse(t_train$Moisture, pred_i_3_train)
mse_test_4_train = mse(t_train$Moisture, pred_i_4_train)
mse_test_5_train = mse(t_train$Moisture, pred_i_5_train)
mse_test_6_train = mse(t_train$Moisture, pred_i_6_train)

print(mse_test_1)
print(mse_test_2)
print(mse_test_3)
print(mse_test_4)
print(mse_test_5)
print(mse_test_6)

print(mse_test_1_train)
print(mse_test_2_train)
print(mse_test_3_train)
print(mse_test_4_train)
print(mse_test_5_train)
print(mse_test_6_train)

mses_test = c(mse_test_1, mse_test_2, mse_test_3, mse_test_4, mse_test_5, mse_test_6)
mses_train = c(mse_test_1_train, mse_test_2_train, mse_test_3_train, mse_test_4_train, mse_test_5_train, mse_test_6_train)
which.min(mses_test)
which.min(mses_train)

plot((1:6), mses_test, col="red")

plot((1:6), mses_train, col="red")

# Both
plot((1:6), mses_test, col="blue", ylim=c(23,45) )
par(new=TRUE)
plot((1:6), mses_train, col="red", ylim=c(23,45)  )



#4
sub_of_tecator = subset(tecator, select = - c(Protein, Moisture, Sample))

n=dim(sub_of_tecator)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
t_train_sub=sub_of_tecator[id,]
t_test_sub=sub_of_tecator[-id,]
  
library(MASS)
linear_model_fat = glm(Fat~., data=sub_of_tecator)
step = stepAIC(linear_model_fat, direction = "both")

step$anova
summary(step)

covariates = scale(subset(sub_of_tecator, select = -Fat))
response = scale(subset(sub_of_tecator, select = Fat))

#-------5---------ridge
ridge = glmnet(as.matrix(covariates), response, alpha=0, family= "gaussian" )
plot(ridge, xvar="lambda", label=TRUE)

#-------6---------
lasso = glmnet(as.matrix(covariates), response, alpha=1, family= "gaussian")
plot(lasso, xvar = "lambda", label=TRUE) #higher lambda, lower vairance, higher variance.

#-------7---------
#gamma = 1, gives me lasso(0 is ridge), lambda = is which lambda to include. 
lasso_mse = cv.glmnet(covariates, response, gamma=1, lambda = seq(from = 0, to=1, by=0.01))
plot(lasso_mse)


#library #i=3 best.(readr)
#spambase <- read_delim("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Lab 1/spambase.csv", 
#                       ";", escape_double = FALSE, trim_ws = TRUE)