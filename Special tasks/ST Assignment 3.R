RNGversion('3.5.1')
library(readr)

set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# plot(trva)
# plot(tr)
# plot(va)
w_j <- runif(10, -1, 1)
b_j <- runif(10, -1, 1)
w_k <- runif(10, -1, 1)
b_k <- runif(1, -1, 1)

l_rate <- 1/nrow(tr)^2
n_ite = 5000 #more iterations will make better predictions, ex 20000. 
error <- rep(0, n_ite)
error_va <- rep(0, n_ite)

for(i in 1:n_ite) {
  # error computation: Your code here
  #training data
  for (j in 1:nrow(tr)) {
    z_j = tanh(w_j * tr[j,1] + b_j)
    y_k = sum(w_k * z_j) + b_k
    error[i] = error[i] + (y_k - tr[j,2])^2
  }
  #validation data
  for (j in 1:nrow(va)) {
    z_j = tanh(w_j * va[j,1] + b_j)
    y_k = sum(w_k * z_j) + b_k
    error_va[i] = error_va[i] + (y_k - va[j,2])^2
  }
  # If more iterations are used, an if statement can be used here to break loop when validation
  # error starts to increase. 
  
  cat("i: ", i, ", error: ", error[i]/2, ", error_va: ", error_va[i]/2, "\n")
  flush.console()
  for(n in 1:nrow(tr)) {
    # 1. forward propagation: Your code here
    z_j = tanh(w_j * tr[n,1] + b_j) #var from tr
    y_k = sum(w_k * z_j) + b_k
    
    # backward propagation: Your code here
    #2. Compute
    d_k = y_k - tr[n,2] #sin from tr
    #3. Brackprop, compute:
    d_j = (1-z_j^2)*w_k*d_k
    
    deriv_w_k = d_k*z_j
    deriv_w_j = d_j * tr[n,1]
    deriv_b_k = d_k
    deriv_b_j = d_j
    
    w_k = w_k - deriv_w_k*l_rate
    w_j = w_j - deriv_w_j*l_rate
    b_k = b_k - deriv_b_k*l_rate
    b_j = b_j - deriv_b_j*l_rate
  }
}

# print final weights and errors
w_j
b_j
w_k
b_k
plot(error/2, ylim=c(0, 5))
points(error_va/2, col = "red")

# plot prediction on training data
pred <- matrix(nrow=nrow(tr), ncol=2)
for(n in 1:nrow(tr)) {
  z_j <-tanh(w_j * tr[n,1] + b_j)
  y_k <-sum(w_k * z_j) + b_k
  pred[n,] <- c(tr[n,]$Var, y_k)
}
plot(pred)
points(tr, col = "red")
# plot prediction on validation data
pred <- matrix(nrow=nrow(tr), ncol=2)
for(n in 1:nrow(va)) {
  z_j <-tanh(w_j * va[n,1] + b_j)
  y_k <-sum(w_k * z_j) + b_k
  pred[n,] <- c(va[n,]$Var, y_k)
}
plot(pred)
points(va, col = "red")

