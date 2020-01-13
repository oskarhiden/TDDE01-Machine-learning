library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)

# Create dataset
trva <- data.frame(Var, Sin=sin(Var))

# Divide dataset into training and validation set
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

# Random initialization of the weights in the interval [-1, 1]
# 31 weights are used
winit <- runif(31, -1, 1)

# Function predicting MSE
MSE <- function(prediction, observation) {
  return (mean((observation - prediction)^2))
}

m <- 10
mse_val <- numeric()
mse_train <- numeric()
threshold <- numeric()

# Train model with different thresholds, compute each model and calculate MSEs
for(i in 1:m) {
  nn <- neuralnet(Sin ~ Var, data = tr, startweights = winit, hidden = c(10),
                  threshold = i/1000)
  
  pred_train <- compute(nn, covariate=tr$Var)$net.result
  pred_val <- compute(nn, covariate=va$Var)$net.result
  threshold[i] <- i/1000
  print(i)
  mse_val[i] <- MSE(pred_val, va$Sin)
  mse_train[i] <- MSE(pred_train, tr$Sin)
}

# Plot MSEs
plot(threshold, mse_val, type="o", ylab="MSE", xlab="Threshold (10^-3)", main = "Validation dataset")
plot(threshold, mse_train, type="o", ylab="MSE", xlab="Threshold (10^-3)", main = "Training dataset")

# Plot above shows the lowest MSE for threshold = 1/1000
# Train network with full dataset
nn <- neuralnet(Sin ~ Var, data = trva, startweights = winit, hidden = c(10),
                threshold = 4/1000)

# Plot network
plot(nn)

# Plot predictions
plot(prediction(nn)$rep1, col="black", main = "Prediction (black) vs Data (red)")
points(trva, col = "red")