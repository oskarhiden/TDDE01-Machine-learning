library(geosphere)

##### Functions #####
# Filter posterior times on same date
filterDataByTime <- function(data, date, time) {
  return (data[!(as.Date(data$date) == as.Date(date) &
                   as.numeric(difftime(strptime(data$time, format = "%H:%M:%S"),
                                       strptime(time, format = "%H:%M:%S"))) > 0),])
}

# Filter dates in data posterior to target date
filterDataByDate <- function(data, date, time) {
  return (data[!as.Date(data$date) >= as.Date(date),])
}

# Gaussian Kernel for distance
gaussianKernel_distance <- function(Xn, x, h) {
  u <- distHaversine(data.frame(Xn$longitude, Xn$latitude), x)/h
  k <- exp(-u^2)
  return(k)
}

# Gaussian Kernel for day
gaussianKernel_day <- function(Xn, x, h) {
  u <- (as.numeric(as.Date(Xn$date) - as.Date(x), unit="days"))/h
  k <- exp(-u^2)
  return(k)
}

# Gaussian Kernel for hours
gaussianKernel_hours <- function(Xn, x, h) {
  time_diff <- difftime(strptime(Xn$time , format = "%H:%M:%S"), 
                        strptime(x , format = "%H:%M:%S"))
  time_diff <- as.numeric(time_diff/(3600))
  u <- time_diff/h
  k <- exp(-u^2)
  return(k)
}

# Sum of kernels
temperatureEstimate <- function(st, df) {
  filtered_data <- filterDataByDate(st, df.date)
  filtered_data_time <- filtered_data
  k_distance <- gaussianKernel_distance(filtered_data, c(df.longitude, df.latitude), df.h_distance)
  k_day <- gaussianKernel_day(filtered_data, df.date, df.h_date)
  
  temp_sum <- vector(length = length(df.times))
  temp_mult <- vector(length = length(df.times))
  
  for (i in 1:length(df.times)){
    filtered_data_time <- filterDataByTime(filtered_data, df.date, df.times[i])
    k_hour <- gaussianKernel_hours(filtered_data, df.times[i], df.h_time)
    k_tot_sum <- k_distance + k_day + k_hour
    k_tot_mult <- k_distance * k_day * k_hour
    temp_sum[i] <- sum(k_tot_sum %*% filtered_data$air_temperature)/sum(k_tot_sum)
    temp_mult[i] <- sum(k_tot_mult %*% filtered_data$air_temperature)/sum(k_tot_mult)
    filtered_data_time <- filtered_data
  }
  return(list(temp_sum = temp_sum, temp_mult = temp_mult))
}

# Plot weights of Distance kernel
plotKernalDistanceH <- function(distances, h) {
  u <- distances/h
  k <- exp(-u^2)
  plot(k, type="l", xlab = "Distance")
}

# Plot weights of Date kernel
plotKernalDateH <- function(date_diff, h) {
  u <- date_diff/h
  k <- exp(-u^2)
  plot(k, type="l", xlab = "Date")
}

# Plot weights of Hour kernel
plotKernalHourH <- function(time_diff, h) {
  u <- time_diff/h
  k <- exp(-u^2)
  plot(k, type="l", xlab = "Hour", xlim=c(0,12))
}

##### User input start #####
# H-values based on plot below
df.h_distance <- 100000
df.h_date <- 10
df.h_time <- 4

# Target place in Sweden (Stockholm)
df.latitude <- 59.329323 
df.longitude <- 18.068581

# Target date
df.date <- "2013-11-04"
##### User input end #####

set.seed(1234567890)
stations <- read.csv("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Lab 3/stations.csv", fileEncoding="latin1")

temps <- read.csv("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Lab 3/temps50k.csv")
st <- merge(stations,temps,by="station_number")

df.times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")

# Plot k against distance to find good h-values
distance <- seq(0,300000,1)
date_diff <- seq(0,20,1)
time_diff <- seq(0,50,1)
plotKernalDistanceH(distance, df.h_distance)
plotKernalDateH(date_diff, df.h_date)
plotKernalHourH(time_diff, df.h_time)

# Returns vectors of temperature estimates
temperatures <- temperatureEstimate(st, df)

# Plot results
plot(temperatures$temp_sum, xaxt = "n", xlab="Time", ylab="Temperature", type="o", main = "Sum of kernels")
axis(1, at=1:length(df.times), labels=df.times)

plot(temperatures$temp_mult, xaxt = "n", xlab="", ylab="Temperature", type="o", main = "Product of kernels")
axis(1, at=1:length(df.times), labels=df.times)
