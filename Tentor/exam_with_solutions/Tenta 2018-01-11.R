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
