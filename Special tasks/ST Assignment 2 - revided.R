RNGversion('3.5.1')
library(readr)
library(partykit)

women = read.csv2("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Special tasks/Women.csv")

# I had to change 2 things to make this Special task work.

# 1. Fisrtly i assumed that the data was automaticly passed to lm when calling lm() with the function
# "y ~ x^2" i came to learn that thats not the case. Instead i get y and x varibles seperate and I 
#  have to combind dem into a dataframe to generate my model by lm(). My changed fit funciton:
fit<-function(y, x, start = NULL, weights = NULL, offset = NULL, ...){
  #combind the seperate data for y(Blood.systolic) and x(height and weight)
  xy=cbind(x[,(2:3)],y)
  # convert it to a dataframe
  data=data.frame(xy)
  names(data) = c("height", "weight", "Blood.systolic")
  model=lm(Blood.systolic~.^2, data=data)
  return(model)
}


# 2. Second, my code produced another result because of my formula was stated wrong.
# I had the formula as : "1 | height + weight". It was supposed to be: 
# Blood.systolic~height+weight | height+weight.
# New formula implemented below:
tree_mob <- mob(Blood.systolic ~ height+weight | height + weight
                , data = women, fit = fit, control = mob_control(minsize=5000))


# Code below this point did not change.
#plot Tree
plot(tree_mob)

#Create grid, form values on height and weight:
grid = matrix(0, ncol = 100, nrow = 100) #rows corosponds to height
height = 111:210 #110-210 Dim=100
weight = 31:130 #30-130 Dim = 100
colnames(grid) = weight
rownames(grid) = height

#predict Blood.systolic for grid values.
for (i in 1:length(height)){
  data = data.frame(rep(height[i], length(weight)),weight )
  names(data) = c("height", "weight")
  grid[i, ] = predict(tree_mob, newdata = data, type="response")
}

library(reshape2)
library(ggplot2)
data = melt(grid)
ggplot(data,aes(x=Var1, y=Var2) ) +
  geom_raster(aes(fill = value)) +
  #scale_fill_gradient(low="grey90", high="red") +# Changed(removed) colours and x/y axix to make my
  labs(x="Weight", y="Height", title="Grid")       # plot look like the correct plot. Otherwise
                                                  # it still displays data the same way. 
