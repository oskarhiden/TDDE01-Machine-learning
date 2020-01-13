RNGversion('3.5.1')
library(readr)
library(partykit)

women = read.csv2("C:/Users/oskar/OneDrive/Universitet/Linköping Universitet/År4/Machine learning/Special tasks/Women.csv")

## a simple basic fitting function for lm
fit <- function(y, x, start = NULL, weights = NULL, offset = NULL) {
  lm(y ~ x^2, start = start, offset=offset, weights=weights)
}

## set up a logistic regression tree
tree_mob <- mob(Blood.systolic ~ 1 | height + weight
                , data = women, fit = fit, control = mob_control(minsize=5000))
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
ggplot(data,aes(x=Var2, y=Var1) ) +
  geom_raster(aes(fill = value)) +
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Weight", y="Height", title="Grid")
