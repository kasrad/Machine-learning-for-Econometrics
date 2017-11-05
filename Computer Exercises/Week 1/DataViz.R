#setup
install.packages('tidyverse')
require(tidyverse)
require(ggplot2)

str(iris)
head(iris)
summary(iris)
View(iris)

#stacked histogram
ggplot(data = iris) +
  geom_histogram(binwidth = 0.2, aes(x = iris$Sepal.Width, fill = Species), colour = 'black', size=1.2)+
  xlab('Sepal Width') + ylab('Frequency') + ggtitle('Histogram of Sepal Width')

#density plots

ggplot(iris) + geom_density(aes(x = iris$Sepal.Length, fill = Species, colour = Species), alpha = 0.5) +
  xlab('Sepal Length') + ylab('density') + ggtitle('Iris dataset: Smoothed Density of Sepal Length per Species')

#Scatter plot

ggplot(iris) + geom_point(aes(x = iris$Sepal.Length, y = iris$Sepal.Width, shape = Species, colour = Species)) +
  geom_smooth(method = lm, aes(x = iris$Sepal.Length, y = iris$Sepal.Width, colour = Species)) + 
  xlab('Sepal Length') + ylab('Sepal Width') + ggtitle('Scatterplot with smoothers')

#Faceting

ggplot(iris) + geom_point(aes(x = iris$Sepal.Length, y = iris$Sepal.Width, shape = Species, colour = Species)) +
  geom_smooth(method = lm, aes(x = iris$Sepal.Length, y = iris$Sepal.Width, colour = Species)) + 
  xlab('Sepal Length') + ylab('Sepal Width') + ggtitle('Scatterplot with smoothers') +
  facet_grid(. ~ Species)


