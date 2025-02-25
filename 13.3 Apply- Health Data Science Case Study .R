#Clear Enviornment 
rm(list = ls())
cat("\014")

#Installing Necessary Packages
install.packages("NHANES")

#Import Libraries 
library(NHANES)
library(ggplot2)
library(dplyr)

#Load Data 
data("NHANES")

#Inspect Data 
str(head(NHANES), 5)
print(NHANES)
print(NHANES[, 1:18])
summary(NHANES)
sum(is.na(NHANES))
colSums(is.na(NHANES))



#Question 1: Write an R code using NHANES data to produce this figure.  
ggplot(NHANES, aes(x = factor(Gender), y = BMI, fill = Diabetes)) +
  geom_boxplot() +
  labs(x = "Gender", y = "BMI") +
  theme_classic() +
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_line(color = "gray", size = 0.25)
  ) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "blue")) +
  facet_grid(Diabetes ~ SurveyYr, space = "free_x", labeller = labeller(Diabetes = "Diabetes Status", SurveyYr = "Survey Year"))



#Question 2: (5 points) Describe the data, aesthetic mappings, and layers used for each of the following plots.  
data(mpg)
ggplot(mpg, aes(cty, hwy)) + geom_point() 
#Plot 1: Scatter Plot
#Data: Fuel economy data that the EPA makes available. 
#Aesthetic Mapping: x: cty , y: hwy
#Layers: geom_point() creates scatterplots. 

ggplot(diamonds, aes(carat, price)) + geom_point() 
#Plot 1: Scatter Plot
#Data: A dataset containing the prices and other attributes of almost 54,000 diamonds.
#Aesthetic Mapping: x: carat , y: price 
#Layers: geom_point creates scatterplot.

ggplot(economics, aes(date, unemploy)) + geom_line() 
#Plot 1: Scatter Plot
#Data: This dataset was produced from US economic time series data available from https://fred.stlouisfed.org/.
#Aesthetic Mapping:x: date , y: unemploy 
#Layers: geom_line connects observations ordered by the x value. 

ggplot(mpg, aes(cty)) + geom_histogram() 
#Plot 1: Scatter Plot
#Data: Fuel economy data that the EPA makes available.
#Aesthetic Mapping: x: date , y: unemploy
#Layers: geom_histogram is a representation of the distribution of a numeric variable. 


