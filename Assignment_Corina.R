#############################################################################

## Assignment Activity - Predictive Analytics

############################################################################

## Load the tidyverse library
library(tidyverse)

## Import the games sales data set
sales <- read.csv(file.choose (), header = T)

## Explore the data set
summary(sales)
head(sales)
str(sales)

## Identify relationships between the two set of variables - North America Sales and Global Sales; and EU Sales and Global Sales
## Find correlation, but only for the numeric variables
cor(sales[, unlist(lapply(sales, is.numeric))])

# Plot the relationship with base R graphics
plot(sales$NA_Sales, sales$Global_Sales)

## Fit the simple linear regression model
model1 <- lm(Global_Sales ~ NA_Sales, data = sales)
## View the model
model1

## View more outputs for the model - the full regression table
summary(model1)

## NA_Sales is a highly significant value (p<0). It explains 88% of the variation in Global Sales

## View residuals on a plot
plot(model1$residuals)

## Add line-of-best-fit
abline(coefficients(model1))

## Complete a log transformation with dplyr's mutate() function
sales <- mutate(sales, log = log(Global_Sales)

## View new object with new variable

head(sales)

## Create a new model using log
model2 <- lm(Global_Sales ~ NA_Sales, data = sales)

## View full regression table
summary(model2)

## Plot the relationship between year and logIndex
plot(sales$NA_Sales, sales$log)

## Add a line-of-best fit
abline(coefficients(model2))


## Make a forecast with this model
## View the last six rows of the data set
tail(sales)

# change data type for Year
class(sales$Year) = "Integer"
# check to see what is the last year in the dataset
max(sales$Year)

# not sure why this returns "N/A" - not able to find the latest year
# will asume the prediction is for 2023

## Create a new data frame for the forecast values
SalesForecast <- data.frame(Year = 2023:2025)

## Predict for 2023
predict(model2, newdata = SalesForecast) #receive error on object "NA_Sales" not found and can't figure out why

## Add the values to the SalesForecast data frame
SalesForecast$log <- predict(model1, newdata = SalesForecast)

## Add the actual index as opposed to the log index by exponentiation
SalesForecast <- mutate(SalesForecast,
                      Index = exp(log))

## View the SalesForecast data frame
SalesForecast

## Remember to save your work
