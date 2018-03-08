setwd("~/R") #basically do this for other data and make some prediction
library(RCurl)
data = read.csv("MustangPrice.csv")
data_url <- getURL("https://github.com/fivethirtyeight/data/blob/master/drug-use-by-age/drug-use-by-age.csv")
data_drugs <- read.csv(text = data.url)
fix(data,data.drugs)
rows = nrow(data) #set the number of rows to use in the bootstrap

#bootstrap function computes regression line and adds line to plot of original Price~Miles data,
#and extracts R^2 values from the regression object

bootstrap = function(data){
  boot = sample(rows, 25, r=T)# Picks a number from rows 25 times, as length of Mustang is 25
  price = data$Price[boot] 
  miles = data$Miles[boot] 
  model = lm(price~miles)
  abline(model,col=sample(rainbow(100)))# Adds line to plot initiated outside of function. Each line is colored using the rainbow palette 
  return(summary(model)$r.squared) # Able to call the r-squared (r.squared) value from the linear model if summary() is used
}

plot(data$Miles,data$Price,main="Regression Lines",xlab="Miles",ylab="Price")

bootstrapped_values = replicate(20,bootstrap(data)) #call bootstrap 20 times
confidence_interval = quantile(bootstrapped_values,c(.025,.975)) #the confidence interval of R^2 values

