setwd("~/R")
data = read.csv("regression_test.csv")
attach(data)

# Linear Regression:

model_linear = lm(Test~Length)
summary(model_linear)
plot(Length,Test)
abline(model_linear)
R2 = cor(Length,Test)*(sd(Test)/sd(Length)) 
R2

# Quadratic Regression

model_poly = lm(Test~poly(Length,4,raw=T),data=data)#creates fourth degree polynomial, raw=T gives nonorthogonal polynomials???

coef(summary(model_poly))# gives coefficients of model with some nice statistics, as opposed to coef ithout summary. 
coef(model_poly) # Check the coefficients of the function

limit_length = range(Length)
length_seq = seq(from=limit_length[1],to=limit_length[2])
predict = predict(model_poly,newdata=list(Length=length_seq), se.fit=T) #with list you can create headings accessible by $
# se.fit makes sure the stuff that we are going to need to call

se_interval = cbind(predict$fit-predict$se.fit*2,predict$fit+predict$se.fit*2) #confidence interval

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))#margin control
#mfrow sets number of columns and rows for graph
plot(Length,Test,xlim=limit_length,col="5",cex=.5)#cex sets scalin of plot dots
title("Polynomial Regression",outer = T)#outer sets name for whole display
lines(length_seq,predict$fit,lwd=.5,col=6, lty=3)#lty sets line type
matlines(length_seq,se_interval,lwd=6, col=35)#plots two columns agaisnt each other
# Returns same result as excel

# Power Regression

fix(data) # No zeroes in data
model1 = lm(log(Test)~log(Length),data=data)
plot(Length, Test,cex=.5)


#have to set se.fit to get the graph below to work
predict1 = predict(model1,newdata=list(Length=length_seq),se.fit=T)
y1 = exp(predict1$fit)
 #plots fitted line
lines(length_seq,y1)

summary(model1)
coef(model1)

# Alternative approach but returns very different function

#m <- nls(Test ~ I(c*Length^power), data = data, start = list(power = 1, c =1), trace = T) 
# Just another way to write this
m = nls(Test ~ (Length^b) ,start = list(b = 1),data=data,trace = T) 
predict1 = predict(m,newdata=list(Length=length_seq),se.fit=T)
#plot(Length, Test,cex=.5)
y = predict1
lines(length_seq,y, add=TRUE)

summary(m)
coef(m)

# Exponential Regression

par(mfrow = c(1,2))

model = lm(log(Test)~Length,data=data)
model = nls(Test~(exp(Length*b)), data = data, start = list(b = .6),trace=T)
# alternative method, requires that plot(Length, Test) is used and y = predict, as predict is a vector, not recursive object

plot(Length, log(Test),cex=.5)

predict = predict(model,newdata=list(Length=length_seq),se.fit=T)#have to set se to get the graph below to work
y = predict$fit # Plot in linear form
lines(length_seq,y) #plots fitted line

plot(Length, Test,cex=.5)
y1 = exp(predict$fit) # Plot as exponential form
lines(length_seq,y1) #plots fitted line

summary(model)
# Returns the same R^2 as excel, but not the same y intercept

# Logarithmic Regression

# Equation: y = a*log(x) - b
model_log = lm(Test~log(Length)) #log regression
predict = predict(model_log,newdata=list(Length=length_seq),se.fit=T)
lines(length_seq,predict$fit,col="19")
coef(model_log) # Returns a and b specified in above comment
summary(model_log) # Returns R^2
