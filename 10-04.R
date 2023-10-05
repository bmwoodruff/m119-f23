p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v1(c(8,2,8,8,4),2)
p.3v1(c(8,2,8,8,4),3)
p.3v1(c(8,2,8,8,4),4)
p.3v1(c(8,2,8,8,4),5)
p.3v1(c(8,2,8,8,4),6)
p.3v1(c(8,2,8,8,4),6.2)


rm(list=ls())

###Define the distribution###
p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

###Define the likelihood function###
LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod(p(x,lambda))
}

###Possible Parameter Values###
lambda <- seq(0,10,0.001)

###Data###
# Florida Hurricane Data (2000-2022)
data <- c(8,2,8,8,4)

#Here we calculate the output from the likelihood function given the observed data.
y <- sapply(lambda,FUN=LP,x=data)

#We plot the likelihood function.
par(mar=c(2.5,2.5,3,0.25))
plot(lambda,y,type='l',main='Poisson Likelihood')
