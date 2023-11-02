library(data4led)
bulb <- led_bulb(1,seed=123)
t <- bulb$hours
y <- bulb$percent_intensity

d <- sum((y-100)*t)
b <- sum(t^2)
a1 <- d/b
a1

f1 <- function(x,a1=d/b){
  100+a1*x
}
par(mar = c(2.5,0.5,0.5,0.5))
plot(t,y)
tvalues <- seq(0,5000,10)
lines(tvalues, f1(tvalues))




## Let's create a function to solve this system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}
## Before moving forward, let's verify that the function works.
## The solution to $2x+3y=4$, $5x+6y=7$ is $(-1,2)$.  
## Does this function yield the same result?
solvesystem(2,3,4,5,6,7)

## Now we can solve the problem at hand. 
c11 <- sum(t^2)
  c12 <- sum(t^3)
  b1 <- sum((y-100)*t)
  c21 <- c12
  c22 <- sum(t^4)
  b2 <- sum((y-100)*t^2)
  
  sol <- solvesystem(c11,c12,b1,c21,c22,b2)
sol
a1 <- sol[1]
a2 <- sol[2]

f2 <- function(x,a1=sol[1], a2 =  sol[2]){
  100+a1*x+a2*x^2
}
par(mar = c(2.5,0.5,0.5,0.5))
plot(t,y)
tvalues <- seq(0,5000,10)
lines(tvalues, f2(tvalues))

d <- sum((y-100*exp(-0.00005*t))*t*exp(-0.00005*t))
b <- sum(t*exp(-0.00005*t)*t*exp(-0.00005*t))
a1 <- d/b
a1

f5 <- function(x,a1=d/b){
  100*exp(-0.00005*x)+a1*x*exp(-0.00005*x)
}
par(mar = c(2.5,0.5,0.5,0.5))
plot(t,y)
tvalues <- seq(0,5000,10)
lines(tvalues, f5(tvalues))



