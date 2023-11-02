library(data4led)
bulb <- led_bulb(1,seed=123) #Remember to use your assigned seed!

t <- bulb$hours
y <- bulb$percent_intensity

c11 <- sum(t^2)
c12 <- sum(t^3)
b1 <- sum((y-100)*t)
c21 <- c12
c22 <- sum(t^4)
b2 <- sum((y-100)*t^2)
  
  ## Create a function to solve a system of equations. 
  solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
    c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
      (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
  }

sol <- solvesystem(c11, c12, b1, c21, c22, b2)
best.a1 <- sol[1] 
best.a2 <- sol[2] 

best.a1
best.a2


f2 <- function(x,a0=100,a1=best.a1,a2=best.a2){
  a0 + a1*x + a2*x^2
}

x <- seq(-10,80001,2)
par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,f2(x),col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f2(x),col=2)

#Let's calculate D in the second derivative test
fxx <- -c11
fyy <- -c22
fxy <- -c12
D <- fxx*fyy-fxy^2
#Is D positive or negative
D
#Since D is positive, 
#then it's either concave up everywhere
#or concave down everwhere.
#We check the concavity in the first variable direction
fxx
#fxx is negative, so concave down. 
#This means we found a maximum. 


#How long till the bulb burns out?
solve_me <- function(x){f2(x)-80}
uniroot(solve_me, c(0,40000))$root
