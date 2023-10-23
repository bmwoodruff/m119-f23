V <- function(x){6*x/4-x^3/4}
x <- seq(0,2.5,0.01)
plot(x,V(x), type = "l")
abline(h=0)

x <- seq(1.414,1.415,0.0001)
plot(x,V(x), type = "l")
sqrt(2)
