f <- function(x){
  3*x
}

x <- seq(-4,7)
y <- f(x)
plot(x,f(x), type = "l")

plot(f, c(-4,7))


g <- function(x){
  -2+0*x
}
g(3)
g(5)
g(7)
g(c(3,5))
g("bob")
x <- seq(-5,5)
y <- g(x)
plot(x,g(x), type = "l")

h <- function(x,m,b){
  m*x+b
}
x <- seq(-5,5)
plot(x,h(x,m=3,b=0), type = "l")
plot(x,h(x,m=0,b=-2), type = "l")

plot(x,h(x,m=3,b=0), type = "l", col = 1)
lines(x,h(x,m=0,b=-2), type = "l", col = 2)
lines(x,h(x,m=0,b=-5), type = "l", col = 3)
lines(x,h(x,m=-4,b=+7), type = "l", col = 4)
lines(x,h(x,m=0,b=-2), type = "l", col = 5)

