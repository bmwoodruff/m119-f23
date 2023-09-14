(-2)^2
(-1)^2
(0)^2
(1)^2

x <- c(-2,-1,0,1,2,3,4,5)
x
x^2
x <- -2:5
x <- -20:300
x
x^2


x <- c(-2,-1,0,1,2,3,4,5)
x
x <- seq(-2,5)
x
x <- seq(-10,10,by = 2)
x
 
x <- seq(-2,5)
y <- x^2
plot(x,y)
plot(x,y, type = "l")


x <- seq(-2,5,0.1)
y <- x^2
plot(x,y)
plot(x,y, type = "l")

f <- function(x){x^2}
x <-  seq(-2,5,0.1)
y <- f(x)
plot(x,y, type = "l")
plot(x,f(x), type = "l")


f <- function(x){
  ifelse(x < 1, 2*x, 3)
}
f(2)
f(7)
f(-10)
f(1)
f(.999)

x <-  seq(-2,5,0.01)
y <- f(x)
#y
plot(x,y, type = "l")
plot(x,f(x), type = "l")



#Scatter plots
mtcars
?mtcars
mtcars$wt

par(mar=c(4,4,0.25,0.25))
x <- mtcars$wt
y <- mtcars$mpg
plot(x,y)


par(mar=c(4,4,2,0.25))
x <- mtcars$wt
y <- mtcars$mpg
plot(x,y,
     pch=11,
     xlab='weight (1000 lbs)',
     ylab='Miles per US gallon',main='Our 1st Scatter Plot')

#We can also put everything on a single line, as done below, but the above is easier to read.
plot(x,y,pch=16,xlab='weight (1000 lbs)',ylab='Miles per US gallon',main='Our 1st Scatter Plot')

