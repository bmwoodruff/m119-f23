A <- matrix(c(1,2,3,4),nrow=2,ncol=2)
A
det(A)

A <- matrix(c(2,4,0,4,2,2,0,2,2),
            nrow=3,ncol=3)
A
det(A)




rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data3_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)


best_a <- sum(y*exp(-x))/sum(exp(-x)^2)
f <- function(x,a = best_a){a*exp(-x)}
x_values <- seq(-2,4,0.1)
plot(x,y)
lines(x_values,f(x_values))

#Check if second derivative is positive
2*sum(exp(-x)^2)



rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)

best_a <- sum(y*log(x))/sum(log(x)^2)
best_a
f <- function(x,a = best_a){a*log(x)}
x_values <- seq(-2,4,0.1)
plot(x,y)
lines(x_values,f(x_values))
