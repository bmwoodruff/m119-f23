rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)


best_a <- sum(y*log(x))/sum(log(x)^2)
f <- function(x,a = best_a){a*log(x)}
plot(x,y)
xv <- seq(0,5,0.01)
lines(xv,f(xv))
