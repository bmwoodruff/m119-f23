f <- function(x){3^(-x)}
f(2)
f(-2)
x <- seq(-2,2, 0.001)
plot(x, f(x), type = "l")

e^2
exp(2)
exp(1)


x <- seq(-4,20,0.1)
y <- 10-sqrt(x+4)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')


x <- seq(-4,200,0.1)
y <- 10-sqrt(x+4)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')



library(data4led)
dist <- led_time(2100)
hist(dist$percent_intensity,probability = TRUE)

rm(list=ls())
f0 <- function(L,a=0,b=1){
  # Make sure a < b when using this function.
  ifelse(L < a,NaN, ifelse(L <= b, 1/(b-a), NaN))
}

a <- 90
b <- 100
L <- seq(a,b,0.1)
y <- f0(L,a,b)

par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(L,y,type='l',xlim=c(90,110), ylim = c(0,.2))
mtext('For f0: a=90, b=100', side = 3, line = 0)

b <- 110
L <- seq(a,b,0.1)
y <- f0(L,a,b)

plot(L,y,type='l',xlim=c(90,110), ylim = c(0,.2))
mtext('keep a=90, change b=110)', side = 3, line = 0)



rm(list=ls())
f1 <- function(L,h=0,a=1){
  #Make sure h > 0 and a > 0.
  1/sqrt(2*pi*a)*exp(-(L-h)^2/(2*a))
}

a <- 25
L <- seq(80,120,0.1)
h <- 103
y <- f1(L,h,a)

par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(L,y,type='l',xlim=c(80,120),ylim = c(0,.2))
mtext('plot f1 with h=103 and a=25', side = 3, line = 0)

a <- 10
y <- f1(L,h,a)

plot(L,y,type='l',xlim=c(80,120),ylim = c(0,.2))
mtext('change a=10, keep h=103', side = 3, line = 0)



