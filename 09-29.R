f <- function(t){
  log(3*t,2)-2
  }
f(3)

solve_me <- function(x){f(x)-2}

uniroot(solve_me,c(2,10000))$root
uniroot(function(x){f(x)-2},c(2,10000))$root


x <- seq(4,7,0.1)
plot(x,f(x), type="l")
abline(h=2)

plot(x,solve_me(x), type="l")
abline(h=0)


f <- function(x){3*x-15}
solve_me <- function(x){f(x) - (exp(-x+6))}

uniroot(solve_me, c(0,10))$root
x <- seq(4,6, 0.1)
plot(x, solve_me(x), type = "l")
abline(h=0)

g <- function(x){
  3*x-15-exp(-x+6)
}

uniroot(g,c(0,7))$root

x <-seq(4,7,0.1)
plot(x,g(x), type="l")
abline(h=0, col = "lightgray")


# 1
f <- function(x){3*x-5}
solve_me <- function(x){f(x) - 0}
uniroot(solve_me, c(1,10))$root

# 2
f <- function(x){3*x-5}
solve_me <- function(x){f(x) - 7}
uniroot(solve_me, c(0,10))$root

# 3
f <- function(x){3*x-5}
solve_me <- function(x){f(x) - exp(-x)}
uniroot(solve_me, c(0,10))$root

# 5
f <- function(x){x^2+x-6}
solve_me <- function(x){f(x) - 0}
uniroot(solve_me, c(0,8))$root
uniroot(solve_me, c(0,10))$root
uniroot(solve_me, c(-20,10))$root
uniroot(solve_me, c(-20,-2))$root

x <-seq(-5,5,0.1)
plot(x,f(x), type="l")
abline(h=0)


# 6
f <- function(x){x^2-8*x+12}
solve_me <- function(x){f(x) - 0}

x <-seq(-5,5,0.1)
plot(x,f(x), type="l")
abline(h=0)

uniroot(solve_me, c(0,4))$root
uniroot(solve_me, c(4,10))$root



f <- function(x){
  1/x
}

uniroot(f,c(-10,-3))$root

uniroot(f,c(-1,1))$root

uniroot(f,c(-1,1))



#### Examples of finding good visual fits
rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y <- bulb$percent_intensity

f2 <- function(x,a0=0,a1=0,a2=1){ a0 + a1*x + a2*x^2 }
f3 <- function(x,a1=0,a2=1){ (100-a1) + a1*exp(-a2*x) }
f4 <- function(x,a0=0,a1=0,a2=1){a0+a1*x+a2*log(0.005*x+1)}
f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,80001,2)
y0 <- f2(x,a0=100,a1=0,a2=0)
y1 <- f2(x,a0=100,a1=7e-4,a2=0)
y2 <- f2(x,a0=100,a1=1.1e-3,a2=-1.5e-7)
y3 <- f3(x,a1=-1.9,a2=0.00114)
y4 <- f4(x,a0=100,a1=-1.81e-4,a2=0.83)
y5 <- f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)


par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f0')
lines(x,y0,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y0,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,y1,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y1,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,y2,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y2,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f3')
lines(x,y3,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y3,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,y4,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y4,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,y5,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y5,col=2)


f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }
solve_me <- function(x){f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)-80}
uniroot(solve_me, c(0,80000))$root
