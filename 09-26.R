f <- function(x){3^x-17}
interval <- c(2,3)
my_root <- uniroot(f, interval)$root

#The code below produces a plot that illustrates what uniroot found. 
x<-seq(interval[1],interval[2],0.1)
plot(x,f(x),type="l")
abline(h=0,col = "gray")
abline(v=my_root,col = "gray")
points(my_root,0, col = "red")


uniroot(f,c(2,3), tol = 1e-40)
?uniroot

lhs <- function(x){log(5*x-30,10)}
rhs <- function(x){0*x+1}
f <- function(x){lhs(x)-rhs(x)}


f <- function(x){log(5*x-30,10)-1}
uniroot(f,c(6.5,100))
uniroot(f,c(6.5,100),tol=1e-20)

uniroot(f,c(6.5,100))

uniroot(f,c(10,100))
f(10)
f(100)

f <- function(x){
  3*x-15-exp(-x+6)
}

uniroot(f,c(-100,100))
x <-seq(0,30,1)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")
