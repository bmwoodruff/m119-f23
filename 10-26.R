b11 <- pi
b12 <- log(2)
c1 <- 7
i <- seq(1,3)
b21 <- sum(i^2)
b22 <- sum(i-1)
c2 <- sum(i-i^2)

a1 <- (c1*b22-b12*c2)/(b11*b22-b12*b21)
a2 <- (b11*c2-c1*b21)/(b11*b22-b12*b21)
a1
a2



solve_system <- function(b11,b12,c1,b21,b22,c2){
  x <- (c1*b22-b12*c2)/(b11*b22-b12*b21)
  y <- (b11*c2-c1*b21)/(b11*b22-b12*b21)
  c(x,y)
}
sol <- solve_system(2,3,4,5,6,7)
sol
sol[1]
sol[2]

n <- seq(1,44)
b11 <- sum(n)
b12 <- sum(3+0*n)
c1 <- sum(7+0*n)
b21 <- sum(5+0*n) 
b22 <- sum(n)
c2 <- sum(n^2)
solve_system(b11,b12,c1,b21,b22,c2)
