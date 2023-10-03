p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

p(1, lambda = 1)
p(0, lambda = 1)

p(1, 1)
p(0, 1)


#P(Y>1)
p(2,1)+
p(3,1)+
p(4,1)+
p(5,1)+
p(6,1)+
p(7,1)+
p(8,1)

x <- seq(2,8)
sum(p(x,1))


x <- seq(2,100)
sum(p(x,1))


x <- seq(0,100000)
sum(p(x,1))

1-p(x=0,lambda=1)-p(x=1,lambda=1)
1-p(0,1)-p(1,1)
1-sum(p(0:1,1))

x <- seq(0,20)
plot(x,p(x,lambda=10))

#### 2Bi
x <- seq(0,7)
#P(x<8, lambda=3)
sum(p(x,3))
#P(x<8, lambda=6)
sum(p(x,6))
#P(x>=8, lambda=3)
1-sum(p(x,6))
sum(p(8:100,6))
#P(x>12, lambda=3)
sum(p(13:100,5))
sum(p(13:1000,5))



#2Bii1
p(7,8)
p(7,4)
#What's more likely?
p(7,6.3)
lamb <- seq(4,8,0.1)
lamb
p(7,lamb)
max(p(7,lamb))
plot(lamb, p(7,lamb))




p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v2 <- function(x1,x2,x3,lambda=2){
  # x1, x2, and x3 must be whole numbers
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}
p.3v1(c(4,4,8))
p.3v2(4,4,8)
p(4)*p(4)*p(8)



p.3v1(c(4,4,8),7)

lamb <- seq(5.33,5.34,0.001)
p.3v2(4,4,8, lambda = lamb)
plot(lamb,p.3v2(4,4,8, lambda = lamb),type = "l")
