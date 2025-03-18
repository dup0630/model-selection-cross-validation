# Univariate case --------------------------------------------------------------
n <- 100
sigma.sq <- 0.5
f <- function(X){return(sin(4*X) + (2*cos(20*X)))}
#f <- function(X){return(ifelse(X<0.3, 1, ifelse(X>0.6, 0.5, 0)))}
X <- runif(n, 0, 1)
e <- rnorm(n, 0, sigma.sq)
Y <- f(X) + e
#Y <- f(X+e)
plot(Y~X)

#K <- function(x){return(ifelse(-1<x & x<1, 0.5, 0))}
K <- function(x){return(dnorm(x, 0,1))}
den <- function(x, h){return(mean(K((x - X) / h)))}
reg <- function(x, h){return(mean(Y * K((x - X) / h)) / den(x, h))}

h = 0.025
x <- seq(0, 1, by=0.01)
y <- rep(0, length(x))
for (i in 1:length(x)) {
  y[i] <-  reg(x[i], h)
}
plot(Y~X)
points(f(x)~x, type="l", ylab="y", col="red")
points(y~x, type="l", col="blue")


# Multivariate case ------------------------------------------------------------
n <- 100
p <- 3
sigma.sq <- 0.05
f <- function(X){
  z <- sin(4*X[, 1]) + 2 * cos(20*X[, 2]) - 3*X[, 3]
  return(z)
}
X <- matrix(runif(n*p, min = -1, max = 1), ncol = p)
e <- rnorm(n, 0, sigma.sq)
Y <- f(X) + e




