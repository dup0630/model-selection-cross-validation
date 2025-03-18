# Univariate case --------------------------------------------------------------
n <- 100
sigma.sq <- 0.01
f <- function(X){return(sin(4*X) + (2*cos(20*X)))}
#f <- function(X){return(ifelse(X<0.3, 1, ifelse(X>0.6, 0.5, 0)))}
X <- runif(n, 0, 1)
e <- rnorm(n, 0, sigma.sq)
y <- f(X) + e
#Y <- f(X+e)
plot(y~X)

#K <- function(x){return(ifelse(-1<x & x<1, 0.5, 0))}
K <- function(x){return(dnorm(x, 0,1))}
den <- function(x, h){return(mean(K((x - X) / h)))}
reg <- function(x, h){return(mean(y * K((x - X) / h)) / den(x, h))}

h = 0.025
x_plot <- seq(0, 1, by=0.01)
y_plot <- rep(0, length(x_plot))
for (i in 1:length(x_plot)) {
  y_plot[i] <-  reg(x_plot[i], h)
}
plot(y~X)
points(f(x_plot)~x_plot, type="l", ylab="y", col="red")
points(y_plot~x_plot, type="l", col="blue")


# Multivariate case ------------------------------------------------------------
density <- function(x_0, X, kernel, h){
  terms <- apply(X, 1, function(row){ kernel((x_0 - row)/h) })
  return(mean(terms))
}

f_hat <- function(x_0, X, y, kernel, h){
  terms <- apply(X, 1, function(row){ y * kernel((x_0 - row)/h) })
  numerator <- mean(terms)
  denominator <- density(x_0, X, kernel, h)
  return(numerator / denominator)
}

K <- function(x){
  output <- prod(dnorm(x, 0, 1))
}

n <- 100
p <- 3
sigma.sq <- 0.05
f <- function(X){
  z <- sin(4*X[, 1]) + 2 * cos(20*X[, 2]) - 3*X[, 3]
  return(z)
}
X <- matrix(runif(n*p, min = -1, max = 1), ncol = p)
e <- rnorm(n, 0, sigma.sq)
y <- f(X) + e

x_0 <- runif(p, -1, 1)
f_hat(x_0, X, y, K, 0.5)



