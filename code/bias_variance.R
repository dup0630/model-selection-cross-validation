generate_data <- function(n, p, sigma_sq=1, true_model){
  X <- matrix(runif(n*p, min = -1, max = 1), ncol = p)
  X <- cbind(rep(1, n), X)
  beta <- rep(0, p+1)
  # Assign nonzero values to true-model entries only
  for (i in true_model) {
    beta[i] <- runif(1, 0, 1)
  }
  e <- rnorm(n, 0, sigma_sq)
  y <- (X %*% beta) + e
  return(list(y, X, beta))
}

OLS <- function(X, y) {
  beta_hat <- solve(t(X) %*% X) %*% (t(X) %*% y)
  return(beta_hat)
}


n=100
sigma_sq = 1
true_model = c(1,2,4,7)

x <- 1:n
e <- rnorm(n, 0, sqrt(sigma_sq))
f_star <- 1 / (x^2)
f_star <- f_star / sqrt(mean(f_star^2))
y <- f_star + e
plot(x,y)

M_alpha <- function(p_alpha){
  X_alpha <- X[ , 1:p_alpha]
  H_alpha <- (X_alpha %*% solve(t(X_alpha)%*%X_alpha)) %*% t(X_alpha)
  return(diag(1, n) - H_alpha)
}
delta_alpha <- function(p_alpha){
  M <- M_alpha(p_alpha)
  prod <- (M %*% X) %*% beta
  return(norm(prod, type = "2") / n)
}

err <- function(p_alpha){
  return((sigma_sq * p_alpha)/n)
}

delta_alpha(3) + err(3)


app <- sapply(1:p, delta_alpha)
est <- sapply(1:p, err)
risk <- sapply(1:p, function(x){delta_alpha(x) + err(x)})
plot(est, type = 'l')
points(app, type = 'l')
points(risk)



# Parameters
n <- 100
sigma2 <- 1/4
set.seed(42)  # For reproducibility

# Construct f* with decay ~ 1/i^2 and normalize
i <- 1:n
f_star <- 1 / (i^2)
f_star <- f_star / sqrt(mean(f_star^2))  # Normalize so that mean(f_star^2) = 1

# Simulate noise and observation y
epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma2))
y <- f_star + epsilon

D <- 50
X <- diag(n)[, 1:D]

hat_f <- X %*% solve(t(X) %*% X) %*% t(X) %*% y
f_proj <- X %*% solve(t(X) %*% X) %*% t(X) %*% f_star
plot(i,y)
points(hat_f, col='red',type='l')
points(f_proj, col='blue',type='l')

# Initialize vectors to store errors
D_values <- 1:n
approx_error <- numeric(n)
estim_error <- numeric(n)
excess_risk <- numeric(n)

# Compute projection estimators for each D
for (D in D_values) {
  # Design matrix: identity with first D columns (canonical basis)
  X <- diag(n)[, 1:D]
  
  # Least squares estimate: projection of y onto span(X)
  hat_f <- X %*% solve(t(X) %*% X) %*% t(X) %*% y
  
  # Projection of f_star onto model space (approximation)
  f_proj <- X %*% solve(t(X) %*% X) %*% t(X) %*% f_star
  
  # Errors
  approx_error[D] <- mean((f_star - f_proj)^2)
  estim_error[D] <- mean((f_proj - hat_f)^2)
  excess_risk[D] <- mean((f_star - hat_f)^2)
}

# Plotting
plot(D_values, excess_risk, type = "l", col = "blue", lwd = 2, ylim = c(0, max(excess_risk)), xlim = c(-1, 101),
     ylab = "Error", xlab = "Model Dimension D", main = "Error Decomposition vs Model Dimension")
lines(D_values, approx_error, col = "red", lwd = 2, lty = 2)
lines(D_values, estim_error, col = "green", lwd = 2, lty = 3)
legend("topright", legend = c("Excess Risk", "Approximation Error", "Estimation Error"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), lwd = 2)



