# Definitions ------------------------------------------------------------------

generate_data <- function(n, p, sigma.sq=1, true_model){
  X <- matrix(runif(n*p, min = -1, max = 1), ncol = p)
  X <- cbind(rep(1, n), X)
  beta <- rep(0, p+1)
  # Assign nonzero values to true-model entries only
  for (i in true_model) {
    beta[i] <- runif(1, 0, 1)
  }
  e <- rnorm(n, 0, sigma.sq)
  y <- (X %*% beta) + e
  return(list(y, X, beta))
}

OLS <- function(X, y) {
  beta_hat <- solve(t(X) %*% X) %*% (t(X) %*% y)
  return(beta_hat)
}

# Leave-one-out
loo.CV <- function(alpha, X, y){
  X_alpha <- X[ , alpha]
  n = length(y)
  H <- (X_alpha %*% solve(t(X_alpha) %*% X_alpha)) %*% t(X_alpha)
  leverages <- diag(H)
  beta_hat <- OLS(X_alpha, y)
  e_hat <- y - (X_alpha %*% beta_hat)
  L <- mean((e_hat / (1 - leverages)) ** 2)
  return(L)
}

# Hold out (single split)
hold.out <- function(alpha, X, y, ho_indices){
  n <- length(y)
  complement <- setdiff(1:n, ho_indices)
  X_train <- X[complement , alpha]
  y_train <- y[complement]
  X_test <- X[ho_indices , alpha]
  y_test <- y[ho_indices]
  
  beta_hat <- OLS(X_train, y_train)
  e_hat <- y_test - (X_test %*% beta_hat)
  return(mean(e_hat^2))
}
  
# K-fold cross-validation (all k-fold splits)
k.fold.CV <- function(alpha, X, y, k=10){
  n = length(y)
  partition <- split(1:n, sample(rep(1:k, length.out = n)))
  L_ho <- c()
  for (s in partition) {
    L_ho <- c(L_ho, hold.out(alpha, X, y, s)) # append new hold out loss
  }
  return(mean(L_ho))
}

# Correctness indicator
is.correct <- function(model, true_model){
  return(ifelse(length(model) >= length(true_model), 1, 0))
}

# Optimality indicator
is.best <- function(model, true_model){
  return(ifelse(length(model) == length(true_model), 1, 0))
}


# Monte Carlo Simulations ------------------------------------------------------

# Leave-one-out
loo.MC <- function(r=1000, n, p, sigma.sq=1, true_model){
  correct <- 0
  best <- 0
  for (i in 1:r) {
    D <- generate_data(n, p, sigma.sq, true_model)
    X <- D[[2]]
    y <- D[[1]]
    values <- rep(0, p)
    for (j in 1:p) {
      alpha <- 1:(j+1)
      values[j] <- loo.CV(alpha, X, y)
    }
    selected_model <- 1:(which.min(values)+1)
    
    correct <- correct + is.correct(selected_model, true_model)
    best <- best + is.best(selected_model, true_model)
  }
  loo_correct <- correct/r
  loo_best <- best/r
  return(c(loo_correct, loo_best))
}

# K-fold
k.fold.MC <- function(r=1000, k=10, n, p, sigma.sq=1, true_model){
  correct <- 0
  best <- 0
  for (i in 1:r) {
    D <- generate_data(n, p, sigma.sq, true_model)
    X <- D[[2]]
    y <- D[[1]]
    values <- rep(0, p)
    for (j in 1:p) {
      alpha <- 1:(j+1)
      values[j] <- k.fold.CV(alpha, X, y, k)
    }
    selected_model <- 1:(which.min(values)+1)
    
    correct <- correct + is.correct(selected_model, true_model)
    best <- best + is.best(selected_model, true_model)
  }
  kfold_correct <- correct/r
  kfold_best <- best/r
  return(c(kfold_correct, kfold_best))
}

# Hold out
ho.MC <- function(r=1000, test_prop=0.2, n, p, sigma.sq=1, true_model){
  correct <- 0
  best <- 0
  test_size <- as.integer(n * test_prop)
  for (i in 1:r) {
    D <- generate_data(n, p, sigma.sq, true_model)
    X <- D[[2]]
    y <- D[[1]]
    values <- rep(0, p)
    for (j in 1:p) {
      alpha <- 1:(j+1)
      ho_split <- sample(1:n, n%/%2, replace = FALSE)
      values[j] <- hold.out(alpha, X, y, ho_split)
    }
    selected_model <- 1:(which.min(values)+1)
    
    correct <- correct + is.correct(selected_model, true_model)
    best <- best + is.best(selected_model, true_model)
  }
  ho_correct <- correct/r
  ho_best <- best/r
  return(c(ho_correct, ho_best))
}


# Experiment 1 -----------------------------------------------------------------

n <- 1000
p <- 10
sigma.sq <- 1
true_model <- 1:5 # Including intercept
r <- 10000

loo_time1 <- Sys.time()
loo_results <- loo.MC(r, n, p, sigma.sq, true_model)
loo_time2 <- Sys.time()
loo_time <- loo_time2 - loo_time1

kfold_time1 <- Sys.time()
kfold_results <- k.fold.MC(r, k=10, n, p, sigma.sq, true_model)
kfold_time2 <- Sys.time()
kfold_time <- kfold_time2 - kfold_time1

ho_time1 <- Sys.time()
ho_results <- ho.MC(r, test_prop = 0.5, n, p, sigma.sq, true_model)
ho_time2 <- Sys.time()
ho_time <- ho_time2 - ho_time1

results <- data.frame("LOO" = loo_results, "ten_fold"=kfold_results, "HO_half"=ho_results)
times <- c(loo_time, kfold_time, ho_time)
results <- as.matrix(t(rbind(results, times)))
colnames(results) <- c("P_Correct", "P_Best", "Runtime")
results

#          P_Correct P_Best   Runtime
# LOO         0.7673 0.5327 14.794250
# ten_fold    0.8120 0.3499 69.220465
# HO_half     0.7792 0.1914  7.939249


# Experiment 2 -----------------------------------------------------------------

n_set <- 100 * 2^(1:5)
p <- 10
sigma.sq <- 1
true_model <- 1:5 # Including intercept
r <- 5000

loo_results <- rep(0, 10)
kfold_results <- rep(0, 10)
ho_results <- rep(0, 10)

for (n in n_set) {
  loo_results[n] <- loo.MC(r, n, p, sigma.sq, true_model)
  kfold_results[n] <- k.fold.MC(r, k=10, n, p, sigma.sq, true_model)
  ho_results[n] <- ho.MC(r, test_prop = 0.5, n, p, sigma.sq, true_model)
}

