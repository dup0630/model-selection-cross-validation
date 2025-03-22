# Selection methods ------------------------------------------------------------
# Hold out (single split)
ho_selection <- function(X_est, y_est, X_eval, y_eval, model1, model2){
  # model 1
  f_hat1 <- model1(X_est, y_est)
  y_hat1 <- predict(f_hat1, X_eval)
  CV1 <- sum((y_eval - y_hat1)^2)
  # model 2
  f_hat2 <- model2(X_est, y_est)
  y_hat2 <- predict(f_hat2, X_eval)
  CV2 <- sum((y_eval - y_hat2)^2)
  
  return(ifelse(CV1 <= CV2, 1, 0))
}


# CV with voting
CV_v_selection <- function(X, y, model1, model2, n_eval, splits=100){
  n <- length(y)
  votes <- c()
  for (i in 1:splits) {
    # Split
    eval_indices <- sample(1:n, n_eval) # Run the risk of repeated splitting?
    est_indices <- setdiff(1:n, eval_indices)
    X_est <- X[est_indices, ]
    y_est <- y[est_indices]
    X_eval <- X[eval_indices, ]
    y_eval <- y[eval_indices]
    # Compute and vote
    tao <- ho_selection(X_est, y_est, X_eval, y_eval, model1, model2)
    votes <- c(votes, tao)
  }
  
  return(ifelse(mean(votes)>=0.5, 1, 0))
}


# CV with averaging (delete-n_eval)
CV_a_selection <- function(X, y, model1, model2, n_eval, splits=100){
  n <- length(y)
  CV1_losses <- c()
  CV2_losses <- c()
  for (i in 1:splits) {
    # Split
    eval_indices <- sample(1:n, n_eval)
    est_indices <- setdiff(1:n, eval_indices)
    X_est <- X[est_indices, ]
    y_est <- y[est_indices]
    X_eval <- X[eval_indices, ]
    y_eval <- y[eval_indices]
    # Compute losses
    # model 1
    f_hat1 <- model1(X_est, y_est)
    y_hat1 <- predict(f_hat1, X_eval)
    CV1 <- sum((y_eval - y_hat1)^2)
    CV1_losses <- c(CV1_losses, CV1)
    # model 2
    f_hat2 <- model2(X_est, y_est)
    y_hat2 <- predict(f_hat2, X_eval)
    CV2 <- sum((y_eval - y_hat2)^2)
    CV2_losses <- c(CV2_losses, CV2)
  }
  L1 <- mean(CV1_losses)
  L2 <- mean(CV2_losses)
  
  return(ifelse(L1 <= L2, 1, 0))
}

# Models for testing -----------------------------------------------------------

KernelRegression <- function(X, y, kernel, h=0.1) {
  if (missing(kernel)) {
    kernel <- function(x) output <- prod(dnorm(x, 0, 1)) # Gaussian kernel by default
  }
  
  density <- function(x_0, model) {
    terms <- apply(model$X, 1, function(row) kernel((x_0 - row) / model$h))
    return(mean(terms))
  }
  
  f_hat <- function(x_0, model) {
    weights <- apply(model$X, 1, function(row) kernel((x_0 - row) / model$h))
    numerator <- sum(weights * model$y)
    denominator <- sum(weights)
    
    if (denominator == 0) return(NA)
    return(numerator / denominator)
  }
  
  model <- list(f_hat = f_hat, density = density, kernel = kernel, h = h, X = X, y = y)
  class(model) <- "KernelRegression"
  return(model)
}

LinearRegression <- function(X, y) {
  beta <- solve(t(X) %*% X, t(X) %*% y) 
  model <- list(coefficients = beta, X = X, y = y)
  class(model) <- "LinearRegression"  # Assign a class
  return(model)
}

# Predict methods
predict.KernelRegression <- function(object, x_new, ...) {
  pred <- sapply(x_new, function(x) object$f_hat(x, object))
  return(pred)
}
predict.LinearRegression <- function(object, x_new, ...) {
  pred <- x_new %*% object$coefficients
  return(pred)
}

# Experiment 1: Nonparametric vs linear model ----------------------------------

n <- 500
p <- 3
sigma_sq <- 0.001
f <- function(X){
  z <- sin(4*X[, 1]) + 2 * cos(20*X[, 2]) - 3*X[, 3]
  return(z)
}
X <- matrix(runif(n*p, min = -1, max = 1), ncol = p)
e <- rnorm(n, 0, sigma_sq)
y <- f(X) + e

n_eval=250

CV_v_selection(X, y, KernelRegression, LinearRegression, n_eval)
CV_a_selection(X, y, KernelRegression, LinearRegression, n_eval)











