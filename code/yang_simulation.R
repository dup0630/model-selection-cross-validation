# Selection methods ------------------------------------------------------------
# Hold out (single split)
hold.out.select <- function(X_est, y_est, X_eval, y_eval, model1, model2){
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
CV.v.selection <- function(X, y, model1, model2, n_est){
  n <- length(y)
  subsets <- combn(n, n_est)
  size <- choose(n, n_est)
  votes <- c()
  for (i in 1:size) {
    # Split
    est_indices <- set[, i]
    eval_indices <- setdiff(1:n, set[, i])
    X_est <- X[est_indices, ]
    y_est <- y[est_indices]
    X_eval <- X[eval_indices, ]
    y_eval <- y[eval_indices]
    # Compute and vote
    tao <- hold.out.selection(X_est, y_est, X_eval, y_eval, model1, model2)
    votes <- c(votes, tao)
  }
  
  return(ifelse(mean(votes)>=0.5, 1, 0))
}


# CV with averaging (delete-n_eval)
CV.a.selection <- function(X, y, model1, model2, n_eval){
  n <- length(y)
  subsets <- combn(n, n_eval)
  size <- choose(n, n_eval)
  CV1_losses <- c()
  CV2_losses <- c()
  for (i in 1:size) {
    # Split
    eval_indices <- set[, i]
    est_indices <- setdiff(1:n, set[, i])
    X_est <- X[est_indices, ]
    y_est <- y[est_indices]
    X_eval <- X[eval_indices, ]
    y_eval <- y[eval_indices]
    # Compute losses
    # model 1
    f_hat1 <- model1(X_est, y_est)
    y_hat1 <- predict(f_hat1, X_eval)
    CV1 <- sum((y_eval - y_hat1)^2)
    CV1_losses <- c(CV1_losses, CV_1)
    # model 2
    f_hat2 <- model2(X_est, y_est)
    y_hat2 <- predict(f_hat2, X_eval)
    CV2 <- sum((y_eval - y_hat2)^2)
    CV2_losses <- c(CV2_losses, CV_2)
  }
  L1 <- mean(CV1_losses)
  L2 <- mean(CV2_losses)
  
  return(ifelse(L1 <= L2, 1, 0))
}

# Models for testing -----------------------------------------------------------

KernelRegression <- function(X, y, K, h){
  den <- function(x, h){ TBD }
  reg <- function(x, h){ TBD }
  model <- list(f_hat = f_hat, density = den, kernel = K, h = h, X = X, y = y)
  class(model) <- "KernelRegression"
}

LinearRegression <- function(X, y) {
  beta <- solve(t(X) %*% X, t(X) %*% y) 
  model <- list(coefficients = beta, X = X, y = y)
  class(model) <- "LinearRegression"  # Assign a class
  return(model)
}

predict.KernelRegression <- function(object, x_new, ...) {
  if (ncol(X_new) != nrow(object$coefficients)) {
    stop("Mismatch in number of predictors")
  }
  predictions <- TBD
  return(predictions)
}

predict.LinearRegression <- function(object, X_new, ...) {
  if (ncol(X_new) != nrow(object$coefficients)) {
    stop("Mismatch in number of predictors")
  }
  
  predictions <- X_new %*% object$coefficients
  return(predictions)
}

# Experiment 1: Nonparametric vs linear model ----------------------------------

n <- 100
p <- 3
sigma.sq <- 0.001
f <- function(X){
  z <- sin(4*X[, 1]) + 2 * cos(20*X[, 2]) - 3*X[, 3]
  return(z)
}
X <- matrix(runif(n*p, min = -1, max = 1), ncol = p)
e <- rnorm(n, 0, sigma.sq)
Y <- f(X) + e

plot(Y~X[,1])
points((sin(4*seq(-1, 1, by=0.01)))~seq(-1, 1, by=0.01), type="l")
plot(Y~X[,2])
points((2*cos(20*seq(-1, 1, by=0.01)))~seq(-1, 1, by=0.01), type="l")
plot(Y~X[,3])
points((-3*seq(-1, 1, by=0.01))~seq(-1, 1, by=0.01), type="l")











