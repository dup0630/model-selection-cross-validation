# Selectors --------------------------------------------------------------------
# Hold out (single split)
ho_selection <- function(x_est, y_est, x_eval, y_eval, model1, model2){
  # model 1
  f_hat1 <- model1(x_est, y_est)
  y_hat1 <- f_hat1(x_eval)
  CV1 <- sum((y_eval - y_hat1)^2)
  # model 2
  f_hat2 <- model2(x_est, y_est)
  y_hat2 <- f_hat2(x_eval)
  CV2 <- sum((y_eval - y_hat2)^2)
  
  return(ifelse(CV1 <= CV2, 1, 0))
}

# CV with voting
CV_v_selection <- function(x, y, model1, model2, n_eval, n_splits=10){
  n <- length(y)
  votes <- c()
  for (i in 1:n_splits) {
    # Split
    split <- split_data(x, y, n_eval)
    x_est <- split[[1]]; y_est <- split[[2]]
    x_eval <- split[[3]]; y_eval <- split[[4]]
    # Compute and vote
    tao <- ho_selection(x_est, y_est, x_eval, y_eval, model1, model2)
    votes <- c(votes, tao)
  }
  
  return(ifelse(mean(votes)>=0.5, 1, 0))
}

split_data <- function(x, y, n_eval){
  n <- length(y)
  eval_indices <- sample(1:n, n_eval)
  est_indices <- setdiff(1:n, eval_indices)
  x_est <- x[est_indices]; y_est <- y[est_indices]
  x_eval <- x[eval_indices]; y_eval <- y[eval_indices]
  return(list(x_est, y_est, x_eval, y_eval))
}

# Candidate models -------------------------------------------------------------
# Fourth-degree polynomial
delta1 <- function(x, y) {
  d <- 5
  X <- t(sapply(x, function(x) x^(1:d)))
  model <- lm(y ~ X)
  beta_hat <- coef(model)
  f_hat <- function(x_new) {
    X_new <- cbind(Intercept = 1, t(sapply(x_new, function(x) x^(1:d))))
    as.numeric(X_new %*% beta_hat)
  }
  return(f_hat)
}

# Quadratic model
delta2 <- function(x, y) {
  d <- 2
  X <- t(sapply(x, function(x) x^(1:d)))
  model <- lm(y ~ X)
  beta_hat <- coef(model)
  f_hat <- function(x_new) {
    X_new <- cbind(Intercept = 1, t(sapply(x_new, function(x) x^(1:d))))
    as.numeric(X_new %*% beta_hat)
  }
  return(f_hat)
}

# Monte Carlo ------------------------------------------------------------------
r <- 1000
n <- 300
n_eval <- 100
sigma.sq <- 5
f_true <- function(x) {exp(0.2*x) + 10*x*exp(-0.2*x^2) -3}

x <- runif(n, -10, 10)
e <- rnorm(n, 0, sigma.sq)
y <- f_true(x) + e


ho_results <- c()
CV_v_results10 <- c()
CV_v_results20 <- c()

time1 <- Sys.time()
for (i in 1:r) {
  # Hold-out
  ho_split <- split_data(x, y, n_eval)
  x_est <- ho_split[[1]]; y_est <- ho_split[[2]]
  x_eval <- ho_split[[3]]; y_eval <- ho_split[[4]]
  ho_selected <- ho_selection(x_est, y_est, x_eval, y_eval, delta1, delta2)
  ho_results <- c(ho_results, ho_selected)
  
  # 10-split CV-v
  CV_v_selected10 <- CV_v_selection(x, y, delta1, delta2, n_eval, n_splits=10)
  CV_v_results10 <- c(CV_v_results10, CV_v_selected10)
  
  # 20-split CV-v
  CV_v_selected20 <- CV_v_selection(x, y, delta1, delta2, n_eval, n_splits=20)
  CV_v_results20 <- c(CV_v_results20, CV_v_selected20)
}
time2 <- Sys.time()
runtime <- time2 - time1

# Results
results <- cbind("HO" = mean(ho_results), 
                 "10-CV-v" = mean(CV_v_results10), 
                 "20-CV-v" = mean(CV_v_results20))
results
runtime

#         HO   10-CV-v 2  0-CV-v
# [1,] 0.808     0.996         1
# 
# Time difference of 46.21305 secs


# Plots:
plot(y~x)
model1 <- delta1(x,y)
model2 <- delta2(x,y)

x_plot <- seq(-10, 10, by = 0.1)
y_plot1 <- model1(x_plot)
y_plot2 <- model2(x_plot)
y_true <- f_true(x_plot)

points(y_plot1~x_plot, type="l", col="red" , lwd=3)
points(y_plot2~x_plot, type="l", col="blue", lwd=3)
points(y_true~x_plot, type="l", col="green", lwd=3)
legend(-10, 25, legend=c("delta1", "delta2", "true"), fill=c("red", "blue", "green"))




