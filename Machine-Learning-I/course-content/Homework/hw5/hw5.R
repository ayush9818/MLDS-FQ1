hw5 <- function(beta=c(1,-1,1.5,0.5,-0.5,rep(0,10)), rho=0.9, sigmae=3, seed=12345, ntrain=100, ntest=10000)
{
  set.seed(seed)
  n = ntrain + ntest
  Z <- matrix(rnorm(n*15), nrow=n)
  e <- rnorm(n)*sigmae
  sigma <- matrix(rho, nrow=15, ncol=15) + diag(rep(1-rho, 15))
  A <- chol(sigma)
  X <- Z %*% A
  y <- 3 + X %*% beta + e
  train <- data.frame(X[1:ntrain,], y=y[1:ntrain])
  test <- data.frame(X[(ntrain+1):n,], y=y[(ntrain+1):n])
  cat("-------------------------------------------\n")
  cat("correlation between x: ", rho, "\n")
  cat("Error variance: ", sigmae^2, "\n")

  # OLS of x1-x5 as predictors
  fit <- lm(y~X1+X2+X3+X4+X5, train)
  print(summary(fit))
  cat("OLS x1-x5:", mean((test$y-predict(fit,test))^2), "\n")

  # fit OLS model with x1-x15
  fit <- lm(y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15, train)
  cat("OLS x1-x15:", mean((test$y-predict(fit,test))^2), "\n")

  # backward
  fit2 <- step(fit, scope=~1, trace=F)
  cat("backward (", fit2$rank-1, "):", mean((test$y-predict(fit2,test))^2), "\n")

  # forward
  fit <- lm(y~1, train)
  fit2 <- step(fit, scope=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15, 
    direction="both", trace=F)
  cat("forward (", fit2$rank-1, "):", mean((test$y-predict(fit2,test))^2), "\n")

  # you add code to fit ridge and lasso models here

  invisible(list(train=train, test=test))
}
