
# least square test

library(dplyr)

# linear regression 
# -----------------------------------------------------------------

df <- faithful %>% as_data_frame

X <- as.matrix(cbind(1, df$eruptions))
y <- as.matrix(df$waiting)


# https://en.wikipedia.org/wiki/Linear_least_squares_(mathematics)#Derivation_of_the_normal_equations

# solve problem with matrix multiplication

# solve = ^-1
b <- solve(t(X) %*% X) %*% (t(X) %*% y)

# check 
model <- lm(y ~ X[, 2])

# get all predictions
X %*% b

pred <- X %*% b
e <- y - pred

# sum of squared error
t(e) %*% e

# check 
aov(y ~ X)

model

# mutli variable
# -----------------------------------------------------------------

df <- mtcars %>% as_data_frame

X <- as.matrix(cbind(Intercept = 1,
                     df[, c(3, 4, 5, 6, 7)]))

y <- as.matrix(df[, 1])


b <- solve(t(X) %*% X) %*% (t(X) %*% y)

pred <- X %*% b
e <- y - pred

# sum of squared error
cbind(y, pred)


# costfunction
cf <- function(b, X, y) {
   sum((y - X %*% b)^2)
}

# optimize
bmin <- optim(c(0,0,0,0,0,0), cf, X = X, y = y)


own_lm <- function(formula, data) {
  m <- match.call()
  m[[1L]] <- quote(stats::model.frame)
  frame <- eval(m)
  y = as.matrix(frame[, 1])
  X = as.matrix(cbind(1, frame[, -1]))

  # get b
  if (length(y) < 1000) {
    b <- solve(t(X) %*% X) %*% (t(X) %*% y)
    m = "linear-algebra"
  } else {
    # initial b 
    b <- rnorm(ncol(X))
    b <- optim(b, cf, X = X, y = y)
    b <- b$par
    names(b) <- colnames(X)
    m = "optim"
  }
  prediction <- X %*% b
  residuals <- y - prediction
  SSE <- sum(residuals^2)
  
  structure(list(data = frame,
                 X = X,
                 y = y,
                 b = b,
                 prediction = prediction,
                 residuals = residuals,
                 SumSquaredError = SSE,
                 method = m),
            class = "own_lm")
}


print.own_lm <- function(model) {
  cat("Linear Regression\n\n")
  cat(paste("Method =", model[["method"]]),"\n\n")
  
  cat(model[["b"]])

}



formula <- mpg~disp+hp+qsec
model <- own_lm(formula, data = mtcars)

library(microbenchmark)

microbenchmark(own_lm(formula, data = mtcars),
               lm(formula, data = mtcars))

## own method is way slower than standard when using optim


own_lm(formula, data = mtcars)
lm(formula, data = mtcars)






