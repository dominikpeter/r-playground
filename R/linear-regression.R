
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
optim(c(0,0), cf, X = X, y = y)

