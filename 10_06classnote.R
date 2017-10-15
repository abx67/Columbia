####10.6.2017####

# Input x should be a single p-value in [0,1]

significant <- function(x) {
  if (x <= 0.05) { return(TRUE) }
  else { return(FALSE) }
}


# Inputs: A vector of numbers (x)
# Outputs: A loss vector with x^2 for small elements,
# and 2|x|-1 for large ones
res_loss <- function(x) {
  loss_vec <- ifelse(x^2 > 1, 2*abs(x) - 1, x^2)
  return(loss_vec)
}

vec <- c(-0.5, 0.9, -3, 4)
res_loss(vec)

res_loss <- function(x) {
  loss_vec <- ifelse(x^2 > 1, 2*abs(x) - 1, x^2)
  return(loss_vec)
}


# Inputs: A vector of numbers (x),
# crossover location (c > 0)
# Outputs: A loss vector with x^2 for small elements,
# and 2c|x|-c for large ones
res_loss2 <- function(x, c = 1) {
  loss_vec <- ifelse(x^2 > c, 2*c*abs(x) - c, x^2)
  return(loss_vec)
}

identical(res_loss(vec), res_loss2(vec, c = 1))
identical(res_loss(vec), res_loss2(vec, c = 2))

identical(res_loss2(vec, c = 1), res_loss2(vec))
identical(res_loss2(x = vec, c = 2),
          res_loss2(c = 2, x = vec))
vec <- c(-0.5, 0.9, -3, 4)
res_loss2(vec, c = c(1,1,1,5))
res_loss2(vec, c = -1)


res_loss2 <- function(x, c = 1) {
  # Scale should be a single positive number
  stopifnot(length(c) == 1, c > 0)
  loss_vec <- ifelse(x^2 > c^2, 2*c*abs(x) - c, x^2)
  return(loss_vec)
}
res_loss2(vec, c = c(1,1,1,5))
res_loss2(vec, c = -1)

curve(res_loss2, from = -2, to = 2)

####globa##############
x <- 2
f <- function(y) {
  return(x + y)
}
f(1)
g <- function(y) {
  x <- 10
  return(x + y)
}
g(1)
x

g <- function(y) {
  f <- function(y) {
    return(x + y)
  }
  x <- 10
  return(f(y))
}
g(1)


3+2
'+'(3,2)
x <- matrix(runif(100), ncol = 10)
x[, 2]
'['(x, , 2)

class(1)
class(runif)
class(function(x) x^2)
square <- class(function(x) x^2)
class(square)


gmp <- read.table("gmp.txt", as.is = TRUE, header = TRUE)
head(gmp)[1:3, ]

gmp$pop <- gmp$gmp/gmp$pcgmp
head(gmp)[1:3, ]

plot(gmp$pop, gmp$pcgmp, log = "x", xlab = "Population",
     ylab = "Per-capita Economic Output")

# beta_0 = 6611; beta_1 = 1/8
curve(6611*x^{1/8}, add = TRUE, col = "blue")


max.iter <- 100 # How long we run the alg.
stop.deriv <- 1/100 # If derivative is small, stop
deriv.step <- 1/1000 # This is h
step.scale <- 1e-12 # This is c
iter <- 0 # Iteration counter
deriv <- Inf
beta <- 0.15
while((iter < max.iter) & (deriv > stop.deriv)) {
  iter <- iter + 1
  mse.1 <- mean((gmp$pcgmp - 6611*gmp$pop^beta)^2)
  mse.2 <- mean((gmp$pcgmp - 6611*gmp$pop^(beta + deriv.step))^2)
  deriv <- (mse.2 - mse.1)/deriv.step
  beta <- beta - step.scale*deriv
}
list(beta = beta, iteration = iter, conv = (iter < max.iter))

est.exp <- function(beta) {
  max.iter <- 100 # How long we run the alg.
  stop.deriv <- 1/100 # If derivative is small, stop
  deriv.step <- 1/1000 # This is h
  step.scale <- 1e-12 # This is c
  iter <- 0
  deriv <- Inf
  while((iter < max.iter) & (abs(deriv) > stop.deriv)) {
    iter <- iter + 1
    mse.1 <- mean((gmp$pcgmp - 6611*gmp$pop^beta)^2)
    mse.2 <- mean((gmp$pcgmp - 6611*gmp$pop^(beta + deriv.step))^2)
    deriv <- (mse.2 - mse.1)/deriv.step
    beta <- beta - step.scale*deriv
  }
  fit <- list(beta = beta, iteration = iter, conv = (iter < max.iter))
  return(fit)
}



est.exp <- function(beta, beta_0 = 6611, max.iter = 100,
                    stop.deriv = .01, deriv.step = .001,
                    step.scale = 1e-12) {
  iter <- 0
  deriv <- Inf
  while((iter < max.iter) & (abs(deriv) > stop.deriv)) {
    iter <- iter + 1
    mse.1 <- mean((gmp$pcgmp - beta_0*gmp$pop^beta)^2)
    mse.2 <- mean((gmp$pcgmp - beta_0*gmp$pop^(beta + deriv.step))^2)
    deriv <- (mse.2 - mse.1)/deriv.step
    beta <- beta - step.scale*deriv
  }
  fit <- list(beta = beta, iteration = iter, conv = (iter < max.iter))
  return(fit)
}



est.exp <- function(beta, beta_0 = 6611, response = gmp$pcgmp,
                    predictor = gmp$pop, max.iter = 100,
                    stop.deriv = .01, deriv.step = .001,
                    step.scale = 1e-12) {
  iter <- 0
  deriv <- Inf
  mse <- function(b) {mean((response - beta_0*predictor^b)^2)}
  for (i in 1:max.iter) {
    iter <- iter + 1
    deriv <- (mse(beta + deriv.step) - mse(beta))/deriv.step
    beta <- beta - step.scale*deriv
    if (abs(deriv) < stop.deriv) {break()}
  }
  fit <- list(beta = beta, iteration = iter, conv = (iter < max.iter))
  return(fit)
}

#####classifier###############
install.packages("ISLR")
library(ISLR)
head(Smarket, 3)


mean(Smarket$Lag1[Smarket$Direction == "Up"])
mean(Smarket$Lag1[Smarket$Direction == "Down"])

plot(Smarket$Lag1, Smarket$Lag2, col = Smarket$Direction,
     xlab="Lag1", ylab="Lag2", main="Today's Direction")
legend("bottomright", legend = levels(Smarket$Direction),
       col=1:length(levels(Smarket$Direction)), pch=1)



K <- 5
L1.new <- 2
L2.new <- 4.25
# K = 5 and new point (2, 4.25).
dists <- sqrt((Smarket$Lag1 - L1.new)^2
              + (Smarket$Lag2 - L2.new)^2)
neighbors <- order(dists)[1:K]
neighb.dir <- Smarket$Direction[neighbors]
choice <- names(which.max(table(neighb.dir)))
choice


test <- Smarket[Smarket$Year == 2005, ]
train <- Smarket[Smarket$Year != 2005, ]
n.test <- nrow(test)
predictions <- rep(NA, n.test)
for (i in 1:n.test){
  predictions[i] <- KNNclass(test$Lag1[i], test$Lag2[i],
                             L1 = train$Lag1, L2 = train$Lag2,
                             Dir = train$Direction)
}
test.error <- mean(predictions != test$Direction)
test.error



