# Class Script, Lecture 10
# November 10



#install.packages("MASS")
library(MASS)
head(cats)

hist(cats$Hwt)
quantile(cats$Hwt, c(0.25, 0.5, 0.75))

plot(ecdf(cats$Hwt))


hist(cats$Hwt, probability = TRUE, ylim = c(0, 0.17))
lines(density(cats$Hwt), lty = "dashed")

library(ggplot2)
ggplot(cats) +
  geom_histogram(aes(x = Hwt, y = ..density..)) +
  geom_density(aes(x = Hwt))


# Estimation Example
samp_size <- 100
samp1     <- rnorm(samp_size, mean = 10, sd = 3)
mean_est1 <- mean(samp1)

n          <- 500
samp_means <- rep(NA, n)

for (i in 1:n) {
  samp_means[i] <- mean(rnorm(samp_size, mean = 10, sd = 3))
}

ggplot(data.frame(samp = samp_means)) +
  geom_histogram(aes(x = samp, y = ..density..)) +
  geom_density(aes(x = samp))


# Sample Distribution of the Sample Mean

samp_size  <- c(5, 10, 25, 50, 100)
n          <- 500 
samp_means <- cbind(rep(NA, n*length(samp_size)),
                    rep(NA, n*length(samp_size)))


for (j in 1:length(samp_size)) {
  for (i in 1:n) {
    row <- (j-1)*n + i
    samp_means[row, 1] <- mean(rnorm(samp_size[j], mean = 10, sd = 3))
    samp_means[row, 2] <- samp_size[j]
  }
}

colnames(samp_means) <- c("Values", "SampleSize")

ggplot(data.frame(samp_means)) +
  geom_histogram(aes(x = Values, y = ..density..)) +
  geom_density(aes(x = Values)) +
  facet_wrap(~SampleSize)


# Sample Distribution of the Sample Variance


samp_size  <- c(5, 10, 25, 50, 100)
n          <- 500 
samp_means <- cbind(rep(NA, n*length(samp_size)),
                    rep(NA, n*length(samp_size)))
colnames(samp_means) <- c("Values", "SampleSize")


for (j in 1:length(samp_size)) {
  for (i in 1:n) {
    row <- (j-1)*n + i
    samp_means[row, 1] <- var(rnorm(samp_size[j], mean = 10, sd = 3))
    samp_means[row, 2] <- samp_size[j]
  }
}

ggplot(data.frame(samp_means)) +
  geom_histogram(aes(x = Values, y = ..density..)) +
  geom_density(aes(x = Values)) +
  facet_wrap(~SampleSize)



# Method of Moments

# a = \mu_1^2/(\mu_2 - \mu_1^2)
# s = (\mu_2 - \mu_1^2)/ \mu_1
# v = \mu_2 - \mu_1^2

gam.MMest <- function(data) {
  #estimates \hat{a} and \hat{s} that best fit the data
  m <- mean(data)
  v <- var(data)
  return(c(a = m^2/v, s = v/m))
}

cat.MM   <- gam.MMest(cats$Hwt)
gam_args <- list(shape = cat.MM[1], scale = cat.MM[2])

ggplot(cats) +
  geom_histogram(aes(x = Hwt, y = ..density..)) +
  geom_density(aes(x = Hwt), linetype = "dashed") +
  stat_function(aes(x = Hwt), fun = dgamma, args = gam_args, color = "red")

gam.mean <- function(a, s) {a*s}
gam.var  <- function(a, s) {a*s^2}

gam.diff <- function(params, data) {
  a <- params[1]
  s <- params[2]
  return((mean(data) - gam.mean(a,s))^2 + (var(data) - gam.var(a,s))^2)
}
(mean(cats$Hwt) / var(cats$Hwt))^2
nlm(gam.diff, c(19, 1), data = cats$Hwt)

cat.MM
nlm(gam.diff, c(19, 1), data = cats$Hwt)$estimate


# Checking Your Estimator

gam.MMest(rgamma(100, shape = 19, scale = 45))
gam.MMest(rgamma(10000, shape = 19, scale = 45))
gam.MMest(rgamma(1000000, shape = 19, scale = 45))


# Maximum Likelihood Estimator

gam.ll <- function(params, data) {
  # Input: parameter vector (length2, shape and scale)
  # Input: data
  a <- params[1]
  s <- params[2]
  return(sum(dgamma(data, shape = a, scale = s, log = TRUE)))
}

nlm(gam.ll, c(19, 1), data = cats$Hwt)

neg.gam.ll <- function(params, data) {
  # Input: parameter vector (length2, shape and scale)
  # Input: data
  a <- params[1]
  s <- params[2]
  return(-sum(dgamma(data, shape = a, scale = s, log = TRUE)))
}

nlm(neg.gam.ll, c(19, 1), data = cats$Hwt)

ll_min <- nlm(neg.gam.ll, c(19, 1), data = cats$Hwt)$minimum

cat.MM
mm_min <- neg.gam.ll(cat.MM, cats$Hwt)


cat.MLE <- nlm(neg.gam.ll, c(19, 1), data = cats$Hwt)$estimate

MLE_args <- list(shape = cat.MLE[1], scale = cat.MLE[2])
MM_args <- list(shape = cat.MM[1], scale = cat.MM[2])

ggplot(cats) +
  geom_histogram(aes(x = Hwt, y = ..density..)) +
  geom_density(aes(x = Hwt), linetype = "dashed") +
  stat_function(aes(x = Hwt), fun = dgamma, args = MM_args, color = "red") +
  stat_function(aes(x = Hwt), fun = dgamma, args = MLE_args, color = "blue")


# Q-Q Plot

# Model (theoretical) quantiles
qgamma(c(0.01, 0.05, 0.95, 0.99), shape = cat.MM[1], scale = cat.MM[2])
# Empirical (data) quantiles
quantile(cats$Hwt, c(0.01, 0.05, 0.95, 0.99))

a <- cat.MM[1]
s <- cat.MM[2]
theory_quant <- qgamma((1:99)/100, shape = a, scale = s)
qqplot(cats$Hwt, theory_quant)
abline(0, 1, col = "red")

# Bootstrap

cat.MM <- gam.MMest(cats$Hwt)
cat.MM

# A single bootstrap resample

n      <- nrow(cats)
resamp <- sample(1:n, n, replace = TRUE)

boot.samp <- cats$Hwt[resamp]

head(sort(boot.samp))
head(sort(cats$Hwt))

gam.MMest(boot.samp)

# 1000 bootstrap resamples

B          <- 1000
param_ests <- matrix(NA, nrow = B, ncol = 2)
colnames(param_ests) <- c("a", "s")

for (b in 1:B) {
  resamp         <- sample(1:n, n, replace = TRUE)
  boot.samp      <- cats$Hwt[resamp]
  param_ests[b,] <- gam.MMest(boot.samp)
}

param_ests <- data.frame(param_ests)

ggplot(param_ests) +
  geom_histogram(aes(x = a)) +
  geom_vline(xintercept = mean(a), col = "red")

ggplot(param_ests) +
  geom_histogram(aes(x = s)) +
  geom_vline(xintercept = mean(s), col = "red")

bootstrap.a <- mean(param_ests$a)
bootstrap.s <- mean(param_ests$s)
bootstrap.a

# Use bootstrap samples to estimate the standard error of our estimates

sd(param_ests$a)
sd(param_ests$s)

# CI

CI.a <- quantile(param_ests$a, probs = c(0.025, 0.975))


ggplot(param_ests) +
  geom_histogram(aes(x = a)) +
  geom_vline(xintercept = mean(a), col = "red") +
  geom_vline(xintercept = CI.a[1], col = "red", linetype = "dashed") +
  geom_vline(xintercept = CI.a[2], col = "red", linetype = "dashed")



# Convince ourselves that hte bootstrap works.
set.seed(1)
samp_size <- 100
samp1     <- rnorm(samp_size, mean = 10, sd = 3)
mean_est1 <- mean(samp1)


# Calulate bootstrap resamples
n <- length(samp1)
B <- 1000
boot.means <- rep(NA, B)

for (b in 1:B) {
  resamp_data   <- sample(samp1, n, replace = TRUE)
  boot.means[b] <- mean(resamp_data)
}
mean(boot.means)
sd(boot.means) # Close to 3/10?

ggplot() + geom_histogram(aes(x = boot.means))


