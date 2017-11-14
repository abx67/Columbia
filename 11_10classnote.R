####11.10.2017####
library(MASS)
library(ggplot2)
head(cats)

hist(cats$Hwt)
quantile(cats$Hwt, c(0.25, 0.5, 0.75))
plot(ecdf(cats$Hwt),
     main = "Empirical CDF of Cat Heart Weights")
hist(cats$Hwt, probability = TRUE, ylim = c(0, 0.17))
lines(density(cats$Hwt), lty = "dashed")
ggplot(cats) +
  geom_histogram(aes(x = Hwt, y = ..density..)) +
  geom_density(aes(x = Hwt))


### esitmation example
samp_size <- 100
samp1 <- rnorm(samp_size, mean = 10, sd = 3)
mmean_est1 <- mean(samp1)
n <- 500
samp_means <- rep(NA, n)
for (i in 1:n) {
  samp_means[i] <- mean(rnorm(samp_size, mean = 10, sd = 3))
}
ggplot(data.frame(samp = samp_means)) +
  geom_histogram(aes(x = samp, y = ..density..)) +
  geom_density(aes(x = samp)) 



# Model quantiles
qgamma(c(0.01, 0.05, 0.95, 0.99), shape = cat.MM["a"],
       scale = cat.MM["s"])
# Data quantiles:
quantile(cats$Hwt, c(0.01, 0.05, 0.95, 0.99))
a <- cat.MM["a"]
s <- cat.MM["s"]
qqplot(cats$Hwt, qgamma((1:99)/100, shape = a, scale = s),
       ylab = "Theoretical Quantiles")
abline(0, 1, col = "red")




