# Class Script, Lecture 8
# October 27


# Envelope Rejction Algorith
# Target Density is Beta(4,3)

f <- function(x) {
  stopifnot(x >= 0 & x <=1)
  return(60*x^3*(1-x)^2)
}

x <- seq(0, 1, length = 100)
plot(x, f(x), type = "l", ylab = "f(x)", col = "red")

max   <- 0.6
f.max <- f(max)

lines(c(0, 0), c(0, f.max), lty = 1)
lines(c(0, 1), c(f.max, f.max), lty = 1)
lines(c(1, 1), c(f.max, 0), lty = 1)

# Envelope Algorithm

n.samps <- 1000 # Number of samples desired
n       <- 0    # Counter for the number of samples
samps   <- numeric(n.samps) # initialize vector to hold output

while(n < n.samps) {
  x <- runif(1) # proposal density g is Unif(0,1)
  u <- runif(1) # uniform sample
  if (f.max*u < f(x)) {
    n        <- n + 1
    samps[n] <- x
  }
}

x <- seq(0, 1, length = 100)
hist(samps, prob = TRUE, ylab = "f(x)", xlab = "x")
lines(x, dbeta(x, 4, 3), lty = 2)



# Monte Carlo Integration

n     <- 1000000
norms <- rnorm(n, mean = 0, sd = 1/sqrt(2))
est   <- mean(sin(norms)^2) 
est2  <- mean(norms <= 1.36)
est2
pnorm(1.36, sd = 1/sqrt(2))

norms <- rnorm(n)
est   <- sqrt(2*pi)*mean(sin(norms)^2*exp(-(1/2)*norms^2))
est

norms <- rnorm(n, mean = 0, sd = 1/sqrt(2))
est3   <- sqrt(pi)*mean(sin(norms)^2) 
est3


# E[sin(X)^2] where X ~ N(0, 1/sqrt(2))

n1 <- 10000; n2 <- 1000
estvec1 <- rep(NA, 1000); estvec2 <- rep(NA, 1000)

for (i in 1:1000) {
  norms1 <- rnorm(n1, sd = 1/sqrt(2))
  norms2 <- rnorm(n2, sd = 1/sqrt(2))
  
  estvec1[i] <- mean(sin(norms1)^2)
  estvec2[i] <- mean(sin(norms2)^2)
}

df <- data.frame(estimates = c(estvec1, estvec2),
                 n = c(rep(n1, 1000), rep(n2, 1000)))
ggplot(df) + geom_histogram(aes(x = estimates)) + facet_wrap(~n, ncol = 2)


# Check Yourself

n     <- 1000000
samps <- rexp(n, rate = 1/3)
mean(samps < 3)
pexp(3, rate = 1/3)



# Simulating Dart Throws

board <- list(
  R1 = 6.35, R2 = 15.9, R3 = 99, R4 = 107, R5 = 162, R = 170, 
  nums = c(20, 1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5)
)

# Normal Model
throws  <- 100
std.dev <- 50

x <- rnorm(throws, sd = std.dev)
y <- rnorm(throws, sd = std.dev)

drawBoard(board)
points(x, y, pch = 20, col = "red")

scores <- scorePositions(x, y, board)
text(x, y+8, scores, cex = .75)

# Uniform Model

R <- 170

x <- runif(throws, min = -R, max = R)
y <- runif(throws, min = -R, max = R)

drawBoard(board)
points(x, y, pch = 20, col = "red")

scores <- scorePositions(x, y, board)
text(x, y+8, scores, cex = .75)

# Comparing Models
throws <- 10000

x1 <- rnorm(throws, sd = std.dev)
y1 <- rnorm(throws, sd = std.dev)
x2 <- runif(throws, min = -R, max = R)
y2 <- runif(throws, min = -R, max = R)

scores1 <- scorePositions(x1, y1, board)
scores2 <- scorePositions(x2, y2, board)
mean(scores1)
mean(scores2)


drawBoard(board)
points(x1, y1, pch = 20, col = "red", cex = .05)
points(x2, y2, pch = 20, col = "blue", cex = .05)

# Comparing SD values

sd.vals    <- seq(5, 150, by = 5)
n          <- length(sd.vals)
avg.scores <- rep(NA, length = n)
names(avg.scores) <- sd.vals

for (i in 1:n) {
  x <- rnorm(throws, sd = sd.vals[i])
  y <- rnorm(throws, sd = sd.vals[i])
  
  scores <- scorePositions(x, y, board)
  avg.scores[i] <- mean(scores)
}

plot(sd.vals, avg.scores, xlab = "Standard Deviation", ylab = "Avg. Score")
abline(mean(scores2), 0, col = "red")

# Check Yourself

drawBoard(board)
points(c(0, -32), c(103, -98), col = c("red", "blue"), pch = 20, cex = 3)

normal.score <- function(mean.x, mean.y, sd, board) {
  x1 <- rnorm(throws, mean.x, sd)
  y1 <- rnorm(throws, mean.y, sd)
  return(mean(scorePositions(x1, y1, board)))
}

std.dev <- 35
normal.score(0, 0, std.dev, board)
normal.score(0, 103, std.dev, board)
normal.score(-32, -98, std.dev, board)

throws <- 200
mean.x1 <- 0; mean.y1 <- 103
mean.x2 <- -32; mean.y2 <- -98
x1 <- rnorm(throws, mean.x1, std.dev)
y1 <- rnorm(throws, mean.y1, std.dev)

x2 <- rnorm(throws, mean.x2, std.dev)
y2 <- rnorm(throws, mean.y2, std.dev)

drawBoard(board)
points(x1, y1, pch = 20, col = "red")
points(x2, y2, pch = 20, col = "blue")

scores1 <- scorePositions(x1, y1, board)
scores2 <- scorePositions(x2, y2, board)
text(x1, y1+8, scores1, cex = .75)
text(x2, y2+8, scores2, cex = .75)
