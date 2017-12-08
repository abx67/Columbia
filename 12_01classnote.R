####12.01.2017####
strikes <- read.csv("strikes.csv", as.is = TRUE)
dim(strikes)
head(strikes, 3)

italy.strikes <- subset(strikes, country == "Italy")
# Equivalently,
italy.strikes <- strikes[strikes$country == "Italy", ]
dim(italy.strikes)
head(italy.strikes, 5)
italy.fit <- lm(strike.volume ~ left.parliament,
                data = italy.strikes)
plot(strike.volume ~ left.parliament, data = italy.strikes,
     main = "Italy Strike Volume Versus Leftwing Alignment",
     ylab = "Strike volume", xlab = "Leftwing Alignment")
abline(italy.fit, col = 2)

my.strike.lm <- function(country.df) {
  return(lm(strike.volume ~ left.parliament,
            data = country.df)$coeff)
}

my.strike.lm(subset(strikes, country == "Italy"))

strike.coef <- NULL
countries <- unique(strikes$country)
for (this.country in countries) {
  country.dat <- subset(strikes, country == this.country)
  new.coefs <- my.strike.lm(country.dat)
  strike.coef <- cbind(strike.coef, new.coefs)
}
colnames(strike.coef) <- countries
strike.coef

strikes.split <- split(strikes, strikes$country)
names(strikes.split)

strike.coef <- sapply(strikes.split[1:12], my.strike.lm)
strike.coef

plot(1:ncol(strike.coef), strike.coef[2, ], xaxt = "n",
     xlab = "", ylab = "Regression coefficient",
     main="Countrywise labor activity by leftwing score")
axis(side = 1, at = 1:ncol(strike.coef),
     labels = colnames(strike.coef), las = 2,
     cex.axis = 0.5)
abline(h = 0, col = "grey")

my.array <- array(1:27, c(3,3,3))
rownames(my.array) <- c("R1", "R2", "R3")
colnames(my.array) <- c("C1", "C2", "C3")
dimnames(my.array)[[3]] <- c("Bart", "Lisa", "Maggie")
my.array
my.array[, , 3]
library(plyr)
aaply(my.array, 1, sum) # Get back an array
adply(my.array, 1, sum) # Get back a data frame
alply(my.array, 1, sum) # Get back a list
aaply(my.array, 2:3, sum) # Get back a 3 x 3 array
adply(my.array, 2:3, sum) # Get back a data frame
alply(my.array, 2:3, sum) # Get back a list

my.list <- list(nums = rnorm(1000), lets = letters,
                pops = state.x77[ ,"Population"])
head(my.list[[1]], 5)
head(my.list[[2]], 5)
head(my.list[[3]], 5)
laply(my.list, range) # Get back an array
ldply(my.list, range) # Get back a data frame
llply(my.list, range) # Get back a list
# Doesn't work! Outputs have different types/lengths
# laply(my.list, summary)
# ldply(my.list, summary)
llply(my.list, summary) # Works just fine

par(mfrow = c(3, 3), mar = c(4, 4, 1, 1))
a_ply(my.array, 2:3, plot, ylim = range(my.array),
      pch = 19, col = 6)
?a_ply
