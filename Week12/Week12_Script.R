# Class Script, Lecture 12
# December 1

setwd("D:/R project/intro_to_ds/Week12")
strikes <- read.csv("strikes.csv", as.is = TRUE)

# Check Yourself
strikes.split <- split(strikes, strikes$country)
names(strikes.split)
length(strikes.split)

sapply(strikes.split, dim)

mean.func <- function(df) {
  return(mean(df$unemployment))
}
mean.unemploy <- sapply(strikes.split, mean.func)

mean.unemploy[which.max(mean.unemploy)]
mean.unemploy[which.min(mean.unemploy)]

# A single country
italy.strikes <- strikes[strikes$country == "Italy", ]

italy.fit <- lm(strike.volume ~ left.parliament, data = italy.strikes)
plot(strike.volume ~ left.parliament, data = italy.strikes,
     main = "Italy strike volume vs. leftwing alignment",
     ylab = "Strike volume", xlab = "Leftwing alignment")
abline(italy.fit, col = 2)

my.strike.lm <- function(country.df) {
  return(lm(strike.volume ~ left.parliament, data = country.df)$coeff)
}
my.strike.lm(italy.strikes)

strike.coeff <- NULL
countries    <- unique(strikes$country)
for (this.country in countries) {
  country.dat  <- strikes[strikes$country == this.country, ]
  new.coeffs   <- my.strike.lm(country.dat)
  strike.coeff <- cbind(strike.coeff, new.coeffs)
}
colnames(strike.coeff) <- countries

strikes.split <- split(strikes, strikes$country)
strike.coef   <- sapply(strikes.split, my.strike.lm)
strike.coef



# Check Yourself
years.split <- split(strikes, strikes$year)
three.mean  <- function(df) {
  return(apply(df[, c("unemployment", "inflation", "strike.volume")], 2, mean))
}
years.mat <- sapply(years.split, three.mean)
years.mat

max.rate <- max(years.mat[1:2, ])
min.rate <- min(years.mat[1:2, ])
plot(colnames(years.mat), years.mat[1, ], xlab = "Year", ylab = "Rate", 
     type = "l", ylim = c(min.rate, max.rate))
points(colnames(years.mat), years.mat[2, ], type = "l", col = "red")
legend("topleft", c("Unemployment", "Inflation"), fill = c("black", "red"), 
       cex = .5)

# a*ply()
my.array <- array(1:27, c(3, 3, 3))
rownames(my.array) <- c("R1", "R2", "R3")
colnames(my.array) <- c("C1", "C2", "C3")
dimnames(my.array)[[3]] <- c("bart", "lisa", "maggie")

library(plyr)
aaply(my.array, 1, sum)
adply(my.array, 1, sum)
alply(my.array, 1, sum)

aaply(my.array, 2:3, sum)
adply(my.array, 2:3, sum)
alply(my.array, 2:3, sum)

my.list <- list(nums = rnorm(1000), lets = letters, 
                pops = state.x77[, "Population"])

head(my.list[[1]], 5)
head(my.list[[2]], 5)
head(my.list[[3]], 5)

laply(my.list, range)
ldply(my.list, range)
llply(my.list, range)

# laply(my.list, summary)
# ldply(my.list, summary)
llply(my.list, summary)

par(mfrow = c(3,3), mar = c(4, 4, 1, 1))
a_ply(my.array, 2:3, plot, ylim = range(my.array), 
      pch = 19, col = 6)
aaply(my.array, 2:3, sum)


# D_ply

my.strike.lm <- function(country.df) {
  return(lm(strike.volume ~ left.parliament, data = country.df)$coeff)
}

strikes.list  <- split(strikes, strikes$country)
strikes.coefs <- sapply(strikes.list, my.strike.lm)
strikes.coefs

strike.coef.a <- daply(strikes, .(country), my.strike.lm)
strike.coef.d <- ddply(strikes, .(country), my.strike.lm)
strike.coef.l <- dlply(strikes, .(country), my.strike.lm)

# Splitting on multiple variables.

strikes$yearPre1975 <- strikes$year <= 1975

strike.coef.75 <- daply(strikes, .(country, yearPre1975), my.strike.lm)
strike.coef.75 <- ddply(strikes, .(country, yearPre1975), my.strike.lm)

strike.coef.75 <- ddply(strikes, .(country, I(year <= 1975)), my.strike.lm)

# Check Yourself

inflation.mean <- function(df) {
  return(mean(df$inflation))
}
inflation75 <- ddply(strikes, .(country, I(year <= 1975)), inflation.mean)
dim(inflation75)
head(inflation75)

split.list <- list(strikes$country, I(strikes$year <= 1975))
data.split <- split(strikes, split.list)
inflation75_v2 <- sapply(data.split, inflation.mean)
length(inflation75_v2)
head(inflation75_v2)

# Dplyr

library(dplyr)
ycs <- select(strikes, year, country, strike.volume)
head(ycs)

ycs <- strikes[, c("year", "country", "strike.volume")]

ycs <- strikes %>% select(year, country, strike.volume)

# Maybe we're only interested in strikes when unemployment > 4%
ycs_unemploy <- strikes %>% 
                filter(unemployment > 4) %>% 
                select(year, country, strike.volume)

# group_by acts like split
# Most useful when combined with summarize

strikes.split <- strikes %>% group_by(country)
country.strikes <- strikes %>% 
                   group_by(country) %>%
                   summarize(avg.strikes = mean(strike.volume))


# Reshaping Data
snoq <- read.csv("snoqualmie.csv", header = FALSE, as.is = TRUE)
colnames(snoq) <- 1:366
snoq$year <- 1948:1983

library(reshape2)
snoq.melt <- melt(snoq, id.vars = "year", variable.name = "day", 
                  value.name = "precip") 
head(snoq.melt)
snoq.melt.chron <- snoq.melt[order(snoq.melt$year, snoq.melt$day), ]

leap.days <- snoq.melt.chron$day == 366
sum(is.na(snoq.melt.chron$precip[leap.days]))

snoq.melt.chron <- na.omit(snoq.melt.chron)
