# Class Script, Lecture 11
# November 17

setwd("D:/R project/intro_to_ds/Week11")

# Selective Access

data(cats, package = "MASS")
head(cats)

# Only want, heart weights of male cats
head(cats$Hwt[cats$Sex == "M"])
head(cats$Hwt[cats$Sex != "M"])

head(cats[cats$Sex == "M", "Hwt"])

# Selective access using indexing

cats.subset <- sample(1:nrow(cats), size = nrow(cats)/2)
new.cats    <- cats[cats.subset, ]
head(new.cats)
dim(new.cats)

# A new way to do this.
boy.cats   <- cats[cats$Sex == "M", ]
boy.cats.1 <- subset(cats, Sex == "M")

# States data, 50 states with 9 variables
states <- data.frame(state.x77, Region = state.region)
unique(states$Region)

# Looking for states data from states in the south
states$Income["South"] # Doesn't do what we want

income.south <- c()
for (i in 1:50) {
  if (states$Region[i] == "South") {
    income.south <- c(income.south, states$Income[i])
  }
}

# The above is a bad way to get what we want
income.south.1 <- states$Income[states$Region == "South"]

# Check Yourself

sum(states$Frost >= 150)
row.names(states)[states$Frost >= 150]

avgs <- colSums(states[, 1:8])/nrow(states)
colSums(states[, 1:8] > rep(avgs, each = nrow(states)))

mat1 <- states[, 1:8] > matrix(rep(avgs, each = nrow(states)), ncol = 8)
mat2 <- states[, 1:8] > avgs #rep(avgs, 8*nrow(states))

# Apply Functions

# Max. entry in each column
max(states$Frost)
apply(states[, 1:8], 2, max)
row.names(states)[apply(states[, 1:8], 2, which.max)]

summary(states$Frost)
apply(states[, 1:8], 2, summary)


# Writing our own function
frow <- function(r) {
  val <- as.numeric(r[7])
  return(ifelse(r[9] == "Northeast", 0.5*val, val))
}

frost.fake <- apply(states, 1, frow)
mean(states$Frost[states$Region == "Northeast"])
mean(frost.fake[states$Region == "Northeast"])

top.3.names <- function(v, names.v) {
  names.v[order(v, decreasing = TRUE)[1:3]]
}

apply(states[, 1:8], 2, top.3.names, names.v = rownames(states))

# Check Yourself

cor.v1.v2 <- function(v1, v2 = states[, "Frost"]) {
  return(cor(v1, v2))
}

cor.v1.v2(states$Life.Exp)
cor.v1.v2(states$Frost)
apply(states[, 1:8], 2, cor.v1.v2)
apply(states[, 1:8], 2, cor, states[, "Frost"])

# lapply, sapply

# Jackknife (leave-one-out mean)
mean.less.one <- function(i, vec) {
  return(mean(vec[-i]))
}

my.vec <- states$Frost
n      <- length(my.vec)

my.vec.jack <- rep(NA, n)
for (i in 1:n) {
  my.vec.jack[i] <- mean.less.one(i, my.vec)
}

my.vec.jack.2 <- lapply(1:n, mean.less.one, my.vec)
my.vec.jack.3 <- sapply(1:n, mean.less.one, my.vec)

# tapply -- Let's avg the frost variable in each region

mean(states$Frost[states$Region == "Northeast"])
tapply(states$Frost, states$Region, mean)

# mapply

rep(1, 4)
mapply(rep, 1:4, 4:1)
mapply(rep, c(1, 2, 3, 4), c(4, 3, 2, 1))



# Re-ordering data

cats$Hwt[order(cats$Hwt)]

hwt.order  <- order(cats$Hwt)
cats.order <- cats[hwt.order, ]


this.vec <- c(25, 13, 25, 77, 68)
order(this.vec)
this.vec[order(this.vec)]
sort(this.vec)
rank(this.vec)

# Merge Example

fha <- read.csv("fha.csv", na.string = "NA", 
                colClasses = c("character", rep("double", 3)))
nrow(fha)
head(fha)
colnames(fha)

ua <- read.csv("ua.txt", sep = ";")
nrow(ua)
head(ua)

dim(fha) # 498 rows
length(unique(fha$Population))

ua.pop.top498 <- sort(ua$POP, decreasing = TRUE)[1:nrow(fha)]
head(ua.pop.top498)
head(fha$Population)

# Option 1: Reorder area in ua by population, append to fha
ua.sort <- ua[order(ua$POP, decreasing = TRUE), ]
area    <- ua.sort$AREALANDSQMI[1:nrow(fha)]
df1     <- data.frame(fha, area)
colnames(df1) <- c("City", "Population", "Roads", "Mileage", "Area")
nrow(df1)

# Option 2: merge()
df2 <- merge(x = fha, y = ua, by.x = "Population", by.y = "POP")
head(df2)
tail(df2)

# Merge on city names
df2.1 <- merge(x = fha, y = ua, by.x = "City", by.y = "NAME")
df2.2 <- merge(x = fha, y = ua, by.x = "City", by.y = "NAME", all.x = TRUE)

df2.2$City[is.na(df2.2$POP)]

#Check Yourself

exp(colMeans(log(states[, 1:8])))

geom.mean <- function(vec) {
  return((prod(vec))^(1/n))
}

apply(states[, 1:8], 2, geom.mean)

# split() function -- split states df according to region
states.by.reg <- split(states, states$Region)
class(states.by.reg)
length(states.by.reg)
names(states.by.reg)
class(states.by.reg[[1]])

# For each, display first two rows

head(states.by.reg[[1]], 2)
lapply(states.by.reg, head, 2)

# For each region, average the eight numeric variables


mean.func <- function(df) {
  apply(df[, 1:8], 2, mean)
}
  
lapply(states.by.reg, mean.func)

# Aggregate

aggregate(states[, 1:8], list(states$Region), mean)

