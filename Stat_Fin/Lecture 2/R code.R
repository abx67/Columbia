dat = read.csv("Stock_Bond.csv", header = T)
price = cbind(dat$GM_AC, dat$F_AC, dat$UTX_AC, 
         dat$CAT_AC, dat$MRK_AC, dat$PFE_AC, dat$IBM_AC, 
         dat$MSFT_AC, dat$C_AC, dat$XOM_AC)[4463:4963,]

n = dim(price)[1]
r = log(price[2:n,]/price[1:(n-1),])   #log-returns
hist(r[,1],breaks=30,main="Histogram of GM Log Return",
     xlab="GM Log Return")   #plot GM log-return

mean_r = colMeans(r)   #mean of each log-return
sd_r = sqrt(diag(cov(r)))   #sd of each log-return
cor_r = cor(r)   #correlation of log-returns

# par(mfrow=c(1,2))
# plot(dat$GM_AC,type="l")   #plot GM price
# title("GM Stock Price")
# plot(dat$MSFT_AC,type="l")   #plot MSFT price
# title("MSFT Stock Price")
# par(mfrow=c(1,1))

w = exp(r[,c(1,2)])/rowSums(exp(r[,c(1,2)]))   #weight
pr = rowSums(r[,c(1,2)]*w)   #weighted portfolio return
plot(100*exp(cumsum(pr)),type="l",col="black",xlab="",ylab="",ylim=c(40,110))   #GM+Ford portfolio value
lines(c(1:500),100*exp(cumsum(r[,1])),type="l",col="red",xlab="",ylab="")   #GM portfolio value
lines(c(1:500),100*exp(cumsum(r[,2])),type="l",col="blue",xlab="",ylab="")   #Ford portfolio value
title("Portfolio Values")
legend("bottomleft",legend=c("GM only","Ford only","GM+Ford"),col=c("red","blue","black"),lty=1:1)

plot(density(pr),main="Log Return Distribution",
     xlab="Log Return",xlim=c(-.1,.1),ylim=c(0,40))   #plot GM+Ford log-return
lines(density(r[,1]),col="red")   #plot GM log-return
lines(density(r[,5]),col="blue")   #plot Ford log-return
legend("topright",legend=c("GM only","Ford only","GM+Ford"),col=c("red","blue","black"),lty=1:1)


w = exp(r[,c(1,5)])/rowSums(exp(r[,c(1,5)]))   #weight
pr = rowSums(r[,c(1,5)]*w)   #weighted portfolio return
plot(100*exp(cumsum(pr)),type="l",col="black",xlab="",ylab="",ylim=c(30,110))   #GM+Merck portfolio value
lines(c(1:500),100*exp(cumsum(r[,1])),type="l",col="red",xlab="",ylab="")   #GM portfolio value
lines(c(1:500),100*exp(cumsum(r[,5])),type="l",col="blue",xlab="",ylab="")   #Merck portfolio value
title("Portfolio Values")
legend("bottomleft",legend=c("GM only","Merck only","GM+Merck"),col=c("red","blue","black"),lty=1:1)

hist(pr,breaks=50,main="Histogram of GM+Merck Log Return",
   xlab="GM+Merck Log Return")   #plot GM+Merck log-return
hist(r[,1],breaks=50,main="Histogram of GM Log Return",
     xlab="GM Log Return")   #plot GM log-return
hist(r[,5],breaks=50,main="Histogram of Merck Log Return",
     xlab="Merck Log Return",xlim=c(-.1,.1))   #plot Merck log-return

plot(density(pr),main="Log Return Distribution",
   xlab="Log Return",xlim=c(-.08,.08),ylim=c(0,40))   #plot GM+Merck log-return
lines(density(r[,1]),col="red")   #plot GM log-return
lines(density(r[,5]),col="blue")   #plot Merck log-return
legend("topright",legend=c("GM only","Merck only","GM+Merck"),col=c("red","blue","black"),lty=1:1)
