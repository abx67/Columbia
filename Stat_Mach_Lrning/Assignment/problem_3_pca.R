# download the data
library(quantmod)
setwd("/Users/linxiliu/Dropbox/Teaching/Statistical_Machine_Learning_Spring2018/Assignment/solutions/assignment_1")
DJ30_smbl_list = c("AAPL", "AXP", "BA", "CAT", "CSCO", "CVX", "DWDP", "DIS", "GE",
                   "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM",
                   "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "V",
                   "VZ", "WMT", "XOM")
# use a for loop to download closing price to X
X <- c()
for(j in DJ30_smbl_list){
  X<- cbind(X, getSymbols(j, auto.assign = F, from = "2017-01-01", to = "2018-01-01")[,4])
}
colnames(X) = DJ30_smbl_list
n = dim(X)[1]
p = dim(X)[2]

## cor = F
pcaX = princomp(X, cor=F)
V = pcaX$loadings
center = pcaX$center
scale = pcaX$scale

# Plot
# biplot
pdf(width=8, height=8, file= "P3figure_corF_biplot.pdf")
biplot(pcaX, cex=0.6)
dev.off()

# screeplot
pdf(width=5, height=5, file="P3figure_corF_screeplot.pdf")
screeplot(pcaX)
dev.off()

## cor = T
pcaX = princomp(X, cor=T)
V = pcaX$loadings
center = pcaX$center
scale = pcaX$scale

# Plot
# biplot
pdf(width=8, height=8, file="P3figure_corT_biplot.pdf")
biplot(pcaX, cex=0.6)
dev.off()

# screeplot
pdf(width=5, height=5, file="P3figure_corT_screeplot.pdf")
screeplot(pcaX)
dev.off()

## calculate
X<-as.matrix(X)
StockReturn = (X[2 : n, ] - X[1 : (n - 1), ]) / X[1 : (n - 1), ]
pcaReturn = princomp(StockReturn, cor=F)
V = pcaReturn$loadings
center = pcaReturn$center
scale = pcaReturn$scale

# Plot
# biplot
pdf(width=8, height=8, file="P3figure_corT_biplotReturn.pdf")
biplot(pcaReturn, cex=0.6)
dev.off()

# screeplot
pdf(width=5, height=5, file= "P3figure_corT_screeplotReturn.pdf")
screeplot(pcaReturn)
dev.off()

