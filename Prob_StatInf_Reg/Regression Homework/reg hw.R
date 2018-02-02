setwd('D:/Subject Materials/Columbia/LINEAR REGRESSION MODELS/data')
getwd()
#####reg hw1########################################

###1.19###
da = read.table("CH01PR19.txt")

model19<-lm(da$V1~da$V2)
summary(model19)

sum((da$V2-mean(da$V2))*(da$V1-mean(da$V1)))/sum((da$V2-mean(da$V2))^2)
plot(da$V2,da$V1)
abline(model19$coefficients)

####
d = read.table("APPENC02.txt")
names(d)<-c("Identification number",
"County",
"State",
"Land area",
"Total population",
"Percent of population aged 18-34",
"Percent of population 65 or older",
"Number of active physicians",
"Number of hospital beds",
"Total serious crimes",
"Percent high school graduates",
"Percent bachelor's degrees",
"Percent below poverty level",
"Percent unemployment",
"Per capita income",
"Total personal income",
"Geographic region")

Y<-d$`Number of active physicians`
X<-cbind(d$`Total population`,d$`Number of hospital beds`,d$`Total personal income`)

lm.cdi1<-lm(Y~X[,1])
summary(lm.cdi1)
plot(X[X[,1]<1e+06,1],Y[X[,1]<1e+06],cex = 0.5,xlab="Total population",ylab="Number of active physicians")
abline(lm.cdi1$coefficients[1],lm.cdi1$coefficients[2])
MSE1 <- sum(lm.cdi1$residuals^2)/(440-2)

lm.cdi2<-lm(Y~X[,2])
summary(lm.cdi2)
plot(X[X[,2]<5000,2],Y[X[,2]<5000],cex = 0.5,xlab="Number of hospital beds",ylab="Number of active physicians")
abline(lm.cdi2$coefficients[1],lm.cdi2$coefficients[2])
MSE2 <- sum(lm.cdi2$residuals^2)/(440-2)

lm.cdi3<-lm(Y~X[,3])
summary(lm.cdi3)
plot(X[X[,3]<30000,3],Y[X[,3]<30000],cex = 0.5,xlab="Total personal income",ylab="Number of active physicians")
abline(lm.cdi3$coefficients[1],lm.cdi3$coefficients[2])
MSE3 <- sum(lm.cdi3$residuals^2)/(440-2)

####

Y1<-d$`Per capita income`[d$`Geographic region`==1]
X1<-d$`Percent bachelor's degrees`[d$`Geographic region`==1]
lm.cdi1<-lm(Y1~X1)
summary(lm.cdi1)

Y2<-d$`Per capita income`[d$`Geographic region`==2]
X2<-d$`Percent bachelor's degrees`[d$`Geographic region`==2]
lm.cdi2<-lm(Y2~X2)
summary(lm.cdi2)

Y3<-d$`Per capita income`[d$`Geographic region`==3]
X3<-d$`Percent bachelor's degrees`[d$`Geographic region`==3]
lm.cdi3<-lm(Y3~X3)
summary(lm.cdi3)

Y4<-d$`Per capita income`[d$`Geographic region`==4]
X4<-d$`Percent bachelor's degrees`[d$`Geographic region`==4]
lm.cdi4<-lm(Y4~X4)
summary(lm.cdi4)
x=0:35
plot(c(0,35),c(10000,30000),cex = 0.5,xlab="Percent bachelor's degrees",ylab="Per capita income")
abline(lm.cdi1$coefficients[1],lm.cdi1$coefficients[2])
abline(lm.cdi2$coefficients[1],lm.cdi2$coefficients[2])
abline(lm.cdi3$coefficients[1],lm.cdi3$coefficients[2])
abline(lm.cdi4$coefficients[1],lm.cdi4$coefficients[2])

Y<-d$`Per capita income`
X1<-d$`Percent bachelor's degrees`
R<-d$`Geographic region`
R<-c(rep(1,220),rep(0,220))
d$`Geographic region`<-as.factor(d$`Geographic region`)
R1<-(d$`Geographic region`==1)*1
R2<-(d$`Geographic region`==2)*1
R3<-(d$`Geographic region`==3)*1
R4<-(d$`Geographic region`==4)*1
as.factor(sample(1:4))
lm.reduce<-lm(Y~X1)
lm.full<-lm(Y~X1+R1*X1+R2*X1+R3*X1)
I(X1*R1)anova(lm.reduce,lm.full)
numerator=(sum(lm.reduce$residuals^2)-sum(lm.full$residuals^2))/(lm.reduce$df.residual-lm.full$df.residual)
denominator=sum(lm.full$residuals^2)/lm.full$df.residual
f=numerator/denominator
f
qf(0.95,lm.reduce$df.residual-lm.full$df.residual,lm.full$df.residual)

sum(lm.cdi1$residuals^2)/101
sum(lm.cdi2$residuals^2)/(lm.cdi2$df.residual)
mean(lm.cdi2$residuals^2)
mean(lm.cdi3$residuals^2)
mean(lm.cdi4$residuals^2)

sum(lm.reduce$residuals^2)

mean(lm.cdi1$residuals^2)
length(lm.cdi1$residuals)
lm.reduce$df.residual

#####reg hw2########################################


summary(model19)

d22 = read.table("CH01PR22.txt")
model22<-lm(d22$V1~d22$V2)
summary(model22)
2.03438-0.09039*2.98

(2.03438-2)/0.09039
2-2*pt(0.3803518,14)

sd(da$V2)



1/118+(28-mean(da$V2))^2/sum((da$V2-mean(da$V2))^2)
0.0706/sqrt(0.01298128)
sum(model19$residuals^2)/model19$df.residual
mean(model19$residuals^2)
sqrt(0.3882848*(1/118+0.004506707))

sqrt(0.3818134*(1/118+0.004506707))
sqrt(0.3818*(1/118+0.0045))

###2.63#refer to 1.44#

b_1 <- lm.cdi4$coefficients[2]
b_1
t <- qt(0.95,lm.cdi4$df.residual)
sd2 <- sum((lm.cdi4$residuals)^2)/lm.cdi4$df.residual / sum((X4-mean(X4))^2)
b_1-t*sqrt(sd2)
b_1+t*sqrt(sd2)

#####reg hw3################


anova(model19)

anova(model22)

?anova
3.5878/0.3883


pred22 <- predict(model22,interval='prediction')

plot( c(0,40),c(-20,20),type="n",ylab="y",xlab="x")
plot( d22$V2,(d22$V1-pred22[,1]),ylab="y-hat(y)",xlab="x")
plot( d22$V2,(pred22[,1]-mean(d22$V1)),ylab="hat(y)-bar(y)",xlab="x")
lines(d22$V2,(d22$V1-pred22[,1]),type='p')
lines(d22$V2,(pred22[,1]-mean(d22$V1)),type='p')
1-10.5/5297.5

d22$V2
pred22[,1]-mean(d22$V1)


X=c(1,4,10,11,14)
sum((5+3*X-29)^2)
0.36+3^2*sum((X-8)^2)
###2.66##
e=c(-4.9633070,6.4244096,-0.4761277,-2.8104307,-0.4428723)

B1=c()
for (i in 1:200){
  e=rnorm(5, mean = 0, sd = 5)
  X=c(4,8,12,16,20)
  Y=20+4*X+e
  model66 <- lm(Y~X)
  B1[i]=model66$coefficients[2]
}

model66 <- lm(Y~X)
summary(model66)

plot(B1)

MSE=sum(model66$residuals^2) / model66$df.residual
s=sqrt( MSE*(1/5+2^2/sum((X-12)^2)) )

59.55603 + qt(0.975,3)*s
59.55603 - qt(0.975,3)*s

newdata=data.frame(X=10)
newy=predict(model66, newdata)
predict(model66, newdata, interval = "confidence")

Bool66=c()
Bool65=c()
for (i in 1:200){
  e=rnorm(5, mean = 0, sd = 5)
  X=c(4,8,12,16,20)
  Y=20+4*X+e
  model66 <- lm(Y~X)
  newdata=data.frame(X=10)
  newy=20 + 4*10
  #Bool66[i]<-(newy%in%predict(model66, newdata, interval = "confidence"))*1
  interval = predict(model66, newdata, interval = "confidence")
  Bool66[i]<-(newy>min(interval) & newy<max(interval))
}
sum(Bool66) / length(Bool66)

mean(B1)
sd(B1)
sqrt(25/sum((X-mean(X))^2))




d20 = read.table("CH01PR20.txt")
model20<-lm(d20$V1~d20$V2)
summary(model20)
plot(d20$V2[d20$V2<=8],d20$V1[d20$V2<=8],
     xlab="number of copiers",ylab="total number of minutes")
abline(model20$coefficients[1],model20$coefficients[2])

newdata=c(1:8)
pred=model20$coefficients[1]+model20$coefficients[2]*newdata
pred

MSE=sum(model20$residuals^2) / model20$df.residual
s=sqrt( MSE*(1/45+(newdata-mean(d20$V2))^2/sum((d20$V2-mean(d20$V2))^2)) )
s

F = 2*qf(0.9,2,43)
W = sqrt(F)
Yup=pred+W*s
Ylow=pred-W*s

abline(newdata,Yup,col='red')
abline(newdata,Ylow,col='red')


Q4 <- function(X){
  sqrt(0.36/sum((X-mean(X))^2))
}
X=6:10
X
Q4(X)

X=c(1,4,10,11,14)

# beta0=10
# Y=beta0+rnorm(30)
# SSR=sum(beta0-mean(Y))
# SSE=sum(Y-beta0)
# F=MSR/(MSE/28)










#####reg hw4######

boxplot(da$V2,xlab="ACT scores")
plot(model19$fitted.values, model19$residuals,
     xlab = "fitted.value", ylab = "residuals")
qqplot(model19$residuals)
summary(model19)


qqnorm(model19$residuals)
qqline(model19$residuals, col = 2)

StdErr = sqrt(sum((da$V1-mean(da$V1))^2))/119
ExpVals = sapply(1:120, function(k) StdErr*qnorm((k-.375)/(120+.25)))
cor(sort(ExpVals),sort(model19$residuals))

res1 <- model19$residuals[da$V2<26]
res2 <- model19$residuals[da$V2>=26]

d1 <- abs(res1-median(res1))
d2 <- abs(res2-median(res2))

s <- sqrt((sum((d1-mean(d1))^2) + sum((d2-mean(d2))^2))/(118))
t <- (mean(d1)-mean(d2))/(s*(sqrt(1/length(res1) + 1/length(res2))))
t
qt(0.995,118)


d3 = read.table("CH03PR03.txt")

model3_1 <- lm(d3$V1~d3$V3)
model3_2 <- lm(d3$V1~d3$V4)

plot(d3$V3,model19$residuals,xlab="X2",ylab="residuals")
plot(d3$V4,model19$residuals,xlab="X3",ylab="residuals")

###3.9

d9 = read.table("CH03PR09.txt")

plot(d9$V1,d9$V2,xlab="X",ylab="residuals")
plot(d9$V1,abs(d9$V2),xlab="X",ylab="absolute residuals")

plot(log(d9$V2),model9.fit$residuals)

###3.16

d15 = read.table("CH03PR15.txt")

plot(d15$V2,d15$V1,xlab="X",ylab="Y")
plot(d15$V2,log(d15$V1),xlab="X",ylab="log(Y)")
plot(d15$V2,sqrt(d15$V1),xlab="X",ylab="Y")

SSE=c()
lambda=c(-0.2,-0.1,0.1,0.2)
K2=prod(d15$V1)^(1/length(d15$V1))
for (i in 1:4){
  K1=1/lambda[i]/K2^(lambda[i]-1)
  coxY=K1*(d15$V1^lambda[i]-1)
  model.boxcox <- lm(coxY~d15$V2)
  a=anova(model.boxcox)
  SSE[i]=a$`Sum Sq`[2]
}
coxY=K2*log(d15$V1)/log(exp(1))
model.boxcox <- lm(coxY~d15$V2)
a=anova(model.boxcox)
SSE[5]=a$`Sum Sq`[2]
SSE

boxY=log(d15$V1)/log(10)
model.boxcox <- lm(boxY~d15$V2)
model.boxcox$coefficients


plot(d15$V2,log(d15$V1)/log(10),xlab="X",ylab="log10Y")
abline(model.boxcox$coefficients[1:2])

# coxY=d15$V1^0.1
# model.boxcox <- lm(coxY~d15$V2)
# plot(d15$V2,coxY,xlab="X",ylab="coxY")
# abline(model.boxcox$coefficients[1:2])
# sum((model.boxcox$residuals)^2)
# a=anova(model.boxcox)
# a$`Sum Sq`[2]

plot(model.boxcox$fitted.values,model.boxcox$residuals,
     xlab="fitted value", ylab="residuals")
normx=rnorm(15)
qqnorm(model.boxcox$residuals)

###3.24

d24 = read.table("CH03PR24.txt")
colnames(d24)<-c("Y","X")
model24<-lm(Y~X,data=d24)
model24$coefficients
plot(d24$V2,model24$residuals,xlab="X",ylab="residuals")
plot(model24)
summary(model24)
#d24=d24[-7,]

newdata24=data.frame(X=12)
Ynew=predict(model24, newdata24,level=0.99)
MSE=sum((d24$Y-model24$fitted.values)^2)/model24$df.residual
s2=MSE*(1+1/7+(12-mean(d24$X))^2/sum((d24$X-mean(d24$X))^2))
Ynew+sqrt(s2)*qt(0.995,5)
Ynew-sqrt(s2)*qt(0.995,5)
###3.31

d31 <- read.table("APPENC07.txt")
names(d31)<-
  c("Identification number",
    "Sales price",
    "Finished square feet",
    "Number of bedrooms",
    "Number of bathrooms",
    "Air conditioning",
    "Garage size",
    "Pool",
    "Year built",
    "Quality",
    "Style",
    "Lot size",
    "Adjacent to highway")
set.seed(0)
sam<-sample(1:522, 200)
d31<-d31[sam,]

d31.fit <- data.frame(X=c(d31$`Finished square feet`),
                      Y=c(d31$`Sales price`))


model31 <- lm(Y~X,data=d31.fit)
summary(model31)
plot(model31$fitted.values,model31$residuals
     ,xlab="fitted", ylab="residuals")
qqnorm(model31$residuals)

model31.fit <- lm(Y~X,data=d31.fit)
summary(model31.fit)
plot(Y~X,data=d31.fit,xlab="Finished square feet",ylab="Sales price")
plot(d31.fit$X,model31$residuals)

plot(model31.fit$fitted.values,model31.fit$residuals)
qqnorm(model31.fit$residuals)
ano31 <- anova(model31.fit)
ano31$`Sum Sq`

# #library(MASS)

boxcox(model31,lambda = seq(-0.5, 0.5, by=0.01))

lambda <- -0.06
d31.fit <- cbind(d31.fit,Ylam<- (d31.fit$Y^lambda-1)/lambda)
d31.fit <- d31.fit[-Ylam]
d31.fit <- cbind(d31.fit,Ylam<- d31.fit$Y^lambda)
model31.lam <- lm(Ylam~X, data=d31.fit)

plot(model31.lam$fitted.values,model31.lam$residuals
     ,xlab="fitted", ylab="residuals")
qqnorm(model31.lam$residuals)

summary(model31.lam)
anova(lm(Ylam~X,data=d31.fit), lm(Ylam~factor(X),data=d31.fit))

###3.32

d32 <- read.table("APPENC05.txt")
names(d32)<-
  c("Identification number",
    "PSA level",
    "Cancer volume",
    "Weight",
    "Age",
    "Benign prostatic hyperplasia",
    "Seminal vesicle invasion",
    "Capsular penetration",
    "Gleason score")


d32.lm <- data.frame(X=c(d32$`Cancer volume`),
                      Y=c(d32$`PSA level`))

plot(Y~X,data=d32.lm)
plot(Y~X,data=d32.fit)
model32 <- lm(Y~X,data=d32.lm)
summary(model32)
plot(model32$fitted.values,model32$residuals
     ,xlab="fitted", ylab="residuals")
qqnorm(model32$residuals)

model32.fit <- lm(Y~X,data=d32.fit)
summary(model32.fit)
plot(Y~X,data=d32.fit,xlab="Finished square feet",ylab="Sales price")
plot(d32.fit$X,model31$residuals)

plot(model31.fit$fitted.values,model31.fit$residuals)
qqnorm(model31.fit$residuals)
ano31 <- anova(model31.fit)
ano31$`Sum Sq`

# #library(MASS)

boxcox(model32,lambda = seq(-0.5, 0.5, by=0.01))
boxcox(model32,lambda = seq(0, 0.2, by=0.01))

lambda <- 0.1
# d32.fit <- cbind(d32.fit,Ylam<- (d32.fit$Y^lambda-1)/lambda)
# d32.fit <- d32.fit[-Ylam]
d32.lm <- cbind(d32.lm,Ylam<- d32.lm$Y^lambda)
model32.lam <- lm(Ylam~X, data=d32.lm)

plot(model32.lam$fitted.values,model32.lam$residuals
     ,xlab="fitted", ylab="residuals")
lines(model32.lam$fitted.values,
      rep(0,length(model32.lam$fitted.values)),col='red')
qqnorm(model32.lam$residuals)

summary(model32.lam)
order(abs(model32.lam$residuals))
#
d32.fit <- data.frame(X=c(d32$`Cancer volume`[-1:-4]),
                     Y=c(d32$`PSA level`[-1:-4]))
model32.fit <- lm(Y~X,data=d32.fit)
summary(model32.fit)
plot(model32.fit$fitted.values,model32.fit$residuals
     ,xlab="fitted", ylab="residuals")
qqnorm(model32.fit$residuals)

boxcox(model32.fit,lambda = seq(-0.5, 0.5, by=0.01))
boxcox(model32.fit,lambda = seq(-0.2, 0.2, by=0.01))

lambda <- -0.02
# d32.fit <- cbind(d32.fit,Ylam<- (d32.fit$Y^lambda-1)/lambda)
# d32.fit <- d32.fit[-Ylam]
d32.fit <- cbind(d32.fit,Ylam<- d32.fit$Y^lambda)
model32.fitlam <- lm(Ylam~X, data=d32.fit)

plot(model32.fitlam$fitted.values,model32.fitlam$residuals
     ,xlab="fitted", ylab="residuals")
lines(model32.fitlam$fitted.values,
      rep(0,length(model32.fitlam$fitted.values)),col='red')
qqnorm(model32.fitlam$residuals)


summary(model32.fitlam)
dim(d32.fit)
anova(lm(Ylam~X,data=d32.fit), lm(Ylam~factor(X),data=d32.fit))

#####reg hw5#####

###6.5###
d5 = read.table("CH06PR05.txt")
dim(d5)
names(d5)

# library(ggplot2)

p11 <- ggplot(data = d5) +
  geom_text(mapping = aes(x=80, y=80, label = "degree of brand liking"), size=5) +
  labs(title = "", x = "", y = "")
p12 <- ggplot(d5, aes(V2, V1)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p13 <- ggplot(d5, aes(V3, V1)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p21 <- ggplot(d5, aes(V1, V2)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p22 <- ggplot(data = d5) +
  geom_text(mapping = aes(x=6, y=6, label = "moisture content"), size=5) +
  labs(title = "", x = "", y = "")
p23 <- ggplot(d5, aes(V3, V2)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p31 <- ggplot(d5, aes(V1, V3)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p32 <- ggplot(d5, aes(V2, V3)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p33 <- ggplot(data = d5) +
  geom_text(mapping = aes(x=3, y=3, label = "sweetness"), size=5) +
  labs(title = "", x = "", y = "")

multiplot(p11,p21,p31,p12,p22,p32,p13,p23,p33, cols=3)

names(d5)=c("degree_of_brand_liking","moisture_content","sweetness")

#b
model5 <- lm(V1~V2+V3, data=d5)
model5
summary(model5)

model5$residuals
boxplot(model5$residuals)

#f
d5[1:2,]
model5$fitted.values

model5$coefficients
37.65+4.375*2

model5_1 <- lm(V1~V2,data=d5)
model5_2 <- lm(V1~V3,data=d5)

model5_1$fitted.values
model5_2$fitted.values

temp <- table(d5$V2,d5$V3)

anova(lm(d5$V1~d5$V2+d5$V3), lm(d5$V1~factor(d5$V2)*factor(d5$V3)))


#6.8
X = cbind(rep(1,nrow(d5)), d5$V2, d5$V3)
Xh = c(1,5,4)
MSE = sum((model5$residuals)^2) / model5$df.residual
s = sqrt(MSE*(t(Xh)%*%solve((t(X)%*%X))%*%Xh))
data.frame(V2=5,V3=4)
Ynew = predict(model5, data.frame(V2=5,V3=4),level=0.99)
Ynew+qt(0.995,model5$df.residual)*s
Ynew-qt(0.995,model5$df.residual)*s

s_pred = sqrt(MSE*(1+t(Xh)%*%solve((t(X)%*%X))%*%Xh))
Ynew+qt(0.995,model5$df.residual)*s_pred
Ynew-qt(0.995,model5$df.residual)*s_pred


#####reg hw6####

###3.14
d22 = read.table("CH01PR22.txt")
dim(d22)
names(d22) <- c("Y", "X")
anova(lm(Y~X,data=d22), lm(Y~factor(X),data=d22))
qf(0.99,2,12)

###7.7
d18 = read.table("CH06PR18.txt")
dim(d18)

model18 <- lm(d18$V1~d18$V2 + d18$V3 + d18$V4 + d18$V5)
model18_4 <- lm(d18$V1~d18$V5)
model18_14 <- lm(d18$V1~d18$V2 + d18$V5)
model18_124 <- lm(d18$V1~d18$V2 + d18$V3 + d18$V5)

SSE18_4 <- tail(anova(model18_4)$`Sum Sq`,1)
SSE18_14 <- SSE18_4 - tail(anova(model18_14)$`Sum Sq`,1)
SSE18_124 <- SSE18_14 - tail(anova(model18_124)$`Sum Sq`,1)

SSR4 <- sum((model18_4$fitted.values-mean(d18$V1))^2)
SSR1_4 <- -SSR4 + sum((model18_14$fitted.values-mean(d18$V1))^2)
SSR14 <- sum((model18_14$fitted.values-mean(d18$V1))^2)
SSR2_14 <- -SSR14 + sum((model18_124$fitted.values-mean(d18$V1))^2)
SSR124 <- sum((model18_124$fitted.values-mean(d18$V1))^2)
SSR3_124 <- -SSR124 + sum((model18$fitted.values-mean(d18$V1))^2)

SSR4; SSR1_4; SSR2_14; SSR3_124

temp <- anova(model18)
temp$`Sum Sq`/temp$`Mean Sq`
sum((model18$residuals)^2)/model18$df.residual

numerator <- sum(model18_124$residuals^2) - sum(model18$residuals^2)
denominator <- sum(model18$residuals^2) / model18$df.residual
Fstatistic = numerator / denominator
Fstatistic
qf(0.99, 1, model18$df.residual)
1 - pf(Fstatistic,1,model18$df.residual)

###7.10
redy <- d18$V1 + 0.1*d18$V2 - 0.4*d18$V3
model18_red <- lm(redy ~ d18$V4 + d18$V5)
numerator <- (sum(model18_red$residuals^2) - sum(model18$residuals^2)) / 2
denominator <- sum(model18$residuals^2) / model18$df.residual
Fstatistic = numerator / denominator
Fstatistic
qf(0.99, 2, model18$df.residual)
1 - pf(Fstatistic,2,model18$df.residual)


###7.16
d5 <- read.table("CH06PR05.txt")
d5_trans <- d5
dim(d5)

n = nrow(d5_trans)
sy = sd(d5_trans$V1)
sx1 = sd(d5_trans$V2)
sx2 = sd(d5_trans$V3)
d5_trans$V1 = 1/sqrt(n-1)*(d5_trans$V1-mean(d5_trans$V1))/sy
d5_trans$V2 = 1/sqrt(n-1)*(d5_trans$V2-mean(d5_trans$V2))/sx1
d5_trans$V3 = 1/sqrt(n-1)*(d5_trans$V3-mean(d5_trans$V3))/sx2

model5_trans <- lm(d5_trans$V1 ~ d5_trans$V2 + d5_trans$V3)
summary(model5_trans)

b <- model5_trans$coefficients
names(b) <- c()
b1 = (sy/sx1) * b[2]
b2 = (sy/sx2) * b[3]
b0 = mean(d5$V1) - b1*mean(d5$V2) - b2*mean(d5$V3)
b0 ; b1 ; b2

###7.24

model24 <- lm(d5$V1 ~ d5$V2 + d5$V3)
model24a <- lm(d5$V1 ~ d5$V2)
model24a

#c
model24b <- lm(d5$V1 ~ d5$V3)
SSR1 <- sum((model24a$fitted.values-mean(d5$V1))^2)
SSR2 <- sum((model24b$fitted.values-mean(d5$V1))^2)
SSR12 <- -SSR2 + sum((model24$fitted.values-mean(d5$V1))^2)
SSR1; SSR12


###7.37

d37 <- read.table("APPENC02.txt")
dim(d37)


names(d37) <- c("Identification number", "County", "State", "Land area",
                "Total population", "Percent of population aged 18-34",
                "Percent of population 65 or older",
                "Number of active physicians", "Number of hospital beds",
                "Total serious crimes", "Percent high school graduates",
                "Percent bachelor's degrees", "Percent below poverty level",
                "Percent unemployment", "Per capita income",
                "Total personal income", "Geographic region")

model37_12 <- lm(`Number of active physicians` ~
                   `Total population`+`Total personal income`, data = d37)
model37_123 <- lm(`Number of active physicians` ~
                   `Total population`+`Total personal income` + 
                   `Land area`, data = d37)
model37_124 <- lm(`Number of active physicians` ~
                   `Total population`+`Total personal income` + 
                   `Percent of population 65 or older`, data = d37)
model37_125 <- lm(`Number of active physicians` ~
                   `Total population`+`Total personal income` + 
                   `Number of hospital beds`, data = d37)
model37_126 <- lm(`Number of active physicians` ~
                   `Total population`+`Total personal income` + 
                   `Total serious crimes`, data = d37)

SSE12 <- sum((d37$`Number of active physicians`-model37_12$fitted.values)^2)

SSR12 <- sum((model37_12$fitted.values-mean(d37$`Number of active physicians`))^2)
SSR3_12 <- -SSR12 + sum((model37_123$fitted.values - 
                           mean(d37$`Number of active physicians`))^2)
SSR4_12 <- -SSR12 + sum((model37_124$fitted.values - 
                           mean(d37$`Number of active physicians`))^2)
SSR5_12 <- -SSR12 + sum((model37_125$fitted.values - 
                           mean(d37$`Number of active physicians`))^2)
SSR6_12 <- -SSR12 + sum((model37_126$fitted.values - 
                           mean(d37$`Number of active physicians`))^2)

R3_12 = SSR3_12 / SSE12
R4_12 = SSR4_12 / SSE12
R5_12 = SSR5_12 / SSE12
R6_12 = SSR6_12 / SSE12

R3_12; R4_12; R5_12; R6_12

SSR3_12; SSR4_12; SSR5_12; SSR6_12


#c
numerator <- sum(model37_12$residuals^2) - sum(model37_125$residuals^2)
denominator <- sum(model37_125$residuals^2) / model37_125$df.residual
Fstatistic = numerator / denominator
Fstatistic
qf(0.99, 1, model37_12$df.residual)
1 - pf(Fstatistic,1,model37_12$df.residual)

numerator <- sum(model37_12$residuals^2) - sum(model37_125$residuals^2)
denominator <- sum(model37_125$residuals^2) / model37_125$df.residual
Fstatistic = numerator / denominator
Fstatistic

#####reg hw7####
#8.6
#model6a<-lm(V1~V2+I(V2^2),data=d6)
d6 = read.table("CH08PR06.txt")
y <- d6$V1
x <- d6$V2
x2 <- d6$V2^2
model6a <- lm(y ~ x + x2)
plot(x,y,xlim=c(6,28),ylim=c(0,30))
coeff <- model6a$coefficients
coeff

f6a <- function(x) {coeff[1] + coeff[2]*x + coeff[3]*x^2}
x <- c(80:250)/10
par(new=T)
plot(x, f6a(x),type = 'l',xlim=c(6,28),ylim=c(0,30),ylab="y")

a<-summary(model6a)
a$fstatistic

SSR <- sum(anova(model6a)$`Sum Sq`[1:2])
SSE <- sum(anova(model6a)$`Sum Sq`[3])
(SSR / 2) / (SSE / 24)
qf(0.99,2,24)
1-pf((SSR/2)/(SSE/24),2,24)

#c
Yh1 <- predict(model6a, data.frame(x=10,x2=100),level=0.99)
Yh2 <- predict(model6a, data.frame(x=15,x2=225),level=0.99)
Yh3 <- predict(model6a, data.frame(x=20,x2=400),level=0.99)
X = cbind(rep(1,27),x,x2)
MSE <- SSE / 24
s2b <- MSE * solve(t(X)%*%X)
Xh1 = c(1,10,10^2)
Xh2 = c(1,15,15^2)
Xh3 = c(1,20,20^2)
s2Y1 = t(Xh1)%*%s2b%*%Xh1
s2Y2 = t(Xh2)%*%s2b%*%Xh2
s2Y3 = t(Xh3)%*%s2b%*%Xh3
Yh1;Yh2;Yh3;s2Y1;s2Y2;s2Y3

Yh1 - qt(0.9983333,24)*sqrt(s2Y1)
Yh1 + qt(0.9983333,24)*sqrt(s2Y1)
Yh2 - qt(0.9983333,24)*sqrt(s2Y2)
Yh2 + qt(0.9983333,24)*sqrt(s2Y2)
Yh3 - qt(0.9983333,24)*sqrt(s2Y3)
Yh3 + qt(0.9983333,24)*sqrt(s2Y3)


#d


Yh2 - qt(0.995,24)*sqrt(MSE+s2Y2)
Yh2 + qt(0.995,24)*sqrt(MSE+s2Y2)


#e

model6e_f <- lm (y ~ x + x2)
model6e_r <- lm (y ~ x)
numerator <- sum(model6e_r$residuals^2) - sum(model6e_f$residuals^2)
denominator <- sum(model6e_f$residuals^2) / model6e_f$df.residual
Fstatistic = numerator / denominator
Fstatistic
qf(0.99, 1, model6e_f$df.residual)
1 - pf(Fstatistic,1,model6e_f$df.residual)

###8.42

da3 = read.table("APPENC03.txt")
names(da3) <- c("Identification number","Market share",
                "Price","Gross Nielsen rating points",
                "Discount price","Package promotion","Month","Year")
y <- da3$`Market share`
x1 <- da3$Price
x2 <- da3$`Gross Nielsen rating points`
x3 <- factor(da3$`Discount price`)
x4 <- factor(da3$`Package promotion`)
x5 <- factor(ifelse(da3$Year!=2000,da3$Year,0))

model42a <- lm(y~x1 + x2 + x3 + x4 +x5) 
plot(model42a)

#b
x1_2 <- da3$Price^2
x2_2 <- da3$`Gross Nielsen rating points`^2
x12 <- da3$Price * da3$`Gross Nielsen rating points`
model42b <- lm(y~x1 + x1_2 + x12 + x2 + x2_2 + x3 + x4 +x5) 

numerator <- (sum(model42a$residuals^2) - sum(model42b$residuals^2))/3
denominator <- sum(model42b$residuals^2) / model42b$df.residual
Fstatistic = numerator / denominator
Fstatistic
qf(0.95, 3, model42b$df.residual)
1 - pf(Fstatistic,3,model42b$df.residual)

#c
model42c <- lm(y~x1 + x3 + x4)
numerator <- (sum(model42c$residuals^2) - sum(model42a$residuals^2))/4
denominator <- sum(model42a$residuals^2) / model42a$df.residual
Fstatistic = numerator / denominator
Fstatistic
qf(0.95, 4, model42a$df.residual)
1 - pf(Fstatistic,4,model42a$df.residual)




###8.43

da4 = read.table("APPENC04.txt")
names(da4) <- c("Identification number","GPA",
                "High school class rank","ACT score",
                "Academic year")

y <- da4$GPA
x1 <- da4$`High school class rank`
x2 <- da4$`ACT score`
x3 <- factor(da4$`Academic year`)
test_loc <- sample(1:nrow(da4),100, replace = F)
test <- data.frame(y=y[test_loc],
                   x1=x1[test_loc],x2=x2[test_loc],x3=x3[test_loc])
train <- data.frame(y=y[-test_loc],
                    x1=x1[-test_loc],x2=x2[-test_loc],x3=x3[-test_loc])

model43 <- lm (y~ x1 + x2 + x3, data = train)
summary(model43)
#1
model43_r <- lm (y~ x1 + x2, data = train)
numerator <- (sum(model43_r$residuals^2) - sum(model43$residuals^2))/4
denominator <- sum(model43$residuals^2) / model43$df.residual
Fstatistic = numerator / denominator
Fstatistic
qf(0.95, 4, model43$df.residual)
1 - pf(Fstatistic,4,model43$df.residual)
summary(model43_r)

testpred <- predict(model43_r, test[,2:4], level=0.99)
plot(test[,1],testpred,xlim=c(0,4),ylim=c(0,4),xlab="real",ylab="predict")
abline(0,1)

model43r2 <- lm (y~ x1 + x2 + I(x1*x2) +I(x1^2)+I(x2^2), data = train)
numerator <- (sum(model43_r$residuals^2) - sum(model43r2$residuals^2))/3
denominator <- sum(model43r2$residuals^2) / model43r2$df.residual
Fstatistic = numerator / denominator
Fstatistic
qf(0.95, 3, model43r2$df.residual)
1 - pf(Fstatistic,3,model43r2$df.residual)

model43r3 <- lm (y~ x1 +x2+I(x1^2), data = train)
numerator <- (sum(model43r3$residuals^2) - sum(model43r2$residuals^2))/3
denominator <- sum(model43r2$residuals^2) / model43r2$df.residual
Fstatistic = numerator / denominator
Fstatistic
qf(0.95, 3, model43r2$df.residual)
1 - pf(Fstatistic,3,model43r2$df.residual)

summary(model43r3)$coefficient[,1]
testpred <- predict(model43r3, test[,2:4], level=0.95)
plot(test[,1],testpred,xlim=c(0,4),ylim=c(0,4),xlab="real",ylab="predict")
abline(0,1)

#10.5
d5 = read.table("CH06PR05.txt")
y = d5$V1
x1 = d5$V2
x2 = d5$V3
model5yx1 <- lm(y~x1)
model5yx2 <- lm(y~x2)
model5x1x2 <- lm(x1~x2)
model5x2x1 <- lm(x2~x1)

plot(model5x1x2$residuals, model5yx2$residuals,
     xlab= "e(x1|x2)",ylab="e(y|x2)")
abline(0,0,lty=3)
plot(model5x2x1$residuals, model5yx1$residuals,
     xlab= "e(x2|x1)",ylab="e(y|x1)")
abline(0,0, lty=3)



model5yx1 <- lm(y~x1)
model5x2x1 <- lm(x2~x1)
model5yx1
model5x2x1
yy <- model5yx1$residuals
xx <- model5x2x1$residuals
plot(model5x2x1$residuals, model5yx1$residuals,
     xlab= "e(x2|x1)",ylab="e(y|x1)")
abline(0,0, lty=3)
modelyyxx <- lm(yy~xx)
abline(modelyyxx$coefficients)


#10.9

X = cbind(rep(1,nrow(d5)),x1,x2)
H = X%*%solve(t(X)%*%X)%*%t(X)
Yhat = H%*%y
e = (diag(rep(1,length(y)))-H)%*%y

SSE = anova(lm(y~x1+x2))$`Sum Sq`[3]
hii = diag(H)

ti = e*sqrt((16-3-1)/(SSE*(1-hii)-e^2))
data.frame(e,hii,ti)


plot(x1,x2)
Xnew = c(1,10,3)
t(Xnew)%*%solve(t(X)%*%X)%*%Xnew

MSE = anova(lm(y~x1+x2))$`Mean Sq`[3]
e^2/(3*MSE)*(hii/(1-hii)^2)


#####reg hw8####

#9.9
d9 = read.table("CH06PR15.txt")
dim(d9)

ABCp <- function(data,col,MSE){
  Y <- data[,1];n <- length(Y);p <- length(col) + 1
  if(p==2){model <- lm(Y~data[,col[1]])}
  else if(p==3){model <- lm(Y~data[,col[1]]+data[,col[2]])}
  else if(p==4){model <- lm(Y~data[,col[1]]+data[,col[2]]+data[,col[3]])}
  SSE <- sum((Y-model$fitted.values)^2)
  AIC = n*log(SSE) - n*log(n) + 2*p
  BIC = n*log(SSE) - n*log(n) + floor(log(n))*p
  Cp = SSE/MSE - (n-2*p)
  return (data.frame(AIC,Cp,BIC))
}

MSE <- anova(lm(V1~V2+V3+V4,data=d9))$`Mean Sq`[4]
col <- list(2,3,4,c(2,3),c(2,4),c(3,4),c(2,3,4))
ABIC <- sapply(col,ABCp,data=d9,MSE=MSE)
colnames(ABIC) <- col
ABIC


#9.15

d15 = read.table("CH09PR15.txt")
dim(d15)

p11 <- ggplot(data = d15) +
  geom_text(mapping = aes(x=80, y=80, label = "Creatinine clearance"), size=4) +
  labs(title = "", x = "", y = "")
p12 <- ggplot(d15, aes(V2, V1)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p13 <- ggplot(d15, aes(V3, V1)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p14 <- ggplot(d15, aes(V4, V1)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p21 <- ggplot(d15, aes(V1, V2)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p22 <- ggplot(data = d15) +
  geom_text(mapping = aes(x=1.75, y=2.0, label = "serum creatinine concentration"), size=3) +
  #geom_text(mapping = aes(x=1.75, y=1.5, label = ""), size=3) +
  labs(title = "", x = "", y = "")
p23 <- ggplot(d15, aes(V3, V2)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p24 <- ggplot(d15, aes(V4, V2)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p31 <- ggplot(d15, aes(V1, V3)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p32 <- ggplot(d15, aes(V2, V3)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p33 <- ggplot(data = d15) +
  geom_text(mapping = aes(x=50, y=50, label = "age"), size=5) +
  labs(title = "", x = "", y = "")
p34 <- ggplot(d15, aes(V4, V3)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p41 <- ggplot(d15, aes(V1, V4)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p42 <- ggplot(d15, aes(V2, V4)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p43 <- ggplot(d15, aes(V3, V4)) +
  geom_point() +
  labs(title = "", x = "", y = "")
p44 <- ggplot(data = d15) +
  geom_text(mapping = aes(x=80, y=80, label = "weight"), size=5) +
  labs(title = "", x = "", y = "")

multiplot(p11,p21,p31,p41,p12,p22,p32,p42,
          p13,p23,p33,p43,p14,p24,p34,p44, cols=4)

cor(d15[,2:4])

#c

model15c <- lm(V1~V2+V3+V4,data=d15)
summary(model15c)

#9.16
Y=d15[,1];X1=d15[,2];X2=d15[,3];X3=d15[,4]
d16 <- data.frame(Y,X1,X2,X3,X4=X1^2,X5=X1*X2,
                  X6=X2^2,X7=X2*X3,X8=X3^2,X9=X1*X3)

Cp2 <- function(data,col,MSE){
  Y <- data[,1];n <- length(Y);col <- unlist(col);p <- length(col) + 1
  X <- data.frame(data[,col])
  model <- lm(Y~.,data=X)
  SSE <- sum((Y-model$fitted.values)^2)
  Cp = SSE/MSE - (n-2*p)
  return (Cp)
}

col <- c()
col1 <- apply(combn(2:10,1),2,list)
col2 <- apply(combn(2:10,2),2,list)
col3 <- apply(combn(2:10,3),2,list)
col4 <- apply(combn(2:10,4),2,list)
col5 <- apply(combn(2:10,5),2,list)
col6 <- apply(combn(2:10,6),2,list)
col7 <- apply(combn(2:10,7),2,list)
col8 <- apply(combn(2:10,8),2,list)
col9 <- apply(combn(2:10,9),2,list)
col <- c(col1,col2,col3,col4,col5,col6,col7,col8,col9)
length(col)

model16 <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9,data=d16)
MSE <- anova(model16)$`Mean Sq`[10]
Cp <- sapply(col,Cp2,data=d16,MSE=MSE)
head(sort(Cp),3)
head(order(Cp),3)
col[131];col[263];col[153]



#9.19
X <- matrix(unlist(d16[,-1]),33,9)

my_stepwise(Y,X)

X <- matrix(rnorm(100),10,10)
y <- rnorm(10)
subset <- c(1,5,6,7,2,3)
Xtry <- cbind(X[,subset],X[,6])
fit <- lm(y ~ Xtry)
