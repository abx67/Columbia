
# create data
Socio_tab <- c(rep("Low",4), rep("Medium",4), rep("High",4))
Boyscout_tab <- rep(c(rep("Yes",2),rep("No",2)),3)
delinquency_tab <- rep(c("Yes","No"),6)
frequency <- c(10,40,40,160,18,132,18,132,8,192,2,48)
Socioeconomic <- factor(c(rep(Socio_tab[1:12],frequency[1:12])))
Boyscout <- factor(c(rep(Boyscout_tab[1:12],frequency[1:12])))
delinquency <- factor(c(rep(delinquency_tab[1:12],frequency[1:12])))

# Descriptive Statistics

## Socio_tab
Socio_low <- which(Socioeconomic == 'Low')
Socio_medium <- which(Socioeconomic == 'Medium')
Socio_high <- which(Socioeconomic == 'High')
## Boyscout_tab
Boyscout_yes <- which(Boyscout == 'Yes')
Boyscout_no <- which(Boyscout == 'No')


labels <- c("yes", "no")

### Socioeconomic
x = table(delinquency[Socio_low])
piepercent <- round(100*x/sum(x), 1)
pie(x,labels=paste(piepercent,"%"),main = "low economic status",col= c('green','yellow'))
legend("topright", labels, cex = 0.8, fill = c('green','yellow'))

x = table(delinquency[Socio_medium])
piepercent <- round(100*x/sum(x), 1)
pie(x,labels=paste(piepercent,"%"),main = "medium economic status",col= c('green','yellow'))
legend("topright", labels, cex = 0.8, fill = c('green','yellow'))

x = table(delinquency[Socio_high])
piepercent <- round(100*x/sum(x), 1)
p1 <- pie(x,labels=paste(piepercent,"%"),main = "high economic status",col= c('green','yellow'))
legend("topright", labels, cex = 0.8, fill = c('green','yellow'))

### Boyscout
x = table(delinquency[Boyscout_no])
piepercent <- round(100*x/sum(x), 1)
pie(x,labels=paste(piepercent,"%"),main = "NOT Boyscout",col= c('green','yellow'))
legend("topright", labels, cex = 0.8, fill = c('green','yellow'))

x = table(delinquency[Boyscout_yes])
piepercent <- round(100*x/sum(x), 1)
pie(x,labels=paste(piepercent,"%"),main = "NOT Boyscout",col= c('green','yellow'))
legend("topright", labels, cex = 0.8, fill = c('green','yellow'))


options(repr.plot.width=8, repr.plot.height=3)
par(mfrow=c(1,3))
plot(Socioeconomic,Boyscout,xlab="Socioeconomic",ylab="Boyscout")
plot(Socioeconomic,delinquency,xlab="Socioeconomic",ylab="delinquency")
plot(Boyscout,delinquency,xlab="Boyscout",ylab="delinquency")

library(MASS)
logit.fit1 <- glm(delinquency~Boyscout+Socioeconomic, family = binomial("logit"))
summary(logit.fit1)


step(logit.fit1,direction = "backward")$anova

logit.fit2 <- glm(delinquency~Socioeconomic, family = binomial("logit"))
summary(logit.fit2)

confint(logit.fit2)

exp(confint(logit.fit2))

exp(logit.fit2$coefficients)
