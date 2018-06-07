FF_data = read.table("FamaFrench_mon_69_98.txt",header=T)
attach(FF_data)
library("Ecdat")
library("robust")
data(CRSPmon)
ge = 100*CRSPmon[,1] - RF
ibm = 100*CRSPmon[,2] - RF
mobil = 100*CRSPmon[,3] - RF
stocks=cbind(ge,ibm,mobil)
fit = lm(cbind(ge,ibm,mobil)~Mkt.RF+SMB+HML)
options(digits=3)
fit

pdf("FamaFrenchPairs.pdf",width=8,height=8)  ##  Figure 18.7
pairs(cbind(ge,ibm,mobil,Mkt.RF,SMB,HML))
graphics.off()

cor(fit$residuals)
covRob(fit$residuals,cor=T)
cor.test(fit$residuals[,1], fit$residuals[,2])
cor.test(fit$residuals[,1], fit$residuals[,3])
cor.test(fit$residuals[,2], fit$residuals[,3])

pdf("FamaFrenchResidualsPairs.pdf",width=6,height=5)  ## Figure 18.8
pairs(fit$residuals)
graphics.off()
