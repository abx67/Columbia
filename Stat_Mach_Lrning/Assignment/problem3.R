###############
#Problem 3#####
###############
d3=read.table("train.3.txt",header = F,sep=',')
d5=read.table("train.5.txt",header = F,sep=',')
library(MASS)
library(glmnet)
d8=read.table("train.8.txt",header = F,sep=',')
test=read.table("ziptest.txt",header = F)
names(test)=c("y",names(test))[1:257]
test=test[test$y%in%c(3,5,8),]
acc=matrix(NA,4,2)
d3$y=3
d5$y=5
d8$y=8
d=rbind(d3,d5,d8)
lad.full=lda(y~.,data=d)
acc[1,1]=mean(predict(lad.full,d)$class == d$y)
acc[1,2]=mean(predict(lad.full,test)$class == test$y)

prin_comp=prcomp(d[1:256], scale. = T)
std_dev = prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
train.pca <- data.frame(y = d$y, prin_comp$x)[,1:50]
lad.pca=lda(y~.,data=train.pca )
acc[2,1]=mean(predict(lad.pca,train.pca)$class == d$y)
test.pca=as.data.frame(y = test$y,predict(prin_comp, newdata =test[,-1] ))[,1:50]
acc[2,2]=mean(predict(lad.pca,test.pca)$class == test$y)

d.mean=matrix(NA,dim(d)[1], 64)
for( i in 1:dim(d)[1])
{
  mat=matrix( d[i,1:256],16,16,byrow=T)
  matrix.4=matrix(NA,8,8)
  for( k in 1:8)
    for( t in 1:8)
    {
      matrix.4[k,t]=mean(   unlist (mat[2*k-0:1, 2*t-0:1] )   )
    }
  d.mean[i,]=c(c(matrix.4))
}
d.mean=data.frame(d.mean)
d.mean$y=d$y

test.mean=matrix(NA,dim(test)[1], 64)
for( i in 1:dim(test)[1])
{
  mat=matrix( test[i,-1],16,16,byrow=T)
  matrix.4=matrix(NA,8,8)
  for( k in 1:8)
    for( t in 1:8)
    {
      matrix.4[k,t]=mean(   unlist (mat[2*k-0:1, 2*t-0:1] )   )
    }
  test.mean[i,]=c(c(matrix.4))
}
test.mean=data.frame(test.mean)
test.mean$y=test$y[i]
lad.4mean=lda(y~.,data=d.mean)
acc[3,1]=mean(predict(lad.4mean,d.mean)$class == d$y)
acc[3,2]=mean(predict(lad.4mean,test.mean)$class == test$y)


x_train=as.matrix(d.mean[,1:64])
y_train=factor(d$y)
x_test=as.matrix(test.mean[,1:64])
y_test=factor(test$y,levels=levels(y_train))
fit=glmnet(x=x_train,y=y_train,  family="multinomial" )
L=predict(fit,x_train,type="response",s=0.01)[,,1]
predict_train=c()
for(i in 1:dim(L)[1])
  predict_train[i]= levels(y_train) [which( L[i,]==max(L[i,]))]
acc[4,1]=mean(predict_train==y_train)
L2=predict(fit,x_test,type="response",s=0.01)[,,1]
predict_test=c()
for(i in 1:dim(L2)[1])
  predict_test[i]= levels(y_test) [which( L2[i,]==max(L2[i,])  )]
acc[4,2]=mean(predict_test==y_test)
print(acc)
