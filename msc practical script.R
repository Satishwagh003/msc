## fitting simple regression model
df = read.csv("C:\\Users\\hp\\Desktop\\PGPDS\\Resume\\data for MLR.csv")
df
# fit simple regression
y=df$FINAL
x3=df$EXAM3
m=lm(y~x3)
summary(m)
#obtain correlation
cor(y,x3)
# anova
anova(m)
#test significance parameter
confint(m)
#construct c.i for regression parameter
#if p value less than 0.05 or level of significxance the parameter is singnificanct.
answer is the model is signifivcant
#construct c.i estimated value y
new= data.frame(x3=85)
predict(m,newdata=new,interval='confidence')


##### fitting multiple regression model ####
df = read.csv("C:\\Users\\hp\\Desktop\\PGPDS\\Resume\\data for MLR.csv")
df
y=df$FINAL
x1=df[,1]
x2=df[,2]
x3=df[,3]
m=lm(y~x1+x2+x3)
m
summary(m)
cor(df)
anova(m)
confint(m)
new=data.frame(x1=20,x2=25,x3=40)
predict(m,newdata=new,interval='confidence')

# **********2)variables selection and model building *****************
df = read.csv("C:\\Users\\hp\\Desktop\\PGPDS\\Resume\\data for MLR.csv")
df
y=df$FINAL
x1=df[,1]
x2=df[,2]
x3=df[,3]
m=lm(y~x1+x2+x3)
summary(m)
# backward selection method
install.packages("MASS")
library("MASS")
install.packages("leaps")

library("leaps")
all=regsubsets(y~x1+x2+x3,data=df,nbest=1,nvmax=3)
?regsubsets
s=summary(all)
s
cbind(s$which,round(cbind(rsq=s$rsq,adjr2=s$adjr2,cp=s$cp,bic=s$bic,rss=s$rss),3))
null=lm(y~1,data=df)
null
full=lm(y~x1+x2+x3,data=df)
full
stepAIC <- step(full, scope = list(lower = null, upper = full), data = df, direction = 'backward')
# same case in forward only change direction='forward'
# same case in stepwise only change in direction = 'both' 

*********3) fitting poission and negative binomial regression********
install.packages("dataset")
install.packages("devtools")
library(devtools)
df=warpbreaks
df
hist(df$breaks)
mean(df$breaks)
y=df$breaks
poi_model=glm(y~wool+tension,data=df,family=poisson(link="log"))
summary(poi_model)
## Quassi poisson regression model
poi_model2=glm(y~wool+tension,data=df,family=quasipoisson(link="log"))
summary(poi_model2)
install.packages("arm")
library(arm)
coef1 <- coef(poi_model)
coef2 <- coef(poi_model2)
se.coef <- sqrt(diag(vcov(poi_model))) # Standard errors of coefficients for poi_model
se.coef2 <- sqrt(diag(vcov(poi_model2))) # Standard errors of coefficients for poi_model2
exponent1 <- exp(coef1)
exponent2 <- exp(coef2)
model.both <- cbind(coef1, se.coef, coef2, se.coef2, exponent1, exponent2)
model.both
new=data.frame(wool="B",tension="M")
predict(poi_model2,newdata=new,type="response")

### justify negative is best fit rather than poission 
df=warpbreaks
df
y=df$breaks
mod_negbin=glm.nb(y~wool+tension,data=df)
summary(mod_negbin)
poi_model$aic
poi_model2$aic
mod_negbin$aic

********4)fitting simple and multiple logistic*********
## simple logistic
y=c(1,-----)
x1=c(1,------)
x2=c(1,----)
n1=length(y);n1
n2=length(x1);n2
n3=length(x3);n3
m=glm(y~x1,family="binomial")
summary(m)
d
## multiple logistic
m=glm(y~x1+x2,family="binomial")
summary(m)

********5)multicollinarity And fitting ridge*******
install.packages("car")
library(car)

df=read.csv("C:\\Users\\hp\\Desktop\\PGPDS\\Resume\\data mtcars.csv")
df
install.packages("car")
library(car)
install.packages("glmnet")
library(glmnet)
y=mtcars$hp
x=data.matrix(mtcars[,c('mpg','wt','drat','qsec')])
model=glmnet(x,y,alpha=0)
summary(model)
cv_model=cv.glmnet(x,y,alpha=0)
best_lambda=cv_model$lambda.min
plot(cv_model)
best_model=glmnet(x,y,alpha=0,lambda=best_lambda)
coef(best_model)
y_predicted=predict(model,s=best_lambda,newx=x)
sst=sum((y-mean(y))^2)
sse=sum((y_predicted-y)^2)
rsq=1-(sse/sst)
rsq


***************6)PCA************

df=read.csv("C:\\Users\\hp\\Desktop\\PGPDS\\Resume\\banknotes.csv")
n=nrow(df);n
p=ncol(df);n
cov=cov(df);cov
E=eigen(cov);E
eval=E$values;eval
## pca analysis on basis of covariance matrix
print("PC1=(0.04377427LENGTH)+(0.11216159LEFTHEIGHT)+(0.13919062RIGHTHEI
GHT)+(0.76830499LOWERMARGIN)+(0.20176610UPPERMARGIN)+(-
0.57890193DIAGONAL)") 
print("PC2=(0.01070966LENGTH)+(0.07144697LEFTHEIGHT)+(0.06628208RIGHTHEIG
HT)+(-0.56307225LOWERMARGIN)+(0.65928988UPPERMARGIN)+(-
0.48854255DIAGONAL)") 

## scree plot
plot(eval,pch="o",type="o",col="red",main="scree plot")

## explain 90% variation
prop=eval/sum(eval);prop
cumprob=cumsum(prop);cumprob
x=c()
for (i in 1:p)
{
if (cumprob[i]<=0.9)
{
x[i]=i
}
}
x
#hence we required 2 pc to explain 90% varaiton

## scor plot of first two pc
evec <- E$vectors
D=as.matrix(D);D
scor <- D %*% evec
scor
plot(scor[,1],scor[,2],xlab="1stpc",ylab="2nd pc",col=rep(c(3,4),each=100),pch=rep(c(15,16),each=100))

# Now you can use scores for further analysis or visualization
print(scores)
## important variables based on first two pc
which(evec[,1]==max(evec[,1]))
which(evec[,2]==max(evec[,2]))

## pca using correlation matrix of original dataset
corr=cor(D);corr
E=eigen(corr);E
eval=E$values;eval
evec=E$vectors;evec


### consider following matrix ex 2 by 2
# carreid out using covariance matrix
cov=matrix(c(1,4,4,100),byrow=TRUE,ncol=2)
cov
E=eigen(cov);E
x
eval=E$values;eval
evec=E$vectors;evec
PC1=evec[,1];PC1
pC2=evec[,2];pC2
plot(eval,type="o",pch=13,main="scree plot")
scor=cov%*%evec
plot(scor[,1],scor[,2],xlab="1stpc",ylab="2nd pc",main="scor plot")
prop=eval/sum(eval);prop

##carried out using correlation matrix
cor=cov2cor(cov)
E=eigen(cor)
eval=E$values
evec=E$vectors
PC1=evec[,1]
PC2=evec[,2]
prop=eval/sum(eval)
prop

******7)factor analysis**************

###Perform factor analysis and interpret the results.
C=matrix(c(1,0.02,0.96,0.42,0.01,0.02,1,0.13,0.71,0.85,0.96,0.13,1,0.5,0.11,0.42,0.71,0.5,1,
0.79,0.01,0.85,0.11,0.79,1),nrow=5,ncol=5,byrow=T) 
eval=eigen(C)$values;eval 
evec=eigen(C)$vectors;evec
plot(eval,type="o",pch=16,main="Scree plot") 
k=2 
p=ncol(C) 
Q=matrix(ncol=p,nrow = p) 
for(i in 1:p) 
{ 
Q[,i]=sqrt(eval[i])*evec[,i] 
} 
Q
Q1=Q[,1:k];Q1
cm=rowSums(Q1^2);cm ##Communalities
psi=C-Q1%*%t(Q1);psi
ps=diag(diag(psi));ps ##Estimated variance matrix of specific factors
plot(Q1,xlim=c(0,0.9),main = "loading plot") 

##2)a. Principal component method   b. Maximum likelihood method.
D=matrix(c(1,0.577,0.509,0.387,0.462,0.577,1,0.599,0.389,0.322,0.509,0.599,1,0.436,0.426
,0.387,0.389,0.436,1,0.523,0.462,0.322,0.426,0.523,1),nrow=5,ncol=5,byrow=TRUE) 
corr=cor(D);corr
eval=eigen(corr)$values;eval
evec=eigen(corr)$vectors;evec
p=ncol(D) 
Q=matrix(nrow=p,ncol=p) 
for(i in 1:p) 
{ 
 Q[,i]=sqrt(eval[i])*evec[,i] 
 } 
 Q ## Loading matrix
plot(eval,type="o",pch="o",main="Scree plot")
#In this scree plot elbow is at 3rd so that first two factors are most important. 
k=2 
Q1=Q[,1:k];Q1 ##loading matrix of specific factors
C=rowSums(Q1^2);C ##Communalities 
psi=corr-Q1%*%t(Q1);psi #variance matrix of specific factors 
ps=diag(diag(psi));ps #Estimated variance matrix of specific factors
q11=-1*Q1[,1] 
Q2=cbind(q11,Q1[,2]) 
Q2



******8)canonical correlation*****
##find sample canonical correlation
m=matrix(c(1,0.6328,0.2412,.....),nrow=4,byrow=TRUE)
m
sxx=m[1:2,1:2]
syy=m[3:4,3:4]
sxy=m[1:2,3:4]
eval=eigen(sxx)$values
lam=diag(sqrt(eval))
gamma=eigen(sxx)$vectors
s12=gamma%*%lam%*%t(gamma)
M1=solve(s12)%*%sxy%*%solve(syy)%*%syx%*%solve(s12)
e=eigen(M1)$values
cc=sqrt(e)
cc

*******9)Random sample from multivariate distribution***
##1)multivariate distribution with given p and mean vector and varaince covariance matrix
 install.packages(“MASS”) 
 library(MASS) 
 sample_size=5 
 sample_meanvector=c(10, 5) 
 sample_covariance_matrix=matrix(c(10, 5, 2, 9), ncol = 2) 
 # create bivariate normal distribution 
 sample=mvrnorm(n = sample_size, mu = sample_meanvector,Sigma = 
sample_covariance_matrix) 
 sample 

##by using choleskys method 
 install.packages("MASS") 
 library(MASS) 
 mu=matrix(c(1,2),nrow=2,ncol=1,byrow=) 
 sig=matrix(c(4,1,1,4),nrow=2,ncol=2,byrow=T) 
 c=chol(sig)
 sig=t(c)%*%c 

 p=2 
 n=15
zerovec=matrix(c(0,0),nrow=2,ncol=1,byrow=) 
unitvec=matrix(c(1,0,0,1),nrow=2,ncol=2,byrow=T) 
y=mvrnorm(n,zerovec,unitvec) 
dim(t(y)) 
a=c%*%t(y) 
dim(a) 
mu2=matrix(c(rep(0,13)),nrow=13,ncol=1,byrow=T) 
r=rbind(mu,mu2) 
mu3=matrix(c(rep(0,15)),nrow=15,ncol=1,byrow=T) 
r1=cbind(r,mu3) 
dim(r1) 
x=t(a)+r1 
x
*****10) other examples*****
y11=c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68) 
y12=c(9.14,8.14,8.74,8.77,9.26,8.10,6.13,3.10,9.13,7.26,4.74) 
y13=c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73) 
x=c(10,8,13,9,11,14,6,4,12,7,7) 
x_=c(8,8,8,8,8,8,8,19,8,8,8) 
y_=c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.50,5.56,7.91,6.89) 

##Regression line of y11 on x 

m1=lm(y11~x)
summary(m1) 
plot(y11,x)

## Regression line of Y12 onX 
m2=lm(y12~x) 
summary(m2)
plot(y12,x)

##Regression line of Y13 on X
m3=lm(y13~x)
summary(m3)
plot(y13,x)

##Regression line of Y’ on X’
m4=lm(y_~x_)
summary(m4) 
plot(y_,x_)


##2) quetion
data=read.csv("C:\\Users\\Dell\\Desktop\\data for scatter plot.csv") 
data 
y=data$Year 
I=data$Inhabitants 
U=data$unemployed 
boxplot(I)
library(pracma) 
A=as.matrix(data[,2:3]) 
A 
f=y 
install.packages("andrewsplot")
library(andrewsplot)
andrewsplot(A, f, style = "pol", scaled = FALSE, npts = 10)
hist(I)
 
