#Classification Space
col1=rep(seq(-3,3,.1),61)
col2=rep(seq(-3,3,.1),each=61)
StProbeX=as.data.frame(cbind(col1,col2))
WeightTestX=seq(4,18,.1)
WeightTestX=rep(WeightTestX,38)
WhiskerTestY=rep(seq(1,4.7,.1),each=141)
x=seq(4,8.1,.1)
y=rep(4.8,42)
WeightTestX=c(WeightTestX,x)
WhiskerTestY=c(WhiskerTestY,y)
ProbeX=cbind(WeightTestX,WhiskerTestY)
ProbeGlm =function (ProbeX,ProbeYhat,InX,InY,xr=c(-3,3),yr=c(-3,3),pts=0.8,fld=1) 
  {                                                       #updated 8-17-19, 2:49 PM
    plot(ProbeX,                           
         cex=fld,                                    #"fld" controls size of space-points
         pch=3,                                      #"pts" controls size of data-points
         col=c(c(ProbeYhat) + 2),        #added additional c( ) to handle factor var's
         xlim=xr,
         ylim=yr)
    par(new=TRUE)
    plot(InX,
         ann=FALSE,
         pch=c(InY + 15),
         col=c(InY + 2),
         xlim=xr,
         ylim=yr,
         cex=pts)
  }

#Fitting Classification Trees
#The tree graphs that take titles well, so my name couldn't be placed on those but all of the other graphs have my name.
library(tree)
library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
tree.carseats=tree(as.factor(High)~.-Sales,Carseats)
#Had to change High to a factor data type, they didn't have to do that at the time of the ISLR example using the tree package.
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
set.seed (2)
train=sample (1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(as.factor(High)~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(104+50)/200
#A little better than the examples.
set.seed (3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow =c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b",main = "Anil Poonai")
plot(cv.carseats$k,cv.carseats$dev,type="b",main = "Anil Poonai")
prune.carseats =prune.misclass(tree.carseats,best =9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(97+58)/200
#A little better again.
prune.carseats=prune.misclass(tree.carseats,best =15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(102+53)/200
#Upgraded their function a bit didn't they.
#Fitting Restression Trees
library(MASS)
set.seed(1)
train=sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b',main = "Anil Poonai")
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty =0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test,main = "Anil Poonai")
abline(0,1)
mean((yhat-boston.test)^2)
#Higher than what they got.

#AOS Classification
library(tidyverse)
AOS <- read_csv("C:/Users/poona/Desktop/School/AOS.csv")
tree.AOS=tree(as.factor(HiLoRisk)~LogR1+LogR2+LogR3,AOS)
summary(tree.AOS)
#Single Node and an error rate of 49.75%. Not too promising so far.
#Can't plot single Nodes.
set.seed(4)
train=sample(3648,2000)
AOS.test=AOS[-train,]
tree.AOS=tree(as.factor(HiLoRisk)~LogR1+LogR2+LogR3,AOS,subset = train)
tree.pred=predict(tree.AOS,AOS.test,type="class")
table(tree.pred,AOS$HiLoRisk[-train])
865/1648
#52.49 success rate
cv.AOS=cv.tree(tree.AOS,FUN = prune.misclass)
#can't cross validate as it is single noded.
#AOS Regression
tree.AOS=tree(`Adj Close`~LogR1+LogR2+LogR3,data=AOS,subset=train)
summary(tree.AOS)
plot(tree.AOS)
text(tree.AOS,pretty=0)
cv.AOS=cv.tree(tree.AOS)
plot(cv.AOS$size,cv.AOS$dev,type='b')
prune.AOS=prune.tree(tree.AOS,best=3)
plot(prune.AOS)
text(prune.AOS,pretty=0)
yhat=predict(tree.AOS,newdata=AOS[-train,])
AOS.test=AOS$`Adj Close`[-train]
plot(yhat,AOS.test)
abline(0,1)
mean((yhat-AOS.test)^2)
#Space
AOS$HiLoRisk=ifelse(AOS$HiLoRisk=='Present',1,0)
glm.fit=glm(HiLoRisk~LogR1+LogR3,data=AOS,family=binomial)
newdata=ProbeX
X=AOS[,c(17,19)]
StdX=apply(X,2,scale)
dfX=as.data.frame(StdX)
glm.probs=predict(glm.fit,newdata=dfX,type="response")
StCard2=as.data.frame(cbind(dfX,AOS$HiLoRisk))
glm.fit2=glm(AOS$HiLoRisk~LogR1+LogR3,data=StCard2,family=binomial)
names(StProbeX)[1]="LogR1"
names(StProbeX)[2]="LogR3"
glm.probe=predict(glm.fit2,newdata=StProbeX,type="response")
glm.y=glm.probe
glm.y[glm.probe>.5]=1
glm.y[glm.probe<.5]=0
ProbeGlm(ProbeX=StProbeX,ProbeYhat = c(glm.y),InX = dfX,InY = AOS$HiLoRisk,xr=c(-3,3),yr=c(-3,3))

#Cardiac Classification
Cardiac <- read_csv("C:/Users/poona/Downloads/Cardiac.csv")
Dead=ifelse(Cardiac$death==0,"Yes","No")
Cardiac=data.frame(Cardiac,Dead)
tree.Cardiac=tree(as.factor(Dead)~.-death,data=Cardiac)
summary(tree.Cardiac)
#5 Nodes and .4% error rate
plot(tree.Cardiac)
text(tree.Cardiac,pretty=0)
set.seed(5)
train=sample(558,558/2)
Cardiac.test=Cardiac[-train,]
tree.Cardiac=tree(as.factor(Dead)~.-death,data=Cardiac,subset=train)
tree.pred=predict(tree.Cardiac,Cardiac.test,type="class")
table(tree.pred,Cardiac$Dead[-train])
271/279
#97.13% success rate
cv.Cardiac=cv.tree(tree.Cardiac,FUN=prune.misclass)
names(cv.Cardiac)
par(mfrow=c(1,2))
plot(cv.Cardiac$size,cv.Cardiac$dev,type="b")
plot(cv.Cardiac$k,cv.Cardiac$dev,type="b")
prune.Cardiac=prune.misclass(tree.Cardiac,best=4)
plot(prune.Cardiac)
text(prune.Cardiac,pretty=0)
tree.pred=predict(prune.Cardiac,Cardiac.test,type="class")
table(tree.pred,Cardiac$Dead[-train])
271/279
#No difference
prune.Cardiac=prune.misclass(tree.Cardiac,best=5)
#Best can't be any bigger.
#Cardiac Regression
tree.Cardiac=tree(sbp~-sbp,data=Cardiac,subset=train)
#I'm using sdp due to a study that heavily correlates systolic blood pressure with death.
summary(tree.Cardiac)
#Single Noded so graphs won't work, neither will prune
yhat=predict(tree.Cardiac,newdata=Cardiac[-train,])
Cardiac.test=Cardiac$sbp[-train]
plot(yhat,Cardiac.test)
abline(0,1)
mean((yhat-Cardiac.test)^2)
#.01631906
#Space
Cardiac <- read_csv("C:/Users/poona/Downloads/Cardiac.csv")
Dead=ifelse(Cardiac$death==0,"Yes","No")
Cardiac$Dead=ifelse(Cardiac$death==0,1,0)
glm.fit=glm(Dead~sbp+bhr,data=Cardiac,family=binomial)
newdata=ProbeX
View(Cardiac)
X=Cardiac[,c(1,5)]
StdX=apply(X,2,scale)
dfX=as.data.frame(StdX)
glm.probs=predict(glm.fit,newdata=dfX,type="response")
StCard2=as.data.frame(cbind(dfX,Cardiac$Dead))
glm.fit2=glm(Cardiac$Dead~sbp+bhr,data=StCard2,family=binomial)
names(StProbeX)[1]="bhr"
names(StProbeX)[2]="sbp"
glm.probe=predict(glm.fit2,newdata=StProbeX,type="response")
glm.y=glm.probe
glm.y[glm.probe>.5]=1
glm.y[glm.probe<.5]=0
ProbeGlm(ProbeX=StProbeX,ProbeYhat = c(glm.y),InX = dfX,InY = Cardiac$Dead,xr=c(-3,3),yr=c(-3,3))
