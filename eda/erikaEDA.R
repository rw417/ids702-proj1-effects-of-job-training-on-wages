library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
library(gridExtra)
library(texreg)


data<-read.table("lalondedata.txt",header=T,sep = ",", dec = ".")

data$age<-as.numeric(data$age)
data$black<-factor(data$black)
data$nodegree<-factor(data$nodegree)
data$hispan<-factor(data$hispan)
data$married<-factor(data$married)


data$treat<-factor(data$treat)
data$re74<-as.numeric(data$re74)
data$re75<-as.numeric(data$re75)
data$re78<-as.numeric(data$re78)

data$positive74_fac <- factor(ifelse(data$re74>0, 'positive', 'not positive'))
data$positive74 <- as.numeric(data$positive74_fac)
data$positive74 <- ifelse(data$positive74 == 2, 1, 0)
  
data$positive75_fac <- factor(ifelse(data$re75>0, 'positive', 'not positive'))
data$positive75 <- as.numeric(data$positive75_fac)
data$positive75 <- ifelse(data$positive75 == 2, 1, 0)

data$positive78_fac <- factor(ifelse(data$re78>0, 'positive', 'not positive'))
data$positive78 <- as.numeric(data$positive78_fac)
data$positive78 <- ifelse(data$positive78 == 2, 1, 0)

#no facet wrap
ggplot(data,aes(x=treat,y=re74,fill=treat))+geom_boxplot()+labs(title= "training vs income 74",x="treat",y="income '74")+theme_classic()+theme(legend.position = "none")
ggplot(data,aes(x=treat,y=re75,fill=treat))+geom_boxplot()+labs(title= "training vs income 75",x="treat",y="income '75")+theme_classic()+theme(legend.position = "none")
ggplot(data,aes(x=treat,y=re78,fill=treat))+geom_boxplot()+labs(title= "training vs income 78",x="treat",y="income '78")+theme_classic()+theme(legend.position = "none")

#age
ggplot(data,aes(x=treat,y=re74,fill=treat))+geom_boxplot()+labs(title= "training vs income 74, age",x="treat",y="income '74")+theme_classic()+theme(legend.position = "none")+facet_wrap(~age)
ggplot(data,aes(x=treat,y=re75,fill=treat))+geom_boxplot()+labs(title= "training vs income 75, age",x="treat",y="income '75")+theme_classic()+theme(legend.position = "none")+facet_wrap(~age)
ggplot(data,aes(x=treat,y=re78,fill=treat))+geom_boxplot()+labs(title= "training vs income 78, age",x="treat",y="income '78")+theme_classic()+theme(legend.position = "none")+facet_wrap(~age)


#black
ggplot(data,aes(x=treat,y=re74,fill=treat))+geom_boxplot()+labs(title= "training vs income 74, black",x="treat",y="income '74")+theme_classic()+theme(legend.position = "none")+facet_wrap(~black)
ggplot(data,aes(x=treat,y=re75,fill=treat))+geom_boxplot()+labs(title= "training vs income 75, black",x="treat",y="income '75")+theme_classic()+theme(legend.position = "none")+facet_wrap(~black)
ggplot(data,aes(x=treat,y=re78,fill=treat))+geom_boxplot()+labs(title= "training vs income 78, black",x="treat",y="income '78")+theme_classic()+theme(legend.position = "none")+facet_wrap(~black)

#hispan
ggplot(data,aes(x=treat,y=re74,fill=treat))+geom_boxplot()+labs(title= "training vs income 74, hispan",x="treat",y="income '74")+theme_classic()+theme(legend.position = "none")+facet_wrap(~hispan)
ggplot(data,aes(x=treat,y=re75,fill=treat))+geom_boxplot()+labs(title= "training vs income 75, hispan",x="treat",y="income '75")+theme_classic()+theme(legend.position = "none")+facet_wrap(~hispan)
ggplot(data,aes(x=treat,y=re78,fill=treat))+geom_boxplot()+labs(title= "training vs income 78, hispan",x="treat",y="income '78")+theme_classic()+theme(legend.position = "none")+facet_wrap(~hispan)

#no degree
ggplot(data,aes(x=treat,y=re74,fill=treat))+geom_boxplot()+labs(title= "training vs income 74, nodegree",x="treat",y="income '74")+theme_classic()+theme(legend.position = "none")+facet_wrap(~nodegree)
ggplot(data,aes(x=treat,y=re75,fill=treat))+geom_boxplot()+labs(title= "training vs income 75, nodegree",x="treat",y="income '75")+theme_classic()+theme(legend.position = "none")+facet_wrap(~nodegree)
ggplot(data,aes(x=treat,y=re78,fill=treat))+geom_boxplot()+labs(title= "training vs income 78, nodegree",x="treat",y="income '78")+theme_classic()+theme(legend.position = "none")+facet_wrap(~nodegree)

#married
ggplot(data,aes(x=treat,y=re74,fill=treat))+geom_boxplot()+labs(title= "training vs income 74, married",x="treat",y="income '74")+theme_classic()+theme(legend.position = "none")+facet_wrap(~married)
ggplot(data,aes(x=treat,y=re75,fill=treat))+geom_boxplot()+labs(title= "training vs income 75, married",x="treat",y="income '75")+theme_classic()+theme(legend.position = "none")+facet_wrap(~married)
ggplot(data,aes(x=treat,y=re78,fill=treat))+geom_boxplot()+labs(title= "training vs income 78, married",x="treat",y="income '78")+theme_classic()+theme(legend.position = "none")+facet_wrap(~married)


tapply(data$positive74_fac,data$treat,function(x) table(x)/sum(table(x)))
tapply(data$positive75_fac,data$treat,function(x) table(x)/sum(table(x)))
tapply(data$positive78_fac,data$treat,function(x) table(x)/sum(table(x)))
