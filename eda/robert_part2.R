####
# Part II ####
####
lalondedata["positivere78_int"] <- rep(0,nrow(lalondedata))
lalondedata$positivere78_int[lalondedata$re78 > 0] <- 1
lalondedata$positivere78 <- as.factor(lalondedata$positivere78_int)


##
## Predictors ####
##

# age # no effects
ggplot(data=lalondedata, aes(x=positivere78, y=age)) + geom_boxplot()

# educ # has effects
ggplot(data=lalondedata, aes(x=positivere78, y=educ)) + geom_boxplot()

# re74 # has effects
ggplot(data=lalondedata, aes(x=positivere78, y=re74)) + geom_boxplot()

# treat # no effects?
chisq.test(lalondedata$positivere78, lalondedata$treat)
round(apply(table(lalondedata[,c("positivere78","treat")])/sum(table(lalondedata[,c("positivere78","treat")])),
            2,function(x) x/sum(x)),2)


# black # huge effects
chisq.test(lalondedata$positivere78, lalondedata$black)
round(apply(table(lalondedata[,c("positivere78","black")])/sum(table(lalondedata[,c("positivere78","black")])),
            2,function(x) x/sum(x)),2)

# hispan # no effects
chisq.test(lalondedata$positivere78, lalondedata$hispan)
round(apply(table(lalondedata[,c("positivere78","hispan")])/sum(table(lalondedata[,c("positivere78","hispan")])),
            2,function(x) x/sum(x)),2)

# married # no effects
chisq.test(lalondedata$positivere78, lalondedata$married)
round(apply(table(lalondedata[,c("positivere78","married")])/sum(table(lalondedata[,c("positivere78","married")])),
            2,function(x) x/sum(x)),2)

# nodegree # no effects
chisq.test(lalondedata$positivere78, lalondedata$nodegree)
round(apply(table(lalondedata[,c("positivere78","nodegree")])/sum(table(lalondedata[,c("positivere78","nodegree")])),
            2,function(x) x/sum(x)),2)


##
## Interactions ####
##

# re74 
# treat # if you didn't get treated, then re74 has an effect???
ggplot(data=lalondedata, aes(x=positivere78, y=re74)) + geom_boxplot() +
  facet_wrap(~treat)
# black # has an effect if you're not black
ggplot(data=lalondedata, aes(x=positivere78, y=re74)) + geom_boxplot() +
  facet_wrap(~black)
# hispan # no effect
ggplot(data=lalondedata, aes(x=positivere78, y=re74)) + geom_boxplot() +
  facet_wrap(~hispan)
# married # has an effect is you're married
ggplot(data=lalondedata, aes(x=positivere78, y=re74)) + geom_boxplot() +
  facet_wrap(~married)
# nodegree # no effect
ggplot(data=lalondedata, aes(x=positivere78, y=re74)) + geom_boxplot() +
  facet_wrap(~nodegree)

# age
# treat # some effects
ggplot(data=lalondedata, aes(x=positivere78, y=age)) + geom_boxplot() +
  facet_wrap(~treat)
# black # no effects
ggplot(data=lalondedata, aes(x=positivere78, y=age)) + geom_boxplot() +
  facet_wrap(~black)
# hispan # some effect if you're hispanic then your age is correlated with positive wage
ggplot(data=lalondedata, aes(x=positivere78, y=age)) + geom_boxplot() +
  facet_wrap(~hispan)
# married # hard to tell
ggplot(data=lalondedata, aes(x=positivere78, y=age)) + geom_boxplot() +
  facet_wrap(~married)
# nodegree # some effect
ggplot(data=lalondedata, aes(x=positivere78, y=age)) + geom_boxplot() +
  facet_wrap(~nodegree)

# educ
# treat # some effects
ggplot(data=lalondedata, aes(x=positivere78, y=educ)) + geom_boxplot() +
  facet_wrap(~treat)
# black # some effects
ggplot(data=lalondedata, aes(x=positivere78, y=educ)) + geom_boxplot() +
  facet_wrap(~black)
# hispan # no effect
ggplot(data=lalondedata, aes(x=positivere78, y=educ)) + geom_boxplot() +
  facet_wrap(~hispan)
# married # some effects
ggplot(data=lalondedata, aes(x=positivere78, y=educ)) + geom_boxplot() +
  facet_wrap(~married)

# treat
# black # black benefit less from treat
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$black==1)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$black==1)])
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$black==0)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$black==0)])

# hispanic # hispanic benefit more from treat
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$hispan==1)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$hispan==1)])
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$hispan==0)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$hispan==0)])

# married # married benefit more from treat; non-married worse off from treat
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$married==1)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$married==1)])
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$married==0)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$married==0)])

# nodegree
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$nodegree==1)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$nodegree==1)])
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$nodegree==0)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$nodegree==0)])

# zero income in re74
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$re74==0)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$re74==0)])
mean(lalondedata$positivere78_int[(lalondedata$treat==1) & (lalondedata$re74>0)])
mean(lalondedata$positivere78_int[(lalondedata$treat==0) & (lalondedata$re74>0)])

nrow(lalondedata[(lalondedata$treat==1) & (lalondedata$re74==0),])
nrow(lalondedata[(lalondedata$treat==1) & (lalondedata$re74>0),])
nrow(lalondedata[(lalondedata$treat==1) & (lalondedata$re74==0) & (lalondedata$re78==0),])
nrow(lalondedata[(lalondedata$treat==1) & (lalondedata$re74==0) & (lalondedata$re78>0),])
nrow(lalondedata[(lalondedata$treat==1) & (lalondedata$re74>0) & (lalondedata$re78==0),])
nrow(lalondedata[(lalondedata$treat==1) & (lalondedata$re74>0) & (lalondedata$re78>0),])
nrow(lalondedata[(lalondedata$treat==0) & (lalondedata$re74==0),])
nrow(lalondedata[(lalondedata$treat==0) & (lalondedata$re74>0),])

#
### Binned predictor vs response ####
#
binnedplot(x=lalondedata$age, y=lalondedata$positivere78_int,
           xlab="Age",ylab ="Positive",
           ylim=c(0,1),col.pts="navy",main="Binned Age vs Positive Income",
           col.int="white")
binnedplot(x=lalondedata$educc, y=lalondedata$positivere78_int,
           xlab="Educ",ylab ="Positive",
           ylim=c(0,1),col.pts="navy",main="Binned Educ vs Positive Income",
           col.int="white")
binnedplot(x=lalondedata$re74c, y=lalondedata$positivere78_int,
           xlab="re74c",ylab ="Positive",
           ylim=c(0,1),col.pts="navy",main="Binned re74 vs Positive Income",
           col.int="white")

##
## Modeling ####
##
logit1 <- glm(positivere78 ~ treat + age + educ + black + hispan + married + re74, data=lalondedata, family="binomial" )
summary(logit1)

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(logit1) >= mean(lalondedata$positivere78_int), "1","0")),
                            as.factor(lalondedata$positivere78_int),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

invisible(roc(lalondedata$positivere78_int,fitted(logit1),plot=T,print.thres="best",legacy.axes=T,
              print.auc =T,col="red3"))

# centering
lalondedata["agec"] <- lalondedata$age - mean(lalondedata$age)
lalondedata["educc"] <- lalondedata$educ - mean(lalondedata$educ)
lalondedata["re74c"] <- lalondedata$re74 - mean(lalondedata$re74)

logit1c <- glm(positivere78 ~ treat + agec + educc + black + hispan + married + re74c, data=lalondedata, family="binomial" )
summary(logit1c)

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(logit1c) >= mean(lalondedata$positivere78_int), "1","0")),
                            as.factor(lalondedata$positivere78_int),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
invisible(roc(lalondedata$positivere78_int,fitted(logit1c),plot=T,print.thres="best",legacy.axes=T,
              print.auc =T,col="red3"))

# diagnostics
logit1res <- residuals(logit1c, "resp")

binnedplot(x=fitted(logit1c),y=logit1res,xlab="Fitted Values",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Fitted Values",col.pts="navy")

binnedplot(x=lalondedata$agec,y=logit1res,xlab="Age",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Age",col.pts="navy")
binnedplot(x=lalondedata$educc,y=logit1res,xlab="Educ",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Educ",col.pts="navy")
binnedplot(x=lalondedata$re74c,y=logit1res,xlab="RE74",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs RE74",col.pts="navy")

# model2
lalondedata["agec2"] <- lalondedata$agec^2
logit2 <- glm(positivere78 ~ treat + age + agec2 + educ + black + hispan + married + re74, data=lalondedata, family="binomial" )
summary(logit2)

# diagnostics
logit2res <- residuals(logit2, "resp")

binnedplot(x=fitted(logit2),y=logit2res,xlab="Fitted Values",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Fitted Values",col.pts="navy")

binnedplot(x=lalondedata$agec,y=logit2res,xlab="Age",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Age",col.pts="navy")
binnedplot(x=lalondedata$educc,y=logit2res,xlab="Educ",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Educ",col.pts="navy")
binnedplot(x=lalondedata$re74c,y=logit2res,xlab="RE74",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs RE74",col.pts="navy")

# model3
lalondedata["logage"] <- log(lalondedata$age)
logit3 <- glm(positivere78 ~ treat + logage + educ + black + hispan + married + re74, data=lalondedata, family="binomial" )
summary(logit3)

binnedplot(x=lalondedata$logage,y=logit2res,xlab="log(Age)",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Age",col.pts="navy")

# model4
lalondedata["ageCat"] <- ifelse(lalondedata$age < 40, 0, 1)
logit4 <- glm(positivere78 ~ treat + ageCat + educ + black + hispan + married + re74, data=lalondedata, family="binomial" )
summary(logit4)

par(mfrow=c(2,1))
invisible(roc(lalondedata$positivere78_int,fitted(logit1c),plot=T,print.thres="best",legacy.axes=T,
              print.auc =T,col="red3"))
invisible(roc(lalondedata$positivere78_int,fitted(logit3),plot=T,print.thres="best",legacy.axes=T,
              print.auc =T,col="red3"))

# use logit1c - age centered around the mean




#
### Interactions ####
#
logit5 <- glm(positivere78 ~ treat + agec + educc + black + hispan + married + re74c +
                treat:re74c + black:re74c + re74c:married + agec:treat + nodegree:agec + 
                educc:treat + educc:black + educc:married + married:treat + black:treat , family = "binomial", data = lalondedata)
summary(logit5)

exp(7.346e-02)

invisible(roc(lalondedata$positivere78_int,fitted(logit5),plot=T,print.thres="best",legacy.axes=T,
              print.auc =T,col="red3"))
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(logit5) >= mean(lalondedata$positivere78_int), "1","0")),
                            as.factor(lalondedata$positivere78_int),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

par(mfrow=c(1,1))
