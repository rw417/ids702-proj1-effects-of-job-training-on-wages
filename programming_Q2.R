knitr::opts_chunk$set(echo = TRUE)
# Load packages
library(ggplot2)
library(tidyverse)
library(arm)
library(pROC)
library(rms)
library(e1071)
library(caret)
require(gridExtra)
library(leaps)

# Import data
jobsl <- read.csv("Data/lalondedata.txt")

### Prepare data for analysis
# Assign factor variables:
jobsl$treat <- factor(jobsl$treat, levels = c(0,1), labels = c('no training', 'training'))
jobsl$black <- factor(jobsl$black, levels = c(0,1), labels = c('not black', 'black'))
jobsl$hispan <- factor(jobsl$hispan, levels = c(0,1), labels = c('not hispanic', 'hispanic'))
jobsl$married <- factor(jobsl$married, levels = c(0,1), labels = c('not married', 'married'))
jobsl$nodegree <- factor(jobsl$nodegree, levels = c(0,1), labels = c('Degree', 'No Degree'))

# Create zero income variable based on income.
jobsl$zero <- as.factor(ifelse(jobsl$re74 == 0, 'zero', 'not zero'))

# Create response variable
jobsl$positive <- ifelse(jobsl$re78 == 0, 0, 1)
jobsl$positive_c <- as.factor(ifelse(jobsl$re78 == 0, 'zero', 'positive'))

# Remove irrelevant variables
jobsl <- dplyr::select(jobsl, -X, -re75)

### EDA
# First, examining the dataset
dim(jobsl)
summary(jobsl)
str(jobsl)
head(jobsl)

## Plotting discrete predictor variables
# Age
ggplot(data=jobsl, aes(x=positive_c, y=age)) + geom_boxplot()
binnedplot(x=jobsl$age, y=jobsl$positive,
           xlab="Age",ylab ="Positive Income?",
           ylim=c(0,1),col.pts="navy",main="Binned Age vs Positive Income",
           col.int="white")

# Education
ggplot(data=jobsl, aes(x=positive_c, y=educ)) + geom_boxplot()
plot(0:18, tapply(jobsl$positive, jobsl$educ, mean), col = 'blue4', pch = 10) 
abline(h=0.7, col = "red3")
binnedplot(x=jobsl$educ, y=jobsl$positive,
           xlab="Education",ylab ="Positive Income?",
           ylim=c(0,1),col.pts="navy",main="Binned Education vs Positive Income",
           col.int="white")

# Noticed that education seems to be different below 8 and below 
jobsl$newed <- ifelse(jobsl$educ >= 9, 1, 0)
jobsl$newed <- factor(jobsl$newed, levels = c(0,1), labels = c('less than 9', '9 or more'))

# Income in 1974
ggplot(data=jobsl, aes(x=positive_c, y=re74)) + geom_boxplot()
binnedplot(x=jobsl$re74, y=jobsl$positive,
           xlab="Income in 1974",ylab ="Positive Income?",
           ylim=c(0,1),col.pts="navy",main="Binned Income in 1974 vs Positive Income",
           col.int="white")

## Tables for continuous variables
# Treatment
chisq.test(jobsl$positive_c, jobsl$treat)
round(apply(table(jobsl[,c("positive_c","treat")])/sum(table(jobsl[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)

# Black
chisq.test(jobsl$positive_c, jobsl$black)
round(apply(table(jobsl[,c("positive_c","black")])/sum(table(jobsl[,c("positive_c","black")])),
            2,function(x) x/sum(x)),2)

# Hispanic
chisq.test(jobsl$positive_c, jobsl$hispan)
round(apply(table(jobsl[,c("positive_c","hispan")])/sum(table(jobsl[,c("positive_c","hispan")])),
            2,function(x) x/sum(x)),2)

# Married
chisq.test(jobsl$positive_c, jobsl$married)
round(apply(table(jobsl[,c("positive_c","married")])/sum(table(jobsl[,c("positive_c","married")])),
            2,function(x) x/sum(x)),2)

# No Degree
chisq.test(jobsl$positive_c, jobsl$nodegree)
round(apply(table(jobsl[,c("positive_c","nodegree")])/sum(table(jobsl[,c("positive_c","nodegree")])),
            2,function(x) x/sum(x)),2)

# New Education variable
chisq.test(jobsl$positive_c, jobsl$newed)
round(apply(table(jobsl[,c("positive_c","newed")])/sum(table(jobsl[,c("positive_c","newed")])),
            2,function(x) x/sum(x)),2)

# Zero income
chisq.test(jobsl$positive_c, jobsl$zero)
round(apply(table(jobsl[,c("positive_c","zero")])/sum(table(jobsl[,c("positive_c","zero")])),
            2,function(x) x/sum(x)),2)

## Interactions between discrete and predictor variables
# Income in 1974 and treatment
ggplot(data=jobsl, aes(x=positive_c, y=re74)) + geom_boxplot() +
  facet_wrap(~treat)
# Income in 1974 and Black
ggplot(data=jobsl, aes(x=positive_c, y=re74)) + geom_boxplot() +
  facet_wrap(~black)
# Income in 1974 and Hispanic
ggplot(data=jobsl, aes(x=positive_c, y=re74)) + geom_boxplot() +
  facet_wrap(~hispan)
# Income in 1974 and Married
ggplot(data=jobsl, aes(x=positive_c, y=re74)) + geom_boxplot() +
  facet_wrap(~married)
# Income in 1974 and No Degree
ggplot(data=jobsl, aes(x=positive_c, y=re74)) + geom_boxplot() +
  facet_wrap(~nodegree)
# Income in 1974 and New Education variable
ggplot(data=jobsl, aes(x=positive_c, y=re74)) + geom_boxplot() +
  facet_wrap(~newed)


# Age and treatment
ggplot(data=jobsl, aes(x=positive_c, y=age)) + geom_boxplot() +
  facet_wrap(~treat)
# Age and Black
ggplot(data=jobsl, aes(x=positive_c, y=age)) + geom_boxplot() +
  facet_wrap(~black)
# Age and Hispanic
ggplot(data=jobsl, aes(x=positive_c, y=age)) + geom_boxplot() +
  facet_wrap(~hispan)
# Age and Married
ggplot(data=jobsl, aes(x=positive_c, y=age)) + geom_boxplot() +
  facet_wrap(~married)
# Age and No Degree
ggplot(data=jobsl, aes(x=positive_c, y=age)) + geom_boxplot() +
  facet_wrap(~nodegree)
# Age and Zero
ggplot(data=jobsl, aes(x=positive_c, y=age)) + geom_boxplot() +
  facet_wrap(~zero)
# Age and New Education variable
ggplot(data=jobsl, aes(x=positive_c, y=re74)) + geom_boxplot() +
  facet_wrap(~newed)

# Education and treatment
ggplot(data=jobsl, aes(x=positive_c, y=educ)) + geom_boxplot() +
  facet_wrap(~treat)
# Education and Black
ggplot(data=jobsl, aes(x=positive_c, y=educ)) + geom_boxplot() +
  facet_wrap(~black)
# Education and Hispanic
ggplot(data=jobsl, aes(x=positive_c, y=educ)) + geom_boxplot() +
  facet_wrap(~hispan)
# Education and Married
ggplot(data=jobsl, aes(x=positive_c, y=educ)) + geom_boxplot() +
  facet_wrap(~married)
# Education and No Degree
ggplot(data=jobsl, aes(x=positive_c, y=educ)) + geom_boxplot() +
  facet_wrap(~nodegree)
# Education and Zero
ggplot(data=jobsl, aes(x=positive_c, y=educ)) + geom_boxplot() +
  facet_wrap(~zero)


## Interactions between categorical variables
# Treat and black
j_black = jobsl[which(jobsl$black == 'black'),]
j_nblack = jobsl[which(jobsl$black == 'not black'),]
round(apply(table(j_black[,c("positive_c","treat")])/sum(table(j_black[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)
round(apply(table(j_nblack[,c("positive_c","treat")])/sum(table(j_nblack[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)

# Treat and hispanic
j_hispan = jobsl[which(jobsl$hispan == 'hispanic'),]
j_nhispan = jobsl[which(jobsl$hispan == 'not hispanic'),]
round(apply(table(j_hispan[,c("positive_c","treat")])/sum(table(j_hispan[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)
round(apply(table(j_nhispan[,c("positive_c","treat")])/sum(table(j_nhispan[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)

# Treat and married
j_married = jobsl[which(jobsl$married == 'married'),]
j_nmarried = jobsl[which(jobsl$married == 'not married'),]
round(apply(table(j_married[,c("positive_c","treat")])/sum(table(j_married[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)
round(apply(table(j_nmarried[,c("positive_c","treat")])/sum(table(j_nmarried[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)

# Treat and no degree
j_deg = jobsl[which(jobsl$nodegree == 'Degree'),]
j_ndeg = jobsl[which(jobsl$nodegree == 'No Degree'),]
round(apply(table(j_deg[,c("positive_c","treat")])/sum(table(j_deg[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)
round(apply(table(j_ndeg[,c("positive_c","treat")])/sum(table(j_ndeg[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)

# Treat and new education variable
j_ed = jobsl[which(jobsl$newed == 'less than 9'),]
j_ned = jobsl[which(jobsl$newed == '9 or more'),]
round(apply(table(j_ed[,c("positive_c","treat")])/sum(table(j_ed[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)
round(apply(table(j_ned[,c("positive_c","treat")])/sum(table(j_ned[,c("positive_c","treat")])),
            2,function(x) x/sum(x)),2)


### Modeling
# Center discrete variables
jobsl$agec <- jobsl$age - mean(jobsl$age)
jobsl$educc <- jobsl$educ - mean(jobsl$educ)
jobsl$re74c <- jobsl$re74 - mean(jobsl$re74)



# Fit full model and null model
logitfull <- glm(positive ~ treat*(agec + educc + black + hispan + married +
                                  re74c + zero + newed) + black*re74c 
              +re74c*married + educc*black + educc*married, 
              data=jobsl, family="binomial")
logitnull <- glm(positive ~ treat, data = jobsl, family = "binomial")

# Evaluate full model
logitfres <- residuals(logitfull, "resp")
binnedplot(x=fitted(logitfull),y=logitfres,xlab="Fitted Values",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Fitted Values",col.pts="navy")
binnedplot(x=jobsl$agec,y=logitfres,xlab="Age",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Age",col.pts="navy")
binnedplot(x=jobsl$educc,y=logitfres,xlab="Educ",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Educ",col.pts="navy")
binnedplot(x=jobsl$re74c,y=logitfres,xlab="RE74",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs RE74",col.pts="navy")
# Perform selection
n <- nrow(jobsl)
aic_step <- step(logitnull, scope = list(upper = logitfull, lower = logitnull), direction = "both", trace = 0)
aic_for <- step(logitnull, scope = list(upper = logitfull, lower = logitnull), direction = "forward", trace = 0)
aic_back <- step(logitfull, scope = list(upper = logitfull, lower = logitnull), direction = "both", trace = 0)
bic_step <- step(logitnull, scope = list(upper = logitfull, lower = logitnull), direction = "both", trace = 0, k = log(n))
bic_for <- step(logitnull, scope = list(upper = logitfull, lower = logitnull), direction = "forward", trace = 0, k = log(n))
bic_back <- step(logitfull, scope = list(upper = logitfull, lower = logitnull), direction = "both", trace = 0, k = log(n))

anova(aic_step, bic_step, test = 'Chisq')
anova(aic_back, aic_step, test = 'Chisq')

# Removed hispanic terms from aic_back
no_hisp <- glm(positive ~ treat + agec + educc + black +
                 re74c + zero + newed + treat:agec + treat:zero, 
               family = "binomial", data = jobsl)
anova(no_hisp, aic_back, test = "Chisq")

# Moved forward with model without hisapnic

# Choosing between final models
invisible(roc(jobsl$positive,fitted(no_hisp),plot=T,legacy.axes=T, print.thres = 'best', col="red3"))
invisible(roc(jobsl$positive,fitted(bic_step),plot=T,legacy.axes=T, col="blue3",add=T))
invisible(roc(jobsl$positive,fitted((aic_step)),plot=T,legacy.axes=T, print.auc = T, print.thres = 'best', col="green3",add=T))
legend('bottomright', c('No Hispanic Terms', 'BIC Stepwise', 'AIC Forward'),lty=c(1,1),
       lwd=c(2,2),col=c('red3', 'blue3', 'green3'))


# Model Assessment for AIC forward without hispanic
aicres <- residuals(no_hisp, "resp")
binnedplot(x=fitted(no_hisp),y=aicres,xlab="Fitted Values",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Fitted Values",col.pts="navy")
binnedplot(x=jobsl$agec,y=aicres,xlab="Age",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Age",col.pts="navy")
binnedplot(x=jobsl$re74c,y=aicres,xlab="RE74",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs RE74",col.pts="navy")
binnedplot(x=jobsl$educc,y=aicres,xlab="RE74",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Education",col.pts="navy")

## Found issues with this, lets explore transformations of age
jobsl$age2 <- jobsl$agec^2
jobsl$age3 <- jobsl$agec^3
testmodel <- glm(positive ~ treat + age2 + age3 + educc + black + re74c + zero + newed +
                   treat*agec +  treat*age2 + treat*age3 +treat*zero, family = "binomial", data = jobsl)
summary(testmodel)

testres <- residuals(testmodel, "resp")

# Binned plots for assessment

binnedplot(x=fitted(testmodel),y=testres,xlab="Fitted Values",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Fitted Values",col.pts="navy")
binnedplot(x=jobsl$agec,y=testres,xlab="Age",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Age",col.pts="navy")
binnedplot(x=jobsl$re74c,y=aicres,xlab="RE74",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs RE74",col.pts="navy")
binnedplot(x=jobsl$educc,y=aicres,xlab="RE74",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Education",col.pts="navy")

# These look better
# Final model
final_model <- glm(positive ~ treat + age2 + age3 + educc + black + re74c + zero + newed +
                     treat*agec + treat*zero + treat*age2 + treat*age3 , family = "binomial", data = jobsl)

final_model_vif <- glm(positive ~ treat + agec+ age2 + age3 + educc + black + re74c + zero + newed +
              treat*zero, family = "binomial", data = jobsl)
vif(final_model_vif)
## added higher order terms in interaction

summary(final_model)
vif(final_model)

invisible(roc(jobsl$positive,fitted(final_model_proper),plot=T,print.thres="best",legacy.axes=T,
              print.auc =T,col="red3"))
# AUC 0.676, best point 0.802 - sensitivity 0.514, specificity 0.748
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(final_model) >= mean(jobsl$positive), "1","0")),
                            as.factor(jobsl$positive),positive = "1")
Conf_mat

# Graphing effect of age
newage <- seq(from=-11,to=27,by=.5)
newdata <- data.frame(matrix(0, nrow=length(newage), ncol = 7))
names(newdata) <- c("treat","agec", "educc", "black", "re74c", "zero", "newed")
newdata$agec <- newage; newdata$treat = 'no training'; newdata$re74c = 0; newdata$black = 'not black'; newdata$newed = 'less than 9'
newdata$educc <- 0
newdata$age2 <- newdata$agec^2
newdata$age3 <- newdata$agec^3
newdata$zero <- 'not zero'
newdata$agec <- as.numeric(newdata$agec)

newdata$educc <- as.numeric(newdata$educc)

preds_no_train <- predict(final_model, newdata, type = "response", se.fit = TRUE)
newdata$zero <- 'zero'
preds_no_train_zero <- predict(final_model, newdata, type = "response", se.fit = TRUE)
newdata$zero <- 'not zero'
newdata$treat <- 'training' 
pred_train_pos <- predict(final_model, newdata, type = "response", se.fit = TRUE)
newdata$zero <- 'zero'
pred_train_zero <- predict(final_model, newdata, type = "response", se.fit = TRUE)

plot(y=preds_no_train$fit,x=newage,xlab="Age (Centered)",ylab="Positive Income?",
     main="Expected Change in Probability of Positive Income with Age",col="darkblue",ylim=c(0,1), type = "l", lwd = 2)
points(y=pred_train_pos$fit, x=newage,col="orange", pch = 19, type = "l", lwd = 2)
points(y=preds_no_train_zero$fit, x=newage, col="green", pch = 19, type = "l", lwd = 2)
points(y=pred_train_zero$fit, x=newage, col="red", pch = 19, type = "l", lwd = 2)
legend("bottomleft",c("No Training (positive in 74)", "No Training (zero in 74)", "Training (positive in 74)","Training (zero in 74)"),col=c("darkblue","green", "orange", "red"),lty=c(2,2))

invisible(roc(jobsl$positive,fitted(no_hisp),plot=T,legacy.axes=T, print.thres = 'best', col="red3"))
invisible(roc(jobsl$positive,fitted(final_model),plot=T,legacy.axes=T, col="blue3",add=T))
legend('bottomright', c('No Hispanic', 'Polynomial'),lty=c(1,1),
       lwd=c(2,2),col=c('red3', 'blue3'))
