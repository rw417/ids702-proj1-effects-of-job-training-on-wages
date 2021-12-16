library(arm)
library(rms)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)
library(leaps)

# set CD and import data
setwd("C:/Users/HP/GitHub/team-project-1-effects-of-job-training-on-wages-purple-team")
lalondedata <- read.csv("C:/Users/HP/GitHub/team-project-1-effects-of-job-training-on-wages-purple-team/Data/lalondedata.txt")

# explore data types
dim(lalondedata)
summary(lalondedata)
str(lalondedata)
head(lalondedata)

# convert black, hispan, married, nodegree to factor variables
lalondedata$treat <- as.factor(lalondedata$treat)
lalondedata$black <- as.factor(lalondedata$black)
lalondedata$hispan <- as.factor(lalondedata$hispan)
lalondedata$married <- as.factor(lalondedata$married)
lalondedata$nodegree <- as.factor(lalondedata$nodegree)

####
# Part I ####
####

##
## Wage ####
##
# maybe use re78 as the response since Michael asked about it?
# but then we should include re74 as a predictor in order to control for the effect of pre-training wage

# re74
ggplot(data=lalondedata, aes(x=re74)) +
  geom_histogram(color = "black", fill = "grey", bins=12) +
  labs(title = "Distribution of Eruption Interval", x="Interval", y="Count")

# re75
ggplot(data=lalondedata, aes(x=re75)) +
  geom_histogram(color = "black", fill = "grey", bins=12) +
  labs(title = "Distribution of Eruption Interval", x="Interval", y="Count")

# re78
ggplot(data=lalondedata, aes(x=re78)) +
  geom_histogram(color = "black", fill = "grey", bins=12) +
  labs(title = "Distribution of Eruption Interval", x="Interval", y="Count")

# post vs pre
lalondedata["re78vs74"] <- lalondedata$re78 - lalondedata$re74
ggplot(data=lalondedata, aes(x=re78vs74)) +
  geom_histogram(color = "black", fill = "grey", bins=12) +
  labs(title = "Distribution of Eruption Interval", x="Interval", y="Count")

lalondedata["re78vs75"] <- lalondedata$re78 - lalondedata$re75
ggplot(data=lalondedata, aes(x=re78vs75)) +
  geom_histogram(color = "black", fill = "grey", bins=12) +
  labs(title = "Distribution of Eruption Interval", x="Interval", y="Count")

# log re78
lalondedata["logre78"] <- log(lalondedata$re78+1)
ggplot(data=lalondedata, aes(x=logre78)) +
  geom_histogram(color = "black", fill = "grey", bins=12) +
  labs(title = "Distribution of Eruption Interval", x="Interval", y="Count")

# re78 for treat vs control
# similar distribution
ggplot(data=lalondedata[lalondedata$treat==1,], aes(x=re78)) +
  geom_histogram(aes(y=..density..), color = "black", fill = "grey", bins=10) +
  geom_density(color = 'red') + labs(title = "Distribution of RE78 for Treat==1", x="Interval", y="Density")

ggplot(data=lalondedata[lalondedata$treat==0,], aes(x=re78)) +
  geom_histogram(aes(y=..density..), color = "black", fill = "grey", bins=10) +
  geom_density(color = 'red') + labs(title = "Distribution of RE78 for Treat==0", x="Interval", y="Density")


##
## Predictor Variables ####
##
# treat doesn't seem to have an effect alone - maybe due to confounding variables
# use treat, age, educ, black, hisp, married, re74 as predictors
# for interaction, use age:hispan, age:married, educ:hispan, re74:treat, re74:hispan, re74:married

# treat
ggplot(data=lalondedata, aes(x=treat, y=re78)) + geom_boxplot() # no difference?

var.test(lalondedata$re78[lalondedata$treat == 1], lalondedata$re78[lalondedata$treat == 0])
t.test(lalondedata$re78[lalondedata$treat == 1], lalondedata$re78[lalondedata$treat == 0], var.equal = TRUE) # not significant
# difference is not significant?
# try controlling for confounding variables?

# age
ggplot(data=lalondedata, aes(x=age, y=re78)) + geom_point() + 
  geom_smooth(aes(x=age, y=re78, color='Loess')) + 
  geom_smooth(aes(x=age, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Age vs Re78")

# educ
ggplot(data=lalondedata, aes(x=educ, y=re78)) + geom_point() + 
  geom_smooth(aes(x=educ, y=re78, color='Loess')) + 
  geom_smooth(aes(x=educ, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Educ vs Re78")

ggplot(data=lalondedata, aes(x=educ)) +
  geom_histogram(color = "black", fill = "grey", bins=12) +
  labs(title = "Distribution of Eruption Interval", x="Interval", y="Count")

# re74
ggplot(data=lalondedata, aes(x=re74, y=re78)) + geom_point() + 
  geom_smooth(aes(x=re74, y=re78, color='Loess')) + 
  geom_smooth(aes(x=re74, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Re74 vs Re78")
ggplot(data=lalondedata, aes(x=log(re74+1), y=log(re78+1))) + geom_point() + 
  geom_smooth(aes(x=log(re74+1), y=log(re78+1), color='Loess')) + 
  geom_smooth(aes(x=log(re74+1), y=log(re78+1), color='OLS'), method=lm) + 
  #scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of log(re74) vs log(re78)")

# re75
ggplot(data=lalondedata, aes(x=re75, y=re78)) + geom_point() + 
  geom_smooth(aes(x=re75, y=re78, color='Loess')) + 
  geom_smooth(aes(x=re75, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Re75 vs Re78")
ggplot(data=lalondedata, aes(x=log(re75+1), y=log(re78+1))) + geom_point() + 
  geom_smooth(aes(x=log(re75+1), y=log(re78+1), color='Loess')) + 
  geom_smooth(aes(x=log(re75+1), y=log(re78+1), color='OLS'), method=lm) + 
  #scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of log(re75) vs log(re78)")


# black
ggplot(data=lalondedata, aes(x=black, y=re78)) + geom_boxplot()

var.test(lalondedata$re78[lalondedata$black == 1], lalondedata$re78[lalondedata$black == 0])
t.test(lalondedata$re78[lalondedata$black == 1], lalondedata$re78[lalondedata$black == 0], var.equal = TRUE) # not significant
# difference is significant

# hispan
ggplot(data=lalondedata, aes(x=hispan, y=re78)) + geom_boxplot()
# hispan wage is actually higher
# is it significant?
# NO!
var.test(lalondedata$re78[lalondedata$hispan == 1], lalondedata$re78[lalondedata$hispan == 0])
t.test(lalondedata$re78[lalondedata$hispan == 1], lalondedata$re78[lalondedata$hispan == 0], var.equal = TRUE) # not significant
# # is it because hispan have higher education?
# ggplot(data=lalondedata, aes(x=hispan, y=educ)) + geom_boxplot() # no
# # wage gap between 78 and 74?
# ggplot(data=lalondedata, aes(x=hispan, y=re78vs74)) + geom_boxplot()
# ggplot(data=lalondedata[lalondedata$hispan == 1,], aes(x=re78vs74)) + geom_histogram(color = "black", fill = "grey", bins=12)
# wilcox.test(lalondedata$re78vs74[lalondedata$hispan == 1], lalondedata$re78vs74[lalondedata$hispan == 0]) # not significant
# var.test(lalondedata$re78vs74[lalondedata$hispan == 1], lalondedata$re78vs74[lalondedata$hispan == 0])
# t.test(lalondedata$re78vs74[lalondedata$hispan == 1], lalondedata$re78vs74[lalondedata$hispan == 0], var.equal = TRUE)

# married
ggplot(data=lalondedata, aes(x=married, y=re78)) + geom_boxplot()
var.test(lalondedata$re78[lalondedata$married == 1], lalondedata$re78[lalondedata$married == 0]) # var not equal
t.test(lalondedata$re78[lalondedata$married == 1], lalondedata$re78[lalondedata$married == 0], var.equal = FALSE)
wilcox.test(lalondedata$re78[lalondedata$married == 1], lalondedata$re78[lalondedata$married == 0])
# difference is very significant

# nodegree
ggplot(data=lalondedata, aes(x=nodegree, y=re78)) + geom_boxplot()
var.test(lalondedata$re78[lalondedata$nodegree == 1], lalondedata$re78[lalondedata$nodegree == 0]) # var not equal
t.test(lalondedata$re78[lalondedata$nodegree == 1], lalondedata$re78[lalondedata$nodegree == 0], var.equal = FALSE)
wilcox.test(lalondedata$re78[lalondedata$nodegree == 1], lalondedata$re78[lalondedata$nodegree == 0])
# difference is very significant


##
## Interactions ####
##
# age
  # treat # no effect
ggplot(data=lalondedata, aes(x=age, y=re78)) + geom_point() + 
  geom_smooth(aes(x=age, y=re78, color='Loess')) + 
  geom_smooth(aes(x=age, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Age vs Re78") + 
  facet_wrap(~treat)
  # black # no effect
ggplot(data=lalondedata, aes(x=age, y=re78)) + geom_point() + 
  geom_smooth(aes(x=age, y=re78, color='Loess')) + 
  geom_smooth(aes(x=age, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Age vs Re78") + 
  facet_wrap(~black)
  # hispan # some effects?
ggplot(data=lalondedata, aes(x=age, y=re78)) + geom_point() + 
  geom_smooth(aes(x=age, y=re78, color='Loess')) + 
  geom_smooth(aes(x=age, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Age vs Re78") + 
  facet_wrap(~hispan)
  # married # some effects - the older your are, the more married takes an effect
  # confounds the effect on training - married people usually makes more
ggplot(data=lalondedata, aes(x=age, y=re78)) + geom_point() + 
  geom_smooth(aes(x=age, y=re78, color='Loess')) + 
  geom_smooth(aes(x=age, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Age vs Re78") + 
  facet_wrap(~married)
  # nodegree # you actually makes more when you're older if you don't have a degree?
  # can this be explained by educ?
ggplot(data=lalondedata, aes(x=age, y=re78)) + geom_point() + 
  geom_smooth(aes(x=age, y=re78, color='Loess')) + 
  geom_smooth(aes(x=age, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Age vs Re78") + 
  facet_wrap(~nodegree)

# educ
  # treat # no effects
ggplot(data=lalondedata, aes(x=educ, y=re78)) + geom_point() + 
  geom_smooth(aes(x=educ, y=re78, color='Loess')) + 
  geom_smooth(aes(x=educ, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of educ vs Re78") + 
  facet_wrap(~treat)
  # black # no effect
ggplot(data=lalondedata, aes(x=educ, y=re78)) + geom_point() + 
  geom_smooth(aes(x=educ, y=re78, color='Loess')) + 
  geom_smooth(aes(x=educ, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of educ vs Re78") + 
  facet_wrap(~black)
  # hispan # if you're not hispanic, then educ makes a difference on wage; if you're hispanic, educ has not effect on wage
ggplot(data=lalondedata, aes(x=educ, y=re78)) + geom_point() + 
  geom_smooth(aes(x=educ, y=re78, color='Loess')) + 
  geom_smooth(aes(x=educ, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of educ vs Re78") + 
  facet_wrap(~hispan)
  # married # no effect
ggplot(data=lalondedata, aes(x=educ, y=re78)) + geom_point() + 
  geom_smooth(aes(x=educ, y=re78, color='Loess')) + 
  geom_smooth(aes(x=educ, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of educ vs Re78") + 
  facet_wrap(~married)
  # nodegree # don't do this - will be very correlated

# re74
  # treat # some effects but hard to tell
ggplot(data=lalondedata, aes(x=re74, y=re78)) + geom_point() + 
  geom_smooth(aes(x=re74, y=re78, color='Loess')) + 
  geom_smooth(aes(x=re74, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Re74 vs Re78") +
  facet_wrap(~treat)
  # black # some effects but hard to tell
ggplot(data=lalondedata, aes(x=re74, y=re78)) + geom_point() + 
  geom_smooth(aes(x=re74, y=re78, color='Loess')) + 
  geom_smooth(aes(x=re74, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Re74 vs Re78") +
  facet_wrap(~black)
  # hispan # has effects
ggplot(data=lalondedata, aes(x=re74, y=re78)) + geom_point() + 
  geom_smooth(aes(x=re74, y=re78, color='Loess')) + 
  geom_smooth(aes(x=re74, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Re74 vs Re78") +
  facet_wrap(~hispan)
  ##
  # married # huge effects
  ##
ggplot(data=lalondedata, aes(x=re74, y=re78)) + geom_point() + 
  geom_smooth(aes(x=re74, y=re78, color='Loess')) + 
  geom_smooth(aes(x=re74, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Re74 vs Re78") +
  facet_wrap(~married)
chisq.test(lalondedata$treat, lalondedata$married)
round(apply(table(lalondedata[,c("treat","married")])/sum(table(lalondedata[,c("treat","married")])),
            2,function(x) x/sum(x)),2)
  # interesting. If you're married, you're actually less likely to be in the treatment group
  # nodegree # no effects
ggplot(data=lalondedata, aes(x=re74, y=re78)) + geom_point() + 
  geom_smooth(aes(x=re74, y=re78, color='Loess')) + 
  geom_smooth(aes(x=re74, y=re78, color='OLS'), method=lm) + 
  scale_color_manual(name = "Lines", values = c("Loess"="blue", "OLS" = "red")) + 
  labs(title = "Scatterplot of Re74 vs Re78") +
  facet_wrap(~nodegree)


#### treat ####
# black # black benefit from treat
mean(lalondedata$re78[(lalondedata$treat==1) & (lalondedata$black==1)])
mean(lalondedata$re78[(lalondedata$treat==1) & (lalondedata$black==0)])
mean(lalondedata$re78[(lalondedata$treat==0) & (lalondedata$black==1)])
mean(lalondedata$re78[(lalondedata$treat==0) & (lalondedata$black==0)])

mean(lalondedata$re78vs75[(lalondedata$treat==1) & (lalondedata$black==1)])
mean(lalondedata$re78vs75[(lalondedata$treat==1) & (lalondedata$black==0)])
mean(lalondedata$re78vs75[(lalondedata$treat==0) & (lalondedata$black==1)])
mean(lalondedata$re78vs75[(lalondedata$treat==0) & (lalondedata$black==0)])

# hispanic # hispanic doesn't benefit from train
mean(lalondedata$re78[(lalondedata$treat==1) & (lalondedata$hispan==1)])
mean(lalondedata$re78[(lalondedata$treat==1) & (lalondedata$hispan==0)])
mean(lalondedata$re78[(lalondedata$treat==0) & (lalondedata$hispan==1)])
mean(lalondedata$re78[(lalondedata$treat==0) & (lalondedata$hispan==0)])

# married # married doesn't benefit more from treat
mean(lalondedata$re78[(lalondedata$treat==1) & (lalondedata$married==1)])
mean(lalondedata$re78[(lalondedata$treat==1) & (lalondedata$married==0)])
mean(lalondedata$re78[(lalondedata$treat==0) & (lalondedata$married==1)])
mean(lalondedata$re78[(lalondedata$treat==0) & (lalondedata$married==0)])


mean(lalondedata$re75[(lalondedata$treat==1) & (lalondedata$married==1)])
mean(lalondedata$re75[(lalondedata$treat==1) & (lalondedata$married==0)])
mean(lalondedata$re75[(lalondedata$treat==0) & (lalondedata$married==1)])
mean(lalondedata$re75[(lalondedata$treat==0) & (lalondedata$married==0)])






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
