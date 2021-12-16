# Load packages
library(ggplot2)
library(tidyverse)
library(car)

### Read in data
jobs <- read.csv("Data/lalondedata.txt")

### Prepare data for analysis
# Assign factor variables:
jobs$treat <- factor(jobs$treat, levels = c(0,1), labels = c('no training', 'training'))
jobs$black <- factor(jobs$black, levels = c(0,1), labels = c('not black', 'black'))
jobs$hispan <- factor(jobs$hispan, levels = c(0,1), labels = c('not hispanic', 'hispanic'))
jobs$married <- factor(jobs$married, levels = c(0,1), labels = c('not married', 'married'))
jobs$nodegree <- factor(jobs$nodegree, levels = c(0,1), labels = c('Degree', 'No Degree'))

# Create zero income variable based on income.
jobs$zero <- as.factor(ifelse(jobs$re74 == 0, 'zero', 'not zero'))

# Select relevant variables
jobs <- dplyr::select(jobs, -X, -re75)


### EDA
# First, examining the dataset
dim(jobs)
summary(jobs)
str(jobs)
head(jobs)

# Plotting potential predictor variables
# Age and income
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method = "lm")

# Education and income
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method = "lm")
plot(0:18, tapply(jobs$re78, jobs$educ, mean), col = 'blue4', pch = 10) 

# Income in 1974 and income in 1978
ggplot(jobs, aes(x=re74, y =re78)) + geom_point() + geom_smooth(method = "lm")

# Treatment and income
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot()

# Black/Hispanic vs. income
ggplot(jobs, aes(x = black, y = re78)) + geom_boxplot()
ggplot(jobs, aes(x = hispan, y = re78)) + geom_boxplot()

# Marrried and income
ggplot(jobs, aes(x = married, y = re78)) + geom_boxplot()

# No degree and income
ggplot(jobs, aes(x = nodegree, y = re78)) + geom_boxplot()

# What about zero income. Unsurprisingly, not zero has a higher income.
ggplot(jobs, aes(x = zero, y = re78)) + geom_boxplot()


## Interactions
# Treatment and discrete/continuous predictors
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~treat)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method ='lm') + facet_wrap(~treat)
ggplot(jobs, aes(x = re74, y = re78)) + geom_point() + geom_smooth(method ='lm') + facet_wrap(~treat)

# Black and discrete/continuous predictors
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method ='lm') + facet_wrap(~black)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method ='lm') + facet_wrap(~black)
ggplot(jobs, aes(x = re74, y = re78)) + geom_point() + geom_smooth(method ='lm') + facet_wrap(~black)

# Hispanic and discrete/continuous predictors
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method ='lm') + facet_wrap(~hispan)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method ='lm') + facet_wrap(~hispan)
ggplot(jobs, aes(x = re74, y = re78)) + geom_point() + facet_wrap(~hispan) + geom_smooth(method = 'lm')

# Married and discrete/continuous predictors
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~married)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~married)
ggplot(jobs, aes(x = re74, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~married)

# Degree and discrete/continuous predictors
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~nodegree)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~nodegree)
ggplot(jobs, aes(x = re74, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~nodegree)

# Zero and discrete/continuous predictors
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~zero)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~zero)


# Interactions between categorical variables
# Treat
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~black)
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~hispan)
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~married)
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~nodegree)
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~zero)

# Black
ggplot(jobs, aes(x = black, y = re78)) + geom_boxplot() + facet_wrap(~married)
ggplot(jobs, aes(x = black, y = re78)) + geom_boxplot() + facet_wrap(~nodegree)
ggplot(jobs, aes(x = black, y = re78)) + geom_boxplot() + facet_wrap(~zero)

# Hispanic
ggplot(jobs, aes(x = hispan, y = re78)) + geom_boxplot() + facet_wrap(~married)
ggplot(jobs, aes(x = hispan, y = re78)) + geom_boxplot() + facet_wrap(~nodegree)
ggplot(jobs, aes(x = hispan, y = re78)) + geom_boxplot() + facet_wrap(~zero)

# Married
ggplot(jobs, aes(x = married, y = re78)) + geom_boxplot() + facet_wrap(~zero)
ggplot(jobs, aes(x = married, y = re78)) + geom_boxplot() + facet_wrap(~nodegree)

# No Degree
ggplot(jobs, aes(x = nodegree, y = re78)) + geom_boxplot() + facet_wrap(~zero)

### Modeling
# Mean center continuous variables
jobs$agec <- jobs$age - mean(jobs$age)
jobs$educc <- jobs$educ - mean(jobs$educ)
jobs$re74c <- jobs$re74 - mean(jobs$re74)

# Fit first model with all predictors and interactions
model1 <- lm(re78~treat*(agec + educc + black + hispan + re74c + married + zero)
             + married*agec + married*re74c + hispan*married + married*zero, data = jobs)
summary(model1)

# Model Assessment
plot(model1)
ggplot(jobs, aes(x = agec, y = model1$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Age",x="Age",y="Residuals")

ggplot(jobs, aes(x = educc, y = model1$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Education",x="Education",y="Residuals")

ggplot(jobs, aes(x = re74c, y = model1$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Income in 1974",x="Income in 1974",y="Residuals")

# Add square term for income
jobs$re742 <- jobs$re74c^2
model2 <-lm(re78~treat*(agec + educc + black + hispan + re74c + married + zero + re742) + married*agec + married*re74c + hispan*married, data = jobs)
summary(model2)


# Model Assessment
plot(model2)
ggplot(jobs, aes(x = agec, y = model1$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Age",x="Age",y="Residuals")

ggplot(jobs, aes(x = educc, y = model1$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Education",x="Education",y="Residuals")

ggplot(jobs, aes(x = re74c, y = model1$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Income in 1974",x="Income in 1974",y="Residuals")

# Model selection
nullmodel <- lm(re78~treat, data = jobs)
n <- nrow(jobs)
aic_step <- step(nullmodel, scope = list(upper = model1, lower = nullmodel), direction = "both", trace = 0)
bic_step <- step(nullmodel, scope = list(upper = model1, lower = nullmodel), direction = "both", trace = 0, k = log(n))

anova(aic_step, bic_step)

plot(aic_step)
ggplot(jobs, aes(x = agec, y = aic_step$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red") + theme_classic() + 
  labs(title="Residuals vs Age",x="Age",y="Residuals")

ggplot(jobs, aes(x = educc, y = aic_step$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Education",x="Education",y="Residuals")

ggplot(jobs, aes(x = re74c, y = aic_step$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Income in 1974",x="Income in 1974",y="Residuals")

summary(aic_step)
vif(aic_step)

