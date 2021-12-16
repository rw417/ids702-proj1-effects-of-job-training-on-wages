# load packages
library(ggplot2)
library(tidyverse)

# read in the data
jobsl <- read.csv("Data/lalondedata.txt")

# Assign factor variables:
jobsl$treat <- factor(jobsl$treat, levels = c(0,1), labels = c('no training', 'training'))
jobsl$black <- factor(jobsl$black, levels = c(0,1), labels = c('not black', 'black'))
jobsl$hispan <- factor(jobsl$hispan, levels = c(0,1), labels = c('not hispanic', 'hispanic'))
jobsl$married <- factor(jobsl$married, levels = c(0,1), labels = c('not married', 'married'))
jobsl$nodegree <- factor(jobsl$nodegree, levels = c(0,1), labels = c('Degree', 'No Degree'))
# Create income variable with 1974 salary for treatment workers and 1975 for non-treatment.
jobsl$inc <- ifelse(jobsl$treat == 1, jobsl$re74, jobsl$re75)
# Create zero income variable based on income.
jobsl$zero <- as.factor(ifelse(jobsl$inc == 0, 'zero', 'not zero'))

# Create response variable!
jobsl$positive <- as.factor(ifelse(jobsl$re78 == 0, 0, 1))
jobsl$positive_c <- as.factor(ifelse(jobsl$re78 == 0, 'zero', 'positive'))

summary(jobsl)
# Note that there are 143 zero and 471 positive.

# Now, let's do some boxplots for numeric values. There is a little something with age.
ggplot(jobsl, aes(x=positive_c, y = age, fill=positive_c)) + geom_boxplot()
# Definitely an interaction with education too.
ggplot(jobsl, aes(x=positive_c, y = educ, fill=positive_c)) + geom_boxplot()
# Now income previously, definitely an effect.
ggplot(jobsl, aes(x=positive_c, y = re74, fill=positive_c)) + geom_boxplot()

# Let's examine categorical variables. For treat, p-value not significant
apply(table(jobsl[,c("positive_c","treat")])/sum(table(jobsl[,c("positive_c","treat")])),
      2,function(x) x/sum(x)) 
chisq.test(table(jobsl[,c('positive_c', 'treat')]))

# For black. Deifnitely a change here, and is significant
apply(table(jobsl[,c("positive_c","black")])/sum(table(jobsl[,c("positive_c","black")])),
      2,function(x) x/sum(x)) 
chisq.test(table(jobsl[,c('positive_c', 'black')]))

# For Hispanic. Hispanics more likely to have positive income, but it's not statistically significant
apply(table(jobsl[,c("positive_c","hispan")])/sum(table(jobsl[,c("positive_c","hispan")])),
      2,function(x) x/sum(x)) 
chisq.test(table(jobsl[,c('positive_c', 'hispan')]))

# For marreid. Not really a difference.
apply(table(jobsl[,c("positive_c","married")])/sum(table(jobsl[,c("positive_c","married")])),
      2,function(x) x/sum(x)) 
chisq.test(table(jobsl[,c('positive_c', 'married')]))

# For no degree. Not really a difference
apply(table(jobsl[,c("positive_c","nodegree")])/sum(table(jobsl[,c("positive_c","nodegree")])),
      2,function(x) x/sum(x)) 
chisq.test(table(jobsl[,c('positive_c', 'nodegree')]))

# For previous income being zero. Definitely an effect here.
apply(table(jobsl[,c("positive_c","zero")])/sum(table(jobsl[,c("positive_c","zero")])),
      2,function(x) x/sum(x)) 
chisq.test(table(jobsl[,c('positive_c', 'zero')]))

# Now lets look at interactions between categorical variables and continuous variables
# First, everything by Black
ggplot(jobsl, aes(x=positive_c, y =age, fill=positive_c)) + geom_boxplot() + facet_wrap(~black)
# Might be an interaction between black and education.
ggplot(jobsl, aes(x=positive_c, y = educ, fill=positive_c)) + geom_boxplot() + facet_wrap(~black)
## Interaction between treatment and re74!
## Interaction between black and re74
## Interaction between re74 and married.
## Interaction between age and treatment
## No degree and age!
## Education and treatment.
## Education and black.
## Education and married.
## Married and treatment
## Interaction between black and treatment.

## Play around with age as a categorical variable.





