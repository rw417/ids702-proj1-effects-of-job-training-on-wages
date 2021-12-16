# load packages
library(ggplot2)
library(tidyverse)
library(car)

# read in the data
jobs <- read.csv("Data/lalondedata.txt")

# Assign factor variables:
jobs$treat <- factor(jobs$treat, levels = c(0,1), labels = c('no training', 'training'))
jobs$black <- factor(jobs$black, levels = c(0,1), labels = c('not black', 'black'))
jobs$hispan <- factor(jobs$hispan, levels = c(0,1), labels = c('not hispanic', 'hispanic'))
jobs$married <- factor(jobs$married, levels = c(0,1), labels = c('not married', 'married'))
jobs$nodegree <- factor(jobs$nodegree, levels = c(0,1), labels = c('Degree', 'No Degree'))
# Create income variable with 1974 salary for treatment workers and 1975 for non-treatment.
jobs$inc <- ifelse(jobs$treat == 1, jobs$re74, jobs$re75)
# Create zero income variable based on income.
jobs$zero <- as.factor(ifelse(jobs$inc == 0, 'zero', 'not zero'))

head(jobs)
summary(jobs)
# Check the baseline income treat vs. not treat.
aggregate(inc ~ treat, jobs, mean)
aggregate(re74 ~ treat, jobs, mean)
aggregate(re75 ~ treat, jobs, mean)
# Check the baseline # of zeros for treat vs. not treat.
apply(table(jobs[,c("zero","treat")])/sum(table(jobs[,c("zero","treat")])),
      2,function(x) x/sum(x)) 

# Everything seems will distributed. One thing to notice is that the number of hispanics is realtively small.


# Investigate numerical variables, notice that age is concentrated younger range.
# Eductation is closer to normal, but large concentration in the 8-12 range.
hist(jobs$age)
hist(jobs$educ)
hist(jobs$re74)
hist(jobs$re75)
hist(jobs$re78)

# Notice that all the incomes (including the response variable) are heavily skewed towards zero. What does the log histogram look like?
hist(log(jobs$re74))
hist(log(jobs$re75))
hist(log(jobs$re78))

# Closer to normal, but still very skewed. Will have to keep this in mind!
# Now let's look at some plots. First scatter plots.
# Between age and income, no direct trend, but there is one very large outlier to consider.
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth()

# Between education and income. There may be a trend, but it's difficult to tell. Maybe consider buckets?
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth()

# Between previous income and current income. Again, a very large potential high influence/outlier.
ggplot(jobs, aes(x=inc, y =re78)) + geom_point() + geom_smooth(method = "lm")


# Now onto categorical variables! 
# For treatment, it actual looks like no training has the advantage
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot()

# For black and hispanic. Looks like black workers make less and hispanic make more.
ggplot(jobs, aes(x = black, y = re78)) + geom_boxplot()
ggplot(jobs, aes(x = hispan, y = re78)) + geom_boxplot()

# For married, looks like married workers make more.
ggplot(jobs, aes(x = married, y = re78)) + geom_boxplot()

# Finally, for no degree, as expected, degree makes more.
ggplot(jobs, aes(x = nodegree, y = re78)) + geom_boxplot()

# What about zero income. Unsurprisingly, not zero has a higher income.
ggplot(jobs, aes(x = zero, y = re78)) + geom_boxplot()


## Now let's do interactions.
# Let's look at treat interactions. Something a little werid between inc and treat?
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth() + facet_wrap(~treat)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth() + facet_wrap(~treat)
ggplot(jobs, aes(x = re74, y = re78)) + geom_point() + geom_smooth() + facet_wrap(~treat)

# Next, everything by black
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth() + facet_wrap(~black)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth() + facet_wrap(~black)
ggplot(jobs, aes(x = re74, y = re78)) + geom_point() + geom_smooth() + facet_wrap(~black)

# Hispanic
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth() + facet_wrap(~hispan)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth() + facet_wrap(~hispan)
ggplot(jobs, aes(x = re74, y = re78)) + geom_point() + facet_wrap(~hispan) + geom_smooth(method = 'lm')

# Married. Maybe ann interaction between married and re75?
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~married)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~married)
ggplot(jobs, aes(x = re74, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~married)

# Now degree.
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~nodegree)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~nodegree)
ggplot(jobs, aes(x = inc, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~nodegree)

# And finally, zero
ggplot(jobs, aes(x = age, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~zero)
ggplot(jobs, aes(x = educ, y = re78)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(~zero)

# Interactions between categorical variables
# First, let's look at interaction between treat and other categorical variables
# Maybe an interaction between training and black!
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~black)
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~hispan)
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~married)
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~nodegree)
# Yes, there's an interaction between zero and treat!!
ggplot(jobs, aes(x = treat, y = re78)) + geom_boxplot() + facet_wrap(~zero)

# Now black and remaining categorical variables
ggplot(jobs, aes(x = black, y = re78)) + geom_boxplot() + facet_wrap(~married)
ggplot(jobs, aes(x = black, y = re78)) + geom_boxplot() + facet_wrap(~nodegree)
ggplot(jobs, aes(x = black, y = re78)) + geom_boxplot() + facet_wrap(~zero)

# Hispanic and remaining
# Looks like an interaction between hispanic and married (but there are few data points). Looks honestly like a interaction between hispanic and all the points.
ggplot(jobs, aes(x = hispan, y = re78)) + geom_boxplot() + facet_wrap(~married)
ggplot(jobs, aes(x = hispan, y = re78)) + geom_boxplot() + facet_wrap(~nodegree)
ggplot(jobs, aes(x = hispan, y = re78)) + geom_boxplot() + facet_wrap(~zero)

# Now married.
ggplot(jobs, aes(x = married, y = re78)) + geom_boxplot() + facet_wrap(~zero)


# Initial modeling
# Mean center continuous variables
jobs$agec <- jobs$age - mean(jobs$age)
jobs$educc <- jobs$educ - mean(jobs$educ)
jobs$re74c <- jobs$re74 - mean(jobs$re74)

# To start, let's do all of the variables plus below interacctions.
# All interactions of treat and other variables. Additionally, Hispanic and married and hispanic and degree.
model1 <- lm(re78~treat + agec + educc + black + hispan + married + re74c, data = jobs)
summary(model1)
plot(model1)


# Try log transforming. First do add one. Then also tried sqrt, nothing great.
jobs$re78st <- sqrt(jobs$re78)
model2 <- lm(re78st~treat + agec + educc + black + hispan + married + re74c, data = jobs)
summary(model2)
plot(model2)

# We like model 1 better. Let's look at linearity assumptions
ggplot(jobs, aes(x = agec, y = model1$residuals)) + geom_point(alhpa = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Age",x="Age",y="Residuals")

ggplot(jobs, aes(x = educc, y = model1$residuals)) + geom_point(alhpa = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Education",x="Education",y="Residuals")

ggplot(jobs, aes(x = re74c, y = model1$residuals)) + geom_point(alhpa = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Income in 1974",x="Income in 1974",y="Residuals")

# Lets try adding a square term for income
jobs$re742 <- jobs$re74c^2
model3 <- lm(re78~treat + agec + educc + black + hispan + married + re74c + re742, data = jobs)
summary(model3)
plot(model3)

ggplot(jobs, aes(x = agec, y = model3$residuals)) + geom_point(alhpa = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Age",x="Age",y="Residuals")

ggplot(jobs, aes(x = educc, y = model3$residuals)) + geom_point(alhpa = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Education",x="Education",y="Residuals")

ggplot(jobs, aes(x = re74c, y = model3$residuals)) + geom_point(alhpa = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Income in 1974",x="Income in 1974",y="Residuals")

# Now let's try adding interactions
# Black and treatment. Married and age. Married and income. Hispanic and married. Hispanic and degree.
model4 <- lm(re78~treat + agec + educc + black + hispan + married + re74c + re742 + black*treat + married*agec + married*re74c + hispan*married, data = jobs)
summary(model4)
plot(model4)

ggplot(jobs, aes(x = agec, y = model4$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Age",x="Age",y="Residuals")

ggplot(jobs, aes(x = educc, y = model4$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Education",x="Education",y="Residuals")

ggplot(jobs, aes(x = re74c, y = model4$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Income in 1974",x="Income in 1974",y="Residuals")


model5 <- lm(re78~treat*(agec + educc + black + hispan + married + re74c) + re742 + married*agec + married*re74c + hispan*married, data = jobs)
summary(model5)

nullmodel <- lm(re78~treat, data = jobs)
n <- nrow(jobs)
step_aic <- step(nullmodel, scope = list(upper = model5, lower = nullmodel), direction = "both", trace = 0)
step_bic <- step(nullmodel, scope = list(upper = model5, lower = nullmodel), direction = "both", trace = 0, k = log(n))
for_aic <- step(nullmodel, scope = list(upper = model5, lower = nullmodel), direction = "forward", trace = 0)
back_aic <- step(model5, scope = list(upper = model5, lower = nullmodel), direction = "backward", trace = 0)

# Gotta deal with education maybe?
ggplot(jobs, aes(x = educc, y = model5$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Education",x="Education",y="Residuals")


## Testing 
anova(step_aic, back_aic)

step_aic <- lm(re78~treat + re742 + educc + black + married + re74c, data = jobs)

plot(back_aic)

## NOTE: remove point 182 and 132 and refit model. Maybe consider other point at +4 SDs away.



