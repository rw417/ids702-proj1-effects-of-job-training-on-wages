rm(list = ls())
library(ggplot2)


###### Data
data <- read.csv("/Users/Jung/IDS702/lalondedata.txt", header= T,
                 colClasses = c("factor","factor","numeric","numeric","factor","factor","factor","factor","numeric","numeric","numeric"))

data$treat_fac <- factor(data$treat,levels=c(0,1),labels=c("No Training","Training"))
data$black <- factor(data$black,levels=c(0,1),labels=c("Otherwise","Black"))
data$hispan <- factor(data$hispan,levels=c(0,1),labels=c("Otherwise","Hispanic"))
data$married <- factor(data$married,levels=c(0,1),labels=c("Otherwise","Married"))
data$nodegree <- factor(data$nodegree,levels=c(0,1),labels=c("Otherwise","dropped out of high school"))
head(data)
summary(data)

###### EDA
# the distribution of the response variable
ggplot(data,aes(x=re78)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                  fill="lightblue", binwidth = 1000) +
#                 fill=rainbow(16),binwidth = 1000) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Salary",y="Salary") + 
  theme_classic() + theme(legend.position="none")

# the relationship between `re78` and each predictor
ggplot(data,aes(x=age, y=re78)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Salary vs Age",x="Age",y="Salary")

ggplot(data,aes(x=educ, y=re78)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Salary vs Education",x="Education",y="Salary")

ggplot(data,aes(x=re74, y=re78)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Salary vs Salary 74",x="Salary 74",y="Salary")

ggplot(data,aes(x=re75, y=re78)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Salary vs Salary 75",x="Salary 75",y="Salary")

ggplot(data,aes(x=treat_fac, y=re78, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Training",x="Training",y="Salary") + 
  theme_classic() + theme(legend.position="none")

ggplot(data,aes(x=black, y=re78, fill=black)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Black",x="Black",y="Salary") + 
  theme_classic() + theme(legend.position="none")

ggplot(data,aes(x=hispan, y=re78, fill=hispan)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Hispanic",x="Hispanic",y="Salary") + 
  theme_classic() + theme(legend.position="none")

ggplot(data,aes(x=married, y=re78, fill=married)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Married",x="Married",y="Salary") + 
  theme_classic() + theme(legend.position="none")

ggplot(data,aes(x=nodegree, y=re78, fill=nodegree)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Degree",x="Degree",y="Salary") + 
  theme_classic() + theme(legend.position="none")

# interactions
# continuous
ggplot(data,aes(x=age, y=re78)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Salary vs Age by Training",x="Age",y="Salary") +
  facet_wrap( ~ treat_fac,ncol=4)

ggplot(data,aes(x=educ, y=re78)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Salary vs Education by Training",x="Education",y="Salary") +
  facet_wrap( ~ treat_fac,ncol=4)

ggplot(data,aes(x=re74, y=re78)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Salary vs Salary 74 by Training",x="Salary 74",y="Salary") +
  facet_wrap( ~ treat_fac,ncol=4)

ggplot(data,aes(x=re75, y=re78)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Salary vs Salary 75 by Training",x="Salary 75",y="Salary") +
  facet_wrap( ~ treat_fac,ncol=4)

# categorical
ggplot(data,aes(x=black, y=re78, fill=black)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Black by Training",x="Black",y="Salary") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ treat_fac,ncol=4)

ggplot(data,aes(x=treat_fac, y=re78, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Training by Black",x="Training",y="Salary") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ black,ncol=4)

ggplot(data,aes(x=hispan, y=re78, fill=hispan)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Hispanic by Training",x="Hispanic",y="Salary") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ treat_fac,ncol=4)

ggplot(data,aes(x=treat_fac, y=re78, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Training by Hispanic",x="Training",y="Salary") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ hispan,ncol=4)

ggplot(data,aes(x=married, y=re78, fill=married)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Married by Training",x="Married",y="Salary") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ treat_fac,ncol=4)

ggplot(data,aes(x=treat_fac, y=re78, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Training by Married",x="Training",y="Salary") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ married,ncol=4)

ggplot(data,aes(x=nodegree, y=re78, fill=nodegree)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Degree by Training",x="Degree",y="Salary") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ treat_fac,ncol=4)

ggplot(data,aes(x=treat_fac, y=re78, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Salary vs Training by Degree",x="Training",y="Salary") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ nodegree,ncol=4)


###### Modeling and Model Assessment
# centering
data$age_c <- data$age - mean(data$age)
data$age_c2 <- data$age_c^2
data$educ_c <- data$educ - mean(data$educ)
data$educ_c2 <- data$educ_c^2

# model fitting
Model1 <- lm(re78~treat+age_c+educ_c+black+hispan+married+nodegree+re74+re75,data=data)
summary(Model1)
plot(Model1, which=1:5, col=c("blue4"))

Model2 <- lm(re78~treat+age_c+age_c2+educ_c+educ_c2+black+hispan+married+nodegree+re74+re75,data=data)
summary(Model2)
plot(Model2, which=1:5, col=c("blue4"))

Model3 <- lm(re78~treat+age_c+age_c2+log(educ)+black+hispan+married+nodegree+re74+re75,data=data)
summary(Model3)
plot(Model3, which=1:5, col=c("blue4"))


vif(model1)











