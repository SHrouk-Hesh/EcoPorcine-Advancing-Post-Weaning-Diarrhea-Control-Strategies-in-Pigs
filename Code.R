###################### Project C: Post-weaning diarrhea I ##################

########## Q1. Descriptive statistics #############
# A) Summarize your data and calculate the following: mean, median, minimum, maximum, first and third quartile (for each variable)

# Reading the file Project C
#load("Project C.RData")
str(PWD)
head(PWD)
summary(PWD)
# B) For a categorical variable a frequency table is more appropriate
freq_3 = table(PWD$Treatment,PWD$Sex)
freq_1 = table(PWD$Treatment)
freq_2 = table(PWD$Sex)
# C) Calculate the correlation coefficient between(ADWG0021 and ADWG2150) and (ADWG0021and ADWG0050)
cor(PWD$ADWG0021 ,PWD$ADWG2150, use="complete.obs",method="spearman")
cor(PWD$ADWG0021,PWD$ADWG0050, use="complete.obs",method="spearman")
cor(PWD$ADWG0021,PWD$ADWG0050, method = "pearson", use="complete.obs")
cor(PWD$ADWG0021,PWD$ADWG2150, method = "pearson", use="complete.obs")

################ Q2. Graphs ###################
# Assuming Male=1, Female=2

# Generate a bar chart of a categorical variable for the sex
barplot(table(PWD$Sex), xlab="Sex",ylab="Frequency",main="Bar chart",col="purple")

# Generate a bar chart graph with mean ADWG0021 in males and females
barplot(tapply(PWD$ADWG0021,list(Sex=PWD$Sex),mean,na.rm=T), xlab="Sex",ylab="Mean ADWG0021",main="Bar chart",col="purple")

#mean(PWD$ADWG0021)

# Make a histogram of a continuous variable: “ADWG2150” as well as “ADWG0021”
hist(PWD$ADWG2150,xlab="ADWG2150",main="Distribution of ADWG2150",col='blue')
hist(PWD$ADWG0021,xlab="ADWG0021",main="Distribution of ADWG0021",col='red')


# Make a scatterplot of 2 continuous variables ADWG0050 and ADWG0021, and add the regression lines for each gender
#p1 = PWD$ADWG2150 [PWD$Sex==1]
#p12 = PWD$ADWG2150 [PWD$Sex==2]
#p2 = PWD$ADWG0021 [PWD$Sex==1]
#p21 = PWD$ADWG0021 [PWD$Sex==2]

#plot(p2~p1,col=1)
#points(p21~p2,col=2)
#points(p12~p1,col=3)
#abline(lm(p12~p1))
#abline(lm(p21~p2),col=2)

plot(PWD$ADWG0050,PWD$ADWG0021,xlab="ADWG0050",ylab="ADWG0021", main="Scatterplot")

plot(ADWG0050~ADWG0021, data=PWD) 

plot(PWD$ADWG0050[PWD$Sex==1],PWD$ADWG0021[PWD$Sex==1],xlab="ADWG0050",ylab="ADWG0021",main="Scatterplot")

points(PWD$ADWG0050[PWD$Sex==2],PWD$ADWG0021[PWD$Sex==2],col=2)
points(PWD$ADWG0021[PWD$Sex==2],PWD$ADWG0050[PWD$Sex==2],col=3)

abline(lm(PWD$ADWG0050[PWD$Sex==1]~PWD$ADWG0021[PWD$Sex==1]),col=2)

abline(lm(PWD$ADWG0050[PWD$Sex==2]~PWD$ADWG0021[PWD$Sex==2]),col=3)

legend("topleft",legend = c("1","2"), fill = c("black","red", cex= 0.75))

# Make a boxplot of ADWG0021 and a separate boxplots per Treatment (as.factors).
boxplot(PWD$ADWG0021,main="Boxplot of ADWG0021")
boxplot(ADWG0021~Treatment,data=PWD,main="Box plot sperated by Treatment group")

############## 3- Outlier detection ########################

# We will find that we have two outliers in the dataset which are 178.5714 and 110.1190
outliers <- boxplot(ADWG0021~Treatment,data=PWD)$out
outliers[1] # 178.5714
outliers[2] # 110.119
outliers[3] # NA , so we have only 2 outliers

############# Q4.Check the normality  ################

#histogram test
#interested in
hist(PWD$Pen,main = "Histogram of Pen",xlab = "Pen")
hist(PWD$Feeder,main = "Histogram of Feeder",xlab = "Feeder")
hist(PWD$Sex,main = "Histogram of Sex",xlab = "Sex")
hist(PWD$W0,main = "Histogram of W0",xlab = "W0")
hist(PWD$P0,main = "Histogram of P0",xlab = "P0")
#interested in
hist(PWD$ADWG0021,main = "Histogram of ADWG0021",xlab = "ADWG0021")
hist(PWD$ADWG0050,main = "Histogram of ADWG0050",xlab = "ADWG0050")
hist(PWD$ADWG2150,main = "Histogram of ADWG2150",xlab = "ADWG2150")
#shapiro test 
#not  interested in
shapiro.test(PWD$Pen)  #0.1241
shapiro.test(PWD$Feeder)  #0.08873
shapiro.test(PWD$Sex)  #1.047e-08
shapiro.test(PWD$W0)  #0.009664
#interested in
shapiro.test(PWD$ADWG0021)  #0.7305
shapiro.test(PWD$ADWG0050)  #0.9086
shapiro.test(PWD$ADWG2150)  #0.9037
#QQ plot
#interested in
qqnorm(PWD$ADWG0021)
qqline(PWD$ADWG0021)
qqnorm(PWD$ADWG0050)
qqline(PWD$ADWG0050)
qqnorm(PWD$ADWG2150)
qqline(PWD$ADWG2150)
#not  interested in
qqnorm(PWD$Pen)
qqline(PWD$Pen)
qqnorm(PWD$Feeder)
qqline(PWD$Feeder)
qqnorm(PWD$Sex)
qqline(PWD$Sex)
qqnorm(PWD$W0)
qqline(PWD$W0)

############## Q4.Check the homoscedasticity   ###############

library(car)
var.test(PWD$ADWG0021)

boxplot(PWD$Pen,PWD$Treatment,PWD$Feeder,PWD$Sex,PWD$W0,PWD$P0,PWD$ADWG0021,PWD$ADWG2150,PWD$ADWG0050)


plot(ADWG2150 ~ ADWG0050,data = PWD)
plot(ADWG2150 ~ ADWG0021,data = PWD)
plot(ADWG0021 ~ ADWG0050,data = PWD)
plot(ADWG0021 ~ ADWG2150,data = PWD)
plot(ADWG0050 ~ ADWG2150,data = PWD)
plot(ADWG0050 ~ ADWG0021,data = PWD)

par(mfrow=c(2,2))

model = lm(ADWG2150 ~ ADWG0050, data = PWD)

summary(model)

abline(model)

plot(model)

#barlett test
bartlett.test(list(PWD$ADWG2150,PWD$ADWG0050))

# Levene's test for homogeneity of variance across groups
leveneTest(Feeder~Treatment, data=PWD)
leveneTest(W0~Treatment, data=PWD)
leveneTest(ADWG0021~Treatment, data=PWD)
leveneTest(ADWG2150~Treatment, data=PWD)
leveneTest(ADWG0050~Treatment, data=PWD)

leveneTest(Feeder~Sex, data=PWD)
leveneTest(ADWG0021~Sex, data=PWD)
leveneTest(W0~Sex, data=PWD)
leveneTest(ADWG2150~Sex, data=PWD)
leveneTest(ADWG0050~Sex, data=PWD)

############ Q5. Confidence interval #############
PWD$Sex = factor(PWD$Sex,labels = c("Male","Female"))
t.test(PWD$ADWG0021, PWD$Male, conf.level = 0.9)
t.test(PWD$ADWG0021, PWD$Female, conf.level = 0.9)
t.test(PWD$ADWG0021, PWD$Male, conf.level = 0.95)
t.test(PWD$ADWG0021, PWD$Female, conf.level = 0.95)
t.test(PWD$ADWG0021, PWD$Male, conf.level = 0.99)
t.test(PWD$ADWG0021, PWD$Female, conf.level = 0.99)


############ Q6. Hypothesis testing #############


# Doing statistical hypothesis testing: 1) First, converting the research question into a statistical question, which is: Is the mean between male and female in ADWG0021 different?


# Then, we state the null and alternative hypotheses, as following:

# The null hypothesis (H0) would be that there is no differences between the mean of groups Male and Female in terms of the mean content.
# The alternative hypothesis (H1) would be that the mean of Male groups will be different from the mean of Female groups.


# Next, we Test Statistic and check the results by comparing the p-value result to our significance level alpha (0.05)
# If the p-value is greater than alpha (0.05), then we don't have enough evidence to reject the null hypothesis in support of alternative hypothesis
# If P-value is smaller or equal than alpha(0.05), then we have enough evidence to reject null hypothesis in support of alternative hypothesis.

# Testing Normality for Males
qqnorm(PWD[PWD$Sex == 1,]$ADWG0021, main='ADWG0021 of Males')
qqline(PWD[PWD$Sex == 1,]$ADWG0021, col = "red")

# Plotting Histogram
hist(PWD[PWD$Sex == 1,]$ADWG0021, main='ADWG0021 of Males',col="green")

# Checking Normality with using Shapiro Test
shapiro.test(PWD[PWD$Sex == 1,]$ADWG0021)
# We will find that our p-value = 0.9771 which is bigger than the significance level alpha(0.05)
# Then, we don't have enough evidence to reject the null hypothesis in support of alternative hypothesis
# So, we can assume normality (normally distributed)

# Testing Normality for Females
qqnorm(PWD[PWD$Sex == 2,]$ADWG0021, main='ADWG0021 of Females')
qqline(PWD[PWD$Sex == 2,]$ADWG0021, col = "blue")

# Plotting Histogram
hist(PWD[PWD$Sex == 2,]$ADWG0021, main='ADWG0021 of Females', col = "red")

# Checking Normality with using Shapiro Test
shapiro.test(PWD[PWD$Sex == 2,]$ADWG0021)
# We will find that our p-value = 0.9513 which is bigger than the significance level alpha(0.05).
# Then, we don't have enough evidence to reject the null hypothesis in support of alternative hypothesis.
# So, we assume normality (The Data of Males and Females is normally distributed)
var.test(ADWG0021~Sex,data=PWD)
#since p-value = 0.4193, therefore homoscedastic
# We will use a t‐test
# Because Paired tests are used when two measurements are taken from the same individual (say, person).
# So here, we are comparing the ADWG0021 between Males and Females 
# Furthermore, The paired t-test allows us to compare the mean difference between the two measurements before and after to determine whether the difference is statistically significant or not.

#check if that ADWG0021is different between male or females using t-test
t.test(ADWG0021~Sex, data=PWD, var.equal = TRUE)
# So here, we will find that after using t-test there is a difference in means between Male and Female in ADWG0021
# P-value = 0.7557 which is greater than the significance level alpha (0.05) , So we don't have enough evidence to reject null hypothesis in support of alternative hypothesis


#Q3
#null>> there's no difference in variance
#Alternative>> there's difference in variance
subdata=PWD[PWD$Treatment=="A"|PWD$Treatment=="B",]
t.test(ADWG0021~Treatment, data=subdata)
#p-value = 0.02783<.05 >>  ADWG0021is “different” in the group receiving Treatment A 
shapiro.test(PWD[PWD$Treatment == 'A',]$ADWG0021)
# p-value = 0.0395, NOT NORMAL
shapiro.test(PWD[PWD$Treatment == 'B',]$ADWG0021)
# p-value = 0.8132, NORMAL
#NOT NORMAL
var.test(ADWG0021~Treatment,data=subdata)
#p-value = 0.955, homoscedastic
#Q4
#null>> there's no difference in variance
#Alternative>> there's difference in variance
library(report)

AnovaModel=aov(ADWG0021~Treatment, data = PWD)
summary(AnovaModel)
report(AnovaModel)
coef(AnovaModel)
#p = 0.101, accept null
TukeyHSD(anova)
#normality
shapiro.test(PWD[PWD$Treatment == "A",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "C",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "D",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "E",]$ADWG0021)
#homo
library(car)
leveneTest(ADWG0021~Treatment, data = PWD)
#P-value>.05
#we don't have enough evidence to reject null, so treatment is equal variance>> data is homoscedasticity

############### Q7. Linear Model ###############


# one hot encoding
library(caret)
dmy <- dummyVars(" ~ .", data = PWD, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = PWD))



# Treatment
simpleRegression2 = lm(dat_transformed$ADWG0021 ~ dat_transformed$Treatment.B+
                         dat_transformed$Treatment.C+dat_transformed$Treatment.D+
                         dat_transformed$Treatment.E, data = dat_transformed)
summary(simpleRegression2)

confint(simpleRegression2, level = 0.95)

# Sex
simpleRegression4 = lm(dat_transformed$ADWG0021 ~ dat_transformed$Sex, data = dat_transformed)
summary(simpleRegression4)

confint(simpleRegression4, level = 0.95)
# intercept of 145.992, slope of -1.920, RSE = 19.37, R2 = 0.002579,
# f-stat 0.09824, pv = 0.7557
# model clearly ineffective due to lack if proper linear relationship
# 95% interval -14.31805 - 10.47877 for sex regressor means that
# on average Intercept + (-14.31805 - 10.47877) contains true values
# average change per unit for Rsex is -1.920, do further demonstrate:
nd = data.frame(Sex = c(1, 2))
predict(simpleRegression4, newdata = nd)
# constant prediction when male: 144.0724, prediction when female: 142.1528
# 144.072 - 142.1528 = 1.9196
