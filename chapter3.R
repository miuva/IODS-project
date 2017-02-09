# Week 3: Logistic regression

library(ggplot2); library(dplyr); library(GGally); library(boot)

## At first I´ll read the alcohol data into R    
getwd()
setwd("/Users/mirva/IODS-project/data")
alc = read.csv("alcohol")

# The structure of the data
str(alc)

# This data includes 382 observations and 36 variables
# The data are from two identical questionaires related to secondary school student alcohol comsumption in Portugal. 
# Database info:
# Using Data Mining To Predict Secondary School Student Alcohol Consumption. 
# Fabio Pagnotta, Hossain Mohammad Amran. 
# Department of Computer Science,University of Camerino
# You can find more info about the variables and data here: https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION

# I will study the relationship between high/low alcohol consumption and four other variables:
# Pstatus, Medu, Fedu and famrel
# My personal hypothesis about these four variables and alcohol consumption are as follows:
# Pstatus = parents´ cohabitation status (living together or apart): 
# My hypothesis is that students whose parents are living together could have lower risk of high alcohol consumption.
# Medu and Fedu = mother's education and father´s education:
# My hypothesis is that parents´lower education (and therefore possibly lower socio economical status) could
# increase the risk of high alcohol consumption.
# famrel = quality of family relationships
# My hypothesis here is that students whose family relationships are graded 4 or 5 could have lower risk of
# high alcohol consumption.

alc %>% group_by(Pstatus, high_use) %>% summarise(count = n())
# Within students whose parents´cohabilitation status is A = "apart" 26 (68%) students´ alcohol usage is
# low and 12 (32%) it is high. Within students whose parents live together (status = T) 242 (70%) students´
# alcohol usage is low and 102 (30%) it is high.

alc %>% group_by(Medu, high_use) %>% summarise(count = n())
g1me <- ggplot(alc, aes(x = high_use, y = Medu))
g1me + geom_boxplot() + xlab("High alcohol consumption") + ylab("Mothers education")

# If we look at the mothers education level first the results of summary are as follofs:
# no education: low drinking n=1 (33%) and high drinking n=2 (67%)
# primary education (4th grade): low drinking n=33 (65%) and high drinking n= 18 (35%)
# 5th to 9th grade: low drinking n=80 (82%) and high drinking n=18 (18%)
# secondary education: low drinking n=59 (62%) and high drinking n=36 (38%)
# higher education: low drinking n=95 (70%) and high drinking n=40 (30%)
# so there might be some differences between these groups but ns are quite low, especially in non-educated-group.
# The boxplots of these two alcohol consumption groups look exactly the same.

alc %>% group_by(Fedu, high_use) %>% summarise(count = n())
g1fe <- ggplot(alc, aes(x = high_use, y = Fedu))
g1fe + geom_boxplot() + xlab("High alcohol consumption") + ylab("Fathers education")
# With fathers´education the same numbers are as follows:
# no education: low drinking n=2 (100%) and high drinking n=0
# primary education (4th grade): low drinking n=53 (69%) and high drinking n= 24 (31%)
# 5th to 9th grade: low drinking n=75 (71%) and high drinking n=30 (29%)
# secondary education: low drinking n=72 (73%) and high drinking n=27 (27%)
# higher education: low drinking n=66 (67%) and high drinking n=33 (33%)
# So also here we can see that fathers´higher education doesn´t possibly imply lower drinking percentages
# as I suggested in my hypothesis.

alc %>% group_by(famrel, high_use) %>% summarise(count = n())
g1fr <- ggplot(alc, aes(x = high_use, y = Fedu))
g1fr + geom_boxplot() + xlab("High alcohol consumption") + ylab("Quality of family relations")
# If we look at the connection between alcohol consumption and quality of family relations amounts 
# in different grade groups are
# grade 1: low drinkers 6 (75%), high drinkers 2 (25%)
# grade 2: low drinkers 10 (53%), high drinkers 9 (47%)
# grade 3: low drinkers 39 (61%), high drinkers 25 (39%)
# grade 4: low drinkers 135 (71%), high drinkers 54 (29%)
# grade 5: low drinkers 78 (76%), high drinkers 24 (24%)
# So there might be some support for my hypothesis that better the family relations lower the alcohol consumption

# Logistic regression and the relationship between the chosen variables and high/low alcohol consumption
m <- glm(high_use ~ Pstatus + Medu + Fedu + famrel, data = alc, family = "binomial")
summary.glm(m)
coef(m)
# Only the variable quality of family relations is significant in this model.

# Odds ratios and confidence intervals
OR <- coef(m) %>% exp
CI <- confint(m) %>% exp
cbind(OR, CI)
# Every one of these confidence intervals includes 1 except famrel so it implies these other variables make 
# no difference in the model.

# Removing non-significant variables one by one doesn´t give any new variables to the model
m2 <- glm(high_use ~ Medu + Fedu + famrel, data = alc, family = "binomial")
summary.glm(m2)

m3 <- glm(high_use ~ Fedu + famrel, data = alc, family = "binomial")
summary.glm(m3)

# So I´ll leave just the variable famrel to the model
m4 <- glm(high_use ~ famrel, data = alc, family = "binomial")
summary.glm(m4)

# Using the variables which, according to your logistic regression model, had a statistical relationship with 
# high/low alcohol consumption, explore the predictive power of you model. Provide a 2x2 cross tabulation of 
# predictions versus the actual values and optionally display a graphic visualizing both the actual values and 
# the predictions. Compute the total proportion of inaccurately classified individuals (= the training error) 
# and comment on all the results. Compare the performance of the model with performance achieved by some simple 
# guessing strategy.

# The predictive power of my model
probabilities <- predict(m4, type = "response")
alc <- mutate(alc, probability = probabilities)
alc <- mutate(alc, prediction = probability > 0.5)
table(high_use = alc$high_use, prediction = alc$prediction)
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))
g2 <- g + geom_point()
g2
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table() %>% addmargins()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# So this means that the predictive power of my model is low (and probably more significant variables should be
# found to make it more predictive.) As long as all the predictions are under 0.5 the level of prediction is
# same with guessing.

loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}
loss_func(class = alc$high_use, prob = alc$probability)
# Mean prediction error is approx. 0.298 whixh means that so many of predictions are false.

# 10-fold cross validation
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m4, K = 10)
cv$delta[1]
# The average number of wrong predictions in the cross validation is approx. 0.304

# Another model where prediction error is smaller compared to the model introduced in DataCamp has variables:
# absences, sex, famrel and goout. It´s prediction error is approx. 0.204
m5 <- glm(high_use ~ absences + sex + famrel + goout, data = alc, family = "binomial")
summary.glm(m5)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m5, K = 10)
cv$delta[1]
