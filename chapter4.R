# Loading the Boston data from the MASS package
library(MASS); library(corrplot); library(dplyr); library(ggplot2); library(GGally)
data("Boston")

# The structure and the dimensions of the data
str(Boston)
dim(Boston)
### This data includes 506 observations and 14 variables which are: crim, zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat and medv.
### You can find the descriptions of the variables [here](https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html)

# A graphical overview of the data
### Drawing bar plots of the distributions of these variables isn´t very useful so compair them with pairs and corrplot
## Pairs
pairs(Boston)
### As you can see the connections between these variables vary; some connections are linear (t.ex. between age and dis) and some are non-linear (t.ex between lstat and medv) and some don´t seem to be connected at all (t.ex. between black and rm )
## Correlation plot
### An easier way to look at the correlations is correlation plot
corrplot(cor_matrix, method="circle", type = "upper", cl.pos = "b", tl.pos = "d", tl.cex = 0.6)
### As you can see here, the colors and shades describe the direction and the strength of the correlation between variables - the stronger the shade the stronger the correlation.
### There seems to be very strong positive correlation t.ex. between rad and tax and strong negative correlation t.ex. between age and dis.  On the other hand there seems to be very weak negative correlation between t.ex. ptratio and black, zn and crim and rm and rad.
## Correlation matrix
### From correlation matrix we can see correlation coefficients between these variables. 
cor_matrix<-cor(Boston) 
cor_matrix %>% round(digits = 2)
### As you can see t.ex. the strong positive correlation between rad and tax, which we saw earlier in the correlation plot, has coefficient r=.91 and weak negative correlation between ptratio and black has coefficient r=-.18.
# Standardizing the dataset
### I´ll standardize (scaled(x)=(x−mean(x))/sd(x)) the Boston dataset using scale() function because linear discrimination analysis is based on assumptions that variables are normally distributed and their variances are same.
boston_scaled <- scale(Boston)
summary(Boston)
summary(boston_scaled)
### If we compare the summaries of Boston and boston_scaled we can see that the mean of every variable changed to 0 (so the variables have also negative values now) and also the range is much smaller.

# Creating a categorial variable
### At first I´ll convert the boston_scaled to a data frame format.
class(boston_scaled)
boston_scaled <- as.data.frame(boston_scaled)
### Then I´ll save the scaled crime rate
scaled_crim <- boston_scaled$crim
### After that I`ll use the quantiles as the break points in the categorical variable and create a categorial variable "crime"
bins <- quantile(scaled_crim)
crime <- cut(scaled_crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))
### I´ll remove original crim from the dataset and add the "crime" to scaled data
boston_scaled <- dplyr::select(boston_scaled, -crim)
boston_scaled <- data.frame(boston_scaled, crime)
## Dividing the dataset to train and test sets
### Now I´ll divide the dataset to train and test sets, so that 80% of the data belongs to the train set
n <- nrow(boston_scaled)
ind <- sample(n,  size = n * 0.8)
train <- boston_scaled[ind,]
test <- boston_scaled[-ind,]

# Fitting the linear discriminant analysis on the train set
### Now I´ll fit a linear discriminant analysis with the function lda() and use the "crime" as target variable and other variables as predictors.
lda.fit <- lda(crime ~ ., data = train)
## The LDA biplot
### Here is the LDA biplot of this analysis
classes <- as.numeric(train$crime)
plot(lda.fit, dimen = 2, col = classes, pch = classes)
# Saving the crime categories and removing the categorical crime variable
### To save the crime categories from the test set I´ll create "correct_classes"
correct_classes <- test$crime
### After that I´ll remove the categorical crime variable from the test dataset.
test <- dplyr::select(test, -crime)

# Predicting the classes with the LDA model
### Next I will predict the classes with the LDA model on the test data using function predict()
lda.pred <- predict(lda.fit, newdata = test)
### Then I´ll cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)
### So as you can see the predicted variables are columns and correct values are rows. If we look at first at the row where the correct value is low we can see that the most of the predicted values are in the class low (n=16) but there are also many values in the class med_low(12) and even some in the med_high (n=2) class. We can see same with the correct med_low class: most of the predicted values are on the med_low class (n=15) but almost as many are in the med_high group (n=14) and some in the low group (n=3). The situation with correct med_high and high classes is much better. In the correct med_high class there are 18 values in the predicted med_high class and only 2 in high class. In correct high class every value (n=20) is in the predicted high class.

# Distance measures
### At first I´ll reload the Boston data
data("Boston")
### After that I´ll scale the variables to get comparable distances as I did earlier in this exercise
boston_scaled <- scale(Boston)
### Then I´ll calculate the distances between the observations by creating an euclidean distance matrix
dist_eu <- dist(Boston)

# K-means
### Now I´ll first run the k-means algorithm where the number of clusters is 10
km <-kmeans(dist_eu, centers = 10)
pairs(Boston, col = km$cluster)
### The plot seems to be quite messy so maybe there are too many clusters
    
### To investigate the optimal number of clusters I will calculate the total within sum of squares
set.seed(123)
k_max <- 10
twcss <- sapply(1:k_max, function(k){kmeans(dist_eu, k)$tot.withinss})
plot(1:k_max, twcss, type='b')
### If we look at the plot we can see that the TWCSS drops radically when the number of clusters is two so that might be the optimal number of them.
    
### I will run the k-means algorithm again
km <-kmeans(dist_eu, centers = 2)

### Now if we draw the plot again we can see that they are much easier to interpret
pairs(Boston, col = km$cluster)
### As we can see in many cases (t.ex. between age and indus) the red cluster seems to be quite random but the black cluster seems to have some clear pattern.