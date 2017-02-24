# Week 5: Dimensionality reduction techniques

### This weeks data is part of the UN Development Programme, Human Development Reports, which you can find [here](http://hdr.undp.org/en/content/human-development-index-hdi)
### The data is combined from two different data sets: Human Development Index and its components and Gender Inequality Index

### At first I will load the ‘human’ data and then explore it´s structure and dimensions.

## Loading the data
setwd("/Users/mirva/IODS-project/data")
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human2.txt", sep=",", header=TRUE)

## Structure and dimensions
str(human)
dim(human)

### As you can see there are 155 observations and 8 variables in this data. The variables are as follows:
### * Edu2.FM =  The ratio of female and male populations with secondary education in each country (female/ male)
### * Labo.FM = The ratio of labour force participation of females and males in each country (female/male)
### * Life.Exp = Life expectancy at birth (years)
### * Edu.Exp = Expected years of schooling 
### * GNI = Gross national income (GNI) per capita
### * Mat.Mor = Maternal mortality ratio (deaths per 100 000 live births)
### * Ado.Birth = Adolescent birth rate (births per 1 000 women ages 15–19)
### * Parli.F = Share of seats in parliament (% held by women)

### Now that you know what the variables used here are let´s look at the graphics and summaries of the data.

## The graphical overview of the data
library(GGally); library(dplyr); library(corrplot)
ggpairs(human)
cor(human) %>% corrplot()

### As you can see from the correlation plots above some correlations are quite strong and some are very minimal. Strongest correlations we can find are between Mat.Mor and Life.Exp (-.857), Edu.Exp and Life.Exp (r=.789), Mat.Mor and Ado.Birth (r=.759), Life.Exp and GNI (r=.627) and Edu.Exp and GNI (r=.624). Weakest correlations are between Edu2.FM and Labo.FM (r=.00956), Labo.FM and GNI (r=-.0217), Labo.FM and Edu.Exp (r=.0473), Ado.Birth and Parli.F (r=-.0709) and Parli.F and Edu2.FM (r=.0786).

## Summaries of the variables in the data 
summary(human)

### As we can see from the summaries and also from the upper of the correlation plots above the median of the Edu2.FM is around 0.9375 so the ratio of female and male populations with secondary education is almost 1 meaning almost equal ratio of education. But in some country the minimum is 0.1717 meaning that the women get 2nd education rarely compared to men. The maximum here is 1.4967 meaning that in some countries women are more educated than men.
### If we look at the summary of the Labo.FM median is 0.7535 so the men are in labour force more often than women. In many countries women are at home taking care of household and it might explain something of this result. It is interesting that the maximum here is only 1.0380 which tells that women in average don´t work more often than men.
### Edu.Exp seems to be quite normally distributed and the median here is 13.50 (expected years of schooling). The 1st and 3rd quartiles are quite near this but the minimum is only 5.40 telling that some of people won´t probably go to school at all or just for few years. Maximum 20.20 years is also quite high meaning that in some countries almost everyone gets at least 2nd education and even higher.
### The median of Life.Exp is 74.20 years. Quite shockingly the minimum here is only 49 years which might be a consequense for example of high child mortality. The maximum is 83.50 years so in some country the life expectancy is almost two times larger than in the country where it is 49 years.
### As we can see in GNI the range is huge. The minimum is only 581$ and the maximum 123 124$ while the median is 12 040$. We can see from the graph also that GNI is far from uniform (or even normal) distribution and therefore is quite inequal.
### Almost the same distribution is true with the Mat.Mor but here the minimum is 1 death, median 49 deaths and maximum 1100 deaths and 50% is between 11.5 and 190 deaths. So this means that in some country there is only 1 death per 100 000 life births (0.001%) and in some country 1100 deaths (1.1%) so the differences are very big but in most countries the death rate is quite low.
### The median of the Ado.Birth is surplisingly high, 33.60 which is 3.36% of womean ages 15-19. Also the range is quite big; minimum is 0.60 and maximum 204.80.
### Parli.F tells much of the equality between the genders. The minimum here is 0 meaning that in some country there are no women in parliament. The maximum is 57.50 so in some country there are more women in parliament than men. The median is 19.30 so in most countries women are under represented in parliaments compared to men.

# Principal component analysis (PCA)

### Because there are 8 variables it would be hard to interpret the relations between them if we could use only pairwise comparisons. That´s why I will use principal component analysis (PCA) which reduces variables to combinations of the data. I will perform the PCA first on the non standardized human data and then on the standardized human data

## PCA on non standardized
pca1_human <- prcomp(human)
s1 <- summary(pca1_human)
s1
pca1_pr <- round(100*s1$importance[2,], digits = 1) 
pca1_pr
pc1_lab <- paste0(names(pca1_pr), " (", pca1_pr, "%)")
biplot(pca1_human, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc1_lab[1], ylab = pc1_lab[2])

## PCA on standardized
human_std <- scale(human)
summary(human_std)
pca2_human <- prcomp(human_std)
s2 <- summary(pca2_human)
s2
pca2_pr <- round(100*s2$importance[2,], digits = 1) 
pca2_pr
pc2_lab <- paste0(names(pca2_pr), " (", pca2_pr, "%)")
biplot(pca2_human, cex = c(0.8, 0.7), col = c("grey40", "deeppink2"), xlab = pc2_lab[1], ylab = pc2_lab[2])

### As you can see the plots created are very different. If we look at first at the PCA on non standardized data we can see that the first component PC1 explains 99,99% of the variation and PC2 0,01% so as we can see from the cumulative proportion together they explain 100% of the variation. Because there are different units of measure in the data variances are quite different and the PCA is really hard to interpret on non standardized data.
### If we then look at the PCA on standardized data we can see that there are more components with some proportions and 100% is acquired not until PC8. PC1 and PC2 together explain 69,84% of the variaton. Also the plot is now easier to interpret. If we look at the arrows we can see that there are quite strong correlations between Edu.Exp, Life.Exp, Edu2.FM and GNI and also between Mat.Mor and Ado.Birth as we already noticed earlier. These 6 variables also correlate strongly with PC1. Parli.F and Labo.FM correlate most with each other and PC2. We can also see that for example in Mozambique and Burundi the ratio of labour force participation of females and males in each country and share of seats in parliament are quite high and also the maternal mortality ratio and adolescent birth ratio. In Mozambique and Burundi the ratio of female and male populations with secondary education, expected years of schooling, life expectancy at birth and gross national income (GNI) per capita are quite low. For example Iran has quite opposite pattern.

## Tea!

### At first I will load the tea dataset from the FactoMineR package

library(FactoMineR); library(tidyr); library(ggplot2)
data("tea")

str(tea)
dim(tea)

### There are 300 observations and 36 variables in this data

### I choose to keep the following columns: tea.time, tearoom, Tea, How, sugar, how and where
keep_columns <- c("tea.time", "tearoom", "Tea", "How", "sugar", "how", "where")
tea_time <- dplyr::select(tea, one_of(keep_columns))
summary(tea_time)
str(tea_time)

### As you can see from the summary there are the following options in the chosen variables:
### * tea.time: not tea time or tea time
### * tearoom: not in a tearoom or in a tearoom
### * Tea: black, earl grey or green
### * How: alone, with lemon, milk or other
### * sugar: no sugar or with sugar
### * how: with a tea bag, with a tea bag and loose leaf (unpackaged) or only loose leaf
### * where: from a chain store, from a chain store and a tea shop or only from a tea shop

gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
### From the barplots we can see that most of the people have tea time, don´t drink at tearoom, drink earl grey, drink tea alone without any lemon or milk, use sugar, use only tea bags and buy their tea from a chain store.

## Multiple Correspondence Analysis

### I will use Multiple Correspondence Analysis (MCA) to inspect the connections between the variables I chose
mca <- MCA(tea_time, graph = FALSE)
summary(mca)

### Here we can see that the dimensions 1 and 2 cover 29,915% of the variance. From the last table we can see how well different variables correlate with the first three dimensions.
plot(mca, invisible=c("ind"), habillage = "quali")

### From the plot above we can see for example that buying unpackaged tea is closely related to buying tea from a tea shop. Also buying only tea bags is closely related to buying tea from a chain store. Drinking earl grey and drinking tea with lemon are closely related and also drinking black tea and drinking tea with milk.
