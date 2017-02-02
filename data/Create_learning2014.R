# PT 1
# Mirva Turkia 31.1.2017
# This is the script file for the data wrangling exercise

# PT 2
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", 
               sep="\t", header=TRUE)
lrn14
str(lrn14)
dim(lrn14)
# structure: 183 observations, 60 variables
library(dplyr)

# PT 3

# Variables:
# Gender: Male = 1  Female = 2
# Age: Age (in years) derived from the date of birth
# Attitude: Global attitude toward statistics ~Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj
# Deep:
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22",
                    "D30","D06",  "D15", "D23", "D31")
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)
# Surface:
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21",
                       "SU29","SU08","SU16","SU24","SU32")
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)
# Strategic
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)
# Points   Yhteispisteet (max kaikista)

# Create an analysis dataset
keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")
learning2014 <- select(lrn14, one_of(keep_columns))
str(learning2014)

# Change column names of "age", "attitude" and "points"
colnames(learning2014)
colnames(learning2014)[2] <- "age"
colnames(learning2014)[3] <- "attitude"
colnames(learning2014)[7] <- "points"
colnames(learning2014)

# Exclude observations where the exam points variable is zero
learning2014 <- filter(learning2014, points > 0)
dim(learning2014)

# PT4
# Set the working directory of R session the iods project folder
getwd()
setwd("/Users/mirva/IODS-project")

# Save the analysis dataset to the ‘data’ folder
?write.csv
write.csv(learning2014, file = "learning2014")

# Demonstrate that you can also read the data again
read.csv("learning2014")

str(learning2014)
